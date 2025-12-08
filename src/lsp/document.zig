const std = @import("std");
const Allocator = std.mem.Allocator;
const types = @import("types.zig");
const workspace_mod = @import("workspace.zig");
const symbols = @import("symbols.zig");
const diag = @import("diagnostics.zig");
const runic = @import("runic");
const Parser = @import("parser.zig").Parser;
const parseFile = @import("parser.zig").parseFile;

pub const DocumentStore = struct {
    allocator: Allocator,
    map: std.StringHashMap(Document),

    pub fn init(allocator: Allocator) DocumentStore {
        return .{ .allocator = allocator, .map = .init(allocator) };
    }

    pub fn deinit(self: *DocumentStore) void {
        var it = self.map.iterator();
        while (it.next()) |entry| {
            self.allocator.free(@constCast(entry.key_ptr.*));
            entry.value_ptr.deinit(self.allocator);
        }
        self.map.deinit();
    }

    pub fn openOrReplace(
        self: *DocumentStore,
        uri: []const u8,
        path: []u8,
        text: []const u8,
        version: types.DocumentVersion,
        workspace: *workspace_mod.Workspace,
    ) !void {
        if (self.map.fetchRemove(uri)) |removed| {
            self.allocator.free(@constCast(removed.key));
            var removed_copy = removed.value;
            removed_copy.deinit(self.allocator);
        }
        const key = try self.allocator.dupe(u8, uri);
        errdefer self.allocator.free(key);
        var document = try Document.init(self.allocator, path, text, version);
        errdefer document.deinit(self.allocator);
        const entry = try self.map.getOrPut(key);
        entry.value_ptr.* = document;
        try entry.value_ptr.rebuildSymbols(
            self.allocator,
            workspace,
            workspace.describePath(entry.value_ptr.path),
        );
        try entry.value_ptr.reportTypeChecker(self.allocator);
    }

    fn getLine(line: u32, source: []const u8) ?[]const u8 {
        const index = types.Position.findIndex(.{
            .character = 0,
            .line = line,
        }, source) orelse return null;

        const end = std.mem.indexOfScalarPos(u8, source, index, '\n');

        return if (end) |e| source[index..e] else source[index..];
    }

    pub fn update(
        self: *DocumentStore,
        uri: []const u8,
        events: []const types.TextDocumentContentChangeEvent,
        version: types.DocumentVersion,
        workspace: *workspace_mod.Workspace,
    ) !bool {
        const doc = self.map.getPtr(uri) orelse return false;

        var text: []const u8 = try self.allocator.dupe(u8, doc.text);

        for (events) |event| switch (event) {
            .text => |t| {
                self.allocator.free(text);
                text = try self.allocator.dupe(u8, t);
            },
            .range => |range_params| {
                const range = range_params.range;
                const startIndex = range.start.findIndex(text) orelse {
                    std.log.err("range not found: {},{} len: {}", .{ range.start.character, range.start.line, text.len });
                    const startLineN = range.start.line -| 2;
                    const endLineN = range.start.line +| 2;
                    const linesLen = endLineN - startLineN;
                    for (0..linesLen) |i| {
                        const lineN = startLineN + i;
                        const line = getLine(@intCast(lineN), text) orelse "<not found>";
                        std.log.err("{}: {s}", .{ lineN, line });
                    }
                    return error.RangeNotFound;
                };
                const rangeLength = range_params.rangeLength orelse return error.RangeLengthNotDefined;
                const endIndex = startIndex + rangeLength;
                const first = text[0..startIndex];
                const second = range_params.text;
                const third = if (endIndex < text.len) text[endIndex..] else "";

                const oldText = text;
                text = try std.fmt.allocPrint(self.allocator, "{s}{s}{s}", .{ first, second, third });
                self.allocator.free(oldText);
            },
        };

        std.log.err("new document text: \n\n{s}", .{text});

        try doc.setText(self.allocator, text, version);
        self.allocator.free(text);
        try doc.rebuildSymbols(
            self.allocator,
            workspace,
            workspace.describePath(doc.path),
        );
        try doc.reportTypeChecker(self.allocator);
        return true;
    }

    pub fn close(self: *DocumentStore, uri: []const u8) void {
        if (self.map.fetchRemove(uri)) |removed| {
            self.allocator.free(@constCast(removed.key));
            var document = removed.value;
            document.deinit(self.allocator);
        }
    }

    pub fn resolveUri(self: DocumentStore, path: []const u8) ![]const u8 {
        const absolutePath = if (std.fs.path.isAbsolute(path))
            try self.allocator.dupe(u8, path)
        else
            try std.fs.cwd().realpathAlloc(self.allocator, path);
        defer self.allocator.free(absolutePath);
        return std.fmt.allocPrint(self.allocator, "file://{s}", .{absolutePath});
    }

    pub fn get(self: *DocumentStore, uri: []const u8) ?*Document {
        return self.map.getPtr(uri);
    }
};

const Document = struct {
    path: []u8,
    text: []u8,
    version: types.DocumentVersion,
    symbols: std.ArrayList(symbols.Symbol) = .empty,
    diagnostics: std.ArrayList(diag.Diagnostic) = .empty,
    ast: ?runic.ast.Script = null,
    parser: ?Parser = null,
    shouldSendDiagnostics: bool = false,

    fn init(
        allocator: Allocator,
        path: []u8,
        text: []const u8,
        version: types.DocumentVersion,
    ) !Document {
        return .{
            .path = path,
            .text = try allocator.dupe(u8, text),
            .version = version,
        };
    }

    fn deinit(self: *Document, allocator: Allocator) void {
        allocator.free(self.path);
        allocator.free(self.text);
        for (self.symbols.items) |*entry| entry.deinit(allocator);
        self.symbols.deinit(allocator);
        if (self.parser) |*p| p.deinit();
        self.diagnostics.deinit(allocator);
        self.* = undefined;
    }

    pub fn uri(self: Document, allocator: Allocator) ![]const u8 {
        return std.fmt.allocPrint(allocator, "file://{s}", .{self.path});
    }

    pub fn clearDiagnostics(self: *Document, allocator: Allocator) void {
        for (self.diagnostics.items) |*entry| entry.deinit(allocator);
        self.diagnostics.clearRetainingCapacity();
    }

    fn setText(
        self: *Document,
        allocator: Allocator,
        text: []const u8,
        version: types.DocumentVersion,
    ) !void {
        allocator.free(self.text);
        self.text = try allocator.dupe(u8, text);
        self.version = version;
    }

    fn rebuildSymbols(
        self: *Document,
        allocator: Allocator,
        workspace: *workspace_mod.Workspace,
        detail: []const u8,
    ) !void {
        self.shouldSendDiagnostics = true;

        for (self.symbols.items) |*entry| entry.deinit(allocator);
        self.symbols.clearRetainingCapacity();
        self.clearDiagnostics(allocator);

        if (self.parser) |*p| p.deinit();

        // var parser = runic.parser.Parser.init(allocator, self.text);
        self.ast = null;
        self.parser = Parser.init(allocator, workspace.documents);
        // defer parser.deinit();
        const script = try parseFile(allocator, &self.diagnostics, &self.parser.?, self.path) orelse return;
        try symbols.collectSymbols(allocator, detail, script, &self.symbols);
    }

    fn reportTypeChecker(
        self: *Document,
        allocator: Allocator,
    ) !void {
        var type_checker = runic.semantic.TypeChecker.init(allocator, &self.ast.?);
        defer type_checker.deinit();
        _ = type_checker.typeCheck() catch |err| {
            const expr_that_errorered = type_checker.expr_that_errorered.?;
            const span = expr_that_errorered.span();

            try self.diagnostics.append(allocator, .{
                .uri = try self.uri(allocator),
                .message = try allocator.dupe(u8, @errorName(err)),
                .span = span,
                .severity = .@"error",
            });
        };
    }
};
