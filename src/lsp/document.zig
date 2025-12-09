const std = @import("std");
const Allocator = std.mem.Allocator;
const types = @import("types.zig");
const workspace_mod = @import("workspace.zig");
const symbols = @import("symbols.zig");
const diag = @import("diagnostics.zig");
const runic = @import("runic");
const parseFile = @import("parser.zig").parseFile;
const DocumentStore = runic.DocumentStore;

pub const LspDocumentStore = struct {
    allocator: Allocator,
    map: std.StringHashMap(*Document),
    workspace: *workspace_mod.Workspace,

    pub fn init(allocator: Allocator, workspace: *workspace_mod.Workspace) LspDocumentStore {
        return .{
            .allocator = allocator,
            .map = .init(allocator),
            .workspace = workspace,
        };
    }

    pub fn deinit(self: *LspDocumentStore) void {
        var it = self.map.iterator();
        while (it.next()) |entry| {
            self.allocator.free(@constCast(entry.key_ptr.*));
            entry.value_ptr.*.deinit(self.allocator);
        }
        self.map.deinit();
    }

    pub fn documentStore(self: *LspDocumentStore) DocumentStore {
        return .{
            .ptr = self,
            .vtable = vtable,
        };
    }

    const vtable = &DocumentStore.VTable{
        .getSource = LspDocumentStore.getSource,
        .getAst = LspDocumentStore.getAst,
        .putAst = LspDocumentStore.putAst,
        .getParser = LspDocumentStore.getParser,
    };

    pub fn getDocument(ptr: *anyopaque, path: []const u8) DocumentStore.Error!*Document {
        const ctx: *LspDocumentStore = @ptrCast(@alignCast(ptr));
        const uri = ctx.resolveUri(path) catch return DocumentStore.Error.GetFailed;
        defer ctx.allocator.free(uri);
        return ctx.requestDocument(
            uri,
            path,
            .open_and_parse_only,
        ) catch |err| switch (err) {
            DocumentStore.Error.DocumentNotFound => return DocumentStore.Error.DocumentNotFound,
            else => DocumentStore.Error.GetFailed,
        };
        // return ctx.get(uri) orelse return DocumentStore.Error.DocumentNotFound;
    }

    pub fn getSource(ptr: *anyopaque, path: []const u8) DocumentStore.Error![]const u8 {
        const document = try getDocument(ptr, path);
        return document.text;
    }

    pub fn getAst(ptr: *anyopaque, path: []const u8) DocumentStore.Error!?runic.ast.Script {
        const document = try getDocument(ptr, path);
        return document.ast;
    }

    pub fn putAst(
        ptr: *anyopaque,
        path: []const u8,
        script: runic.ast.Script,
    ) DocumentStore.Error!void {
        const document = try getDocument(ptr, path);
        document.ast = script;
    }

    pub fn getParser(
        ptr: *anyopaque,
        path: []const u8,
    ) DocumentStore.Error!*runic.parser.Parser {
        const document = try getDocument(ptr, path);
        return if (document.parser) |*parser| parser else unreachable;
    }

    pub fn openOrReplace(
        self: *LspDocumentStore,
        uri: []const u8,
        path: []const u8,
        text: []const u8,
        version: types.DocumentVersion,
        options: ProcessDocumentOptions,
    ) !void {
        if (self.map.fetchRemove(uri)) |removed| {
            self.allocator.free(@constCast(removed.key));
            var removed_copy = removed.value;
            removed_copy.deinit(self.allocator);
        }

        const document = try self.createAndStoreDocument(uri, path, text, version, .client);
        try self.processDocument(document, options);
    }

    fn createAndStoreDocument(
        self: *LspDocumentStore,
        uri: []const u8,
        path: []const u8,
        text: []const u8,
        version: types.DocumentVersion,
        manage_mode: Document.ManageMode,
    ) !*Document {
        const key = try self.allocator.dupe(u8, uri);
        errdefer self.allocator.free(key);

        const document = try self.allocator.create(Document);
        document.* = try Document.init(self.allocator, path, text, version, manage_mode);
        errdefer {
            document.deinit(self.allocator);
            self.allocator.destroy(document);
        }

        const entry = try self.map.getOrPut(key);
        entry.value_ptr.* = document;

        return document;
    }

    fn processDocument(
        self: *LspDocumentStore,
        document: *Document,
        options: ProcessDocumentOptions,
    ) !void {
        if (options.shouldRebuildSymbols()) {
            try document.rebuildSymbols(
                self.allocator,
                self.documentStore(),
                self.workspace.describePath(document.path),
            );
        }

        if (options.shouldTypeCheck()) {
            try document.reportTypeChecker(self.allocator, self.workspace);
        }
    }

    pub const ProcessDocumentOptions = union(enum) {
        open_and_parse_only,
        parse_and_type_check,

        pub fn shouldRebuildSymbols(_: ProcessDocumentOptions) bool {
            return true;
        }

        pub fn shouldTypeCheck(self: ProcessDocumentOptions) bool {
            return switch (self) {
                .open_and_parse_only => false,
                .parse_and_type_check => true,
            };
        }
    };

    pub fn requestDocument(
        self: *LspDocumentStore,
        uri: []const u8,
        path: []const u8,
        options: ProcessDocumentOptions,
    ) !*Document {
        if (self.get(uri)) |document| return document;

        const contents = try self.readDocumentFile(path);
        defer self.allocator.free(contents);

        const document = try self.createAndStoreDocument(uri, path, contents, 0, .server);
        try self.processDocument(document, options);

        return document;
    }

    fn readDocumentFile(self: *LspDocumentStore, path: []const u8) ![]const u8 {
        const file = std.fs.cwd().openFile(path, .{}) catch |err| switch (err) {
            error.FileNotFound => return DocumentStore.Error.DocumentNotFound,
            else => return err,
        };
        defer file.close();
        var file_buffer: [512]u8 = undefined;
        var file_reader = file.reader(&file_buffer);
        const reader = &file_reader.interface;

        var alloc_writer = std.Io.Writer.Allocating.init(self.allocator);
        defer alloc_writer.deinit();
        const writer = &alloc_writer.writer;

        while (true) {
            _ = reader.stream(writer, .unlimited) catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };
        }

        return try alloc_writer.toOwnedSlice();
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
        self: *LspDocumentStore,
        uri: []const u8,
        events: []const types.TextDocumentContentChangeEvent,
        version: types.DocumentVersion,
        workspace: *workspace_mod.Workspace,
    ) !bool {
        const doc = self.get(uri) orelse return false;
        doc.manage_mode = .client;

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
            self.documentStore(),
            workspace.describePath(doc.path),
        );
        try doc.reportTypeChecker(self.allocator, workspace);
        return true;
    }

    pub fn close(self: *LspDocumentStore, uri: []const u8) void {
        const document = self.get(uri) orelse return;
        document.manage_mode = .server;
        document.version = 0;

        // if (self.map.fetchRemove(uri)) |removed| {
        //     self.allocator.free(@constCast(removed.key));
        //     var document = removed.value;
        //     document.deinit(self.allocator);
        // }
    }

    pub fn resolveUri(self: LspDocumentStore, path: []const u8) ![]const u8 {
        const absolutePath = if (std.fs.path.isAbsolute(path))
            try self.allocator.dupe(u8, path)
        else
            try std.fs.cwd().realpathAlloc(self.allocator, path);
        defer self.allocator.free(absolutePath);
        return std.fmt.allocPrint(self.allocator, "file://{s}", .{absolutePath});
    }

    pub fn get(self: *LspDocumentStore, uri: []const u8) ?*Document {
        return self.map.get(uri);
    }
};

const Document = struct {
    path: []const u8,
    text: []u8,
    version: types.DocumentVersion,
    symbols: std.ArrayList(symbols.Symbol) = .empty,
    diagnostics: std.ArrayList(diag.Diagnostic) = .empty,
    ast: ?runic.ast.Script = null,
    parser: ?runic.parser.Parser = null,
    manage_mode: ManageMode,
    shouldSendDiagnostics: bool = false,

    pub const ManageMode = enum { client, server };

    fn init(
        allocator: Allocator,
        path: []const u8,
        text: []const u8,
        version: types.DocumentVersion,
        manage_mode: ManageMode,
    ) !Document {
        return .{
            .path = try allocator.dupe(u8, path),
            .text = try allocator.dupe(u8, text),
            .version = version,
            .manage_mode = manage_mode,
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
        document_store: DocumentStore,
        detail: []const u8,
    ) !void {
        self.shouldSendDiagnostics = true;

        for (self.symbols.items) |*entry| entry.deinit(allocator);
        self.symbols.clearRetainingCapacity();
        self.clearDiagnostics(allocator);

        if (self.parser) |*p| p.deinit();

        self.ast = null;
        self.parser = runic.parser.Parser.init(allocator, document_store);

        const script = try parseFile(allocator, &self.diagnostics, &self.parser.?, self.path) orelse return;
        try symbols.collectSymbols(allocator, detail, script, &self.symbols);
    }

    fn reportTypeChecker(
        self: *Document,
        allocator: Allocator,
        workspace: *workspace_mod.Workspace,
    ) !void {
        var type_checker = runic.semantic.TypeChecker.init(
            allocator,
            workspace.documents.documentStore(),
        );
        defer type_checker.deinit();
        const result = type_checker.typeCheck(self.path) catch |err| switch (err) {
            error.DocumentNotParsed => return,
            else => {
                std.log.err("Type checker failed to run: {}", .{err});
                return;
            },
        };

        switch (result) {
            .success => {},
            .err => |diagnostics| {
                for (diagnostics) |d| {
                    const diag_uri = try workspace.documents.resolveUri(d.span().start.file);
                    const diag_doc = workspace.documents.get(diag_uri).?;

                    try diag_doc.diagnostics.append(allocator, .{
                        .uri = diag_uri,
                        .message = try allocator.dupe(u8, d.message),
                        .span = d.span(),
                        .severity = std.meta.stringToEnum(
                            types.DiagnosticSeverity,
                            @tagName(d.severity),
                        ).?,
                    });
                }
            },
        }
    }
};
