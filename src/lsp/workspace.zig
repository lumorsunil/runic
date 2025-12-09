const std = @import("std");
const symbols = @import("symbols.zig");
const diag = @import("diagnostics.zig");
const runic = @import("runic");
const parseFile = @import("parser.zig").parseFile;
const LspDocumentStore = @import("document.zig").LspDocumentStore;

const max_source_bytes: usize = 4 * 1024 * 1024;

const Allocator = std.mem.Allocator;

fn makeKeyword(allocator: Allocator, name: []const u8, documentation: []const u8) !symbols.Symbol {
    return .{
        .detail = try allocator.dupe(u8, "keyword"),
        .documentation = try allocator.dupe(u8, documentation),
        .name = try allocator.dupe(u8, name),
        .kind = .keyword,
    };
}

fn keywordSymbols(allocator: Allocator) ![]const symbols.Symbol {
    var list = std.ArrayList(symbols.Symbol).empty;

    try list.appendSlice(allocator, &.{
        try makeKeyword(allocator, "import", "# import\nImport a runic module.\n\n```runic\nconst lib = import \"lib.rn\"\n```"),
        try makeKeyword(allocator, "const", "Define a constant.\n\n```runic\nconst myConst = 5\n```"),
        try makeKeyword(allocator, "var", "Define a variable.\n\n```runic\nvar myConst = 5\nmyConst = 3\n```"),
        try makeKeyword(allocator, "fn", "Declare a function.\n\n```runic\nfn hello() Void {\n    echo \"hello\"\n}\n```"),
    });

    return try list.toOwnedSlice(allocator);

    // .kw_error => "error",
    // .kw_enum => "enum",
    // .kw_union => "union",
    // .kw_if => "if",
    // .kw_else => "else",
    // .kw_for => "for",
    // .kw_while => "while",
    // .kw_match => "match",
    // .kw_return => "return",
    // .kw_bash => "bash",
    // .kw_try => "try",
    // .kw_catch => "catch",
    // .kw_true => "true",
    // .kw_false => "false",
    // .kw_null => "null",
}

pub const Workspace = struct {
    allocator: Allocator,
    roots: std.ArrayList([]const u8) = .empty,
    index: std.ArrayList(symbols.Symbol) = .empty,
    diagnostics: std.ArrayList(diag.Diagnostic) = .empty,
    documents: *LspDocumentStore,

    const self_dirs = [_][]const u8{ "src", "examples", "tests" };

    pub fn init(allocator: Allocator, documentStore: *LspDocumentStore) !Workspace {
        var workspace = Workspace{
            .allocator = allocator,
            .documents = documentStore,
        };

        try workspace.addKeywordsToIndex();

        return workspace;
    }

    pub fn deinit(self: *Workspace) void {
        for (self.roots.items) |root| {
            self.allocator.free(root);
        }
        self.roots.deinit(self.allocator);
        self.clearIndex();
        self.clearDiagnostics();
        self.index.deinit(self.allocator);
        self.diagnostics.deinit(self.allocator);
    }

    fn addKeywordsToIndex(self: *Workspace) !void {
        const keywords = try keywordSymbols(self.allocator);
        defer self.allocator.free(keywords);
        try self.index.appendSlice(self.allocator, keywords);
    }

    pub fn resetRoots(self: *Workspace, roots: []const []const u8) !void {
        for (self.roots.items) |root| {
            self.allocator.free(root);
        }
        self.roots.clearRetainingCapacity();
        for (roots) |root| {
            const duped = try self.allocator.dupe(u8, root);
            try self.roots.append(self.allocator, duped);
        }
    }

    pub fn refresh(self: *Workspace) !void {
        self.clearIndex();
        try self.addKeywordsToIndex();
        self.clearDiagnostics();
        // for (self.roots.items) |root| {
        //     try self.scanRoot(root);
        // }
    }

    pub fn symbolSlice(self: *Workspace) []const symbols.Symbol {
        return self.index.items;
    }

    pub fn describePath(self: *Workspace, absolute_path: []const u8) []const u8 {
        for (self.roots.items) |root| {
            if (absolute_path.len < root.len) continue;
            if (!std.mem.startsWith(u8, absolute_path, root)) continue;
            var suffix = absolute_path[root.len..];
            if (suffix.len == 0) return absolute_path;
            if (suffix[0] == '/' or suffix[0] == '\\') {
                suffix = suffix[1..];
            }
            return suffix;
        }
        return absolute_path;
    }

    pub fn symbolCount(self: *Workspace) usize {
        return self.index.items.len;
    }

    fn clearIndex(self: *Workspace) void {
        for (self.index.items) |*entry| {
            entry.deinit(self.allocator);
        }
        self.index.clearRetainingCapacity();
    }

    pub fn clearDiagnostics(self: *Workspace) void {
        for (self.diagnostics.items) |*entry| {
            entry.deinit(self.allocator);
        }
        self.diagnostics.clearRetainingCapacity();
    }

    fn scanRoot(self: *Workspace, root: []const u8) !void {
        for (self_dirs) |segment| {
            const absolute = try std.fs.path.join(self.allocator, &.{ root, segment });
            defer self.allocator.free(absolute);
            try self.walkDir(absolute, segment);
        }
    }

    fn walkDir(self: *Workspace, absolute_path: []const u8, rel_path: []const u8) !void {
        var dir = std.fs.openDirAbsolute(absolute_path, .{ .iterate = true }) catch |err| switch (err) {
            error.FileNotFound => return,
            else => return err,
        };
        defer dir.close();

        var it = dir.iterate();
        while (try it.next()) |entry| {
            const child_abs = try std.fs.path.join(self.allocator, &.{ absolute_path, entry.name });
            defer self.allocator.free(child_abs);
            const rel_child = try joinRelative(self.allocator, rel_path, entry.name);
            defer self.allocator.free(rel_child);

            switch (entry.kind) {
                .directory => try self.walkDir(child_abs, rel_child),
                .file => if (std.mem.endsWith(u8, entry.name, ".rn")) try self.indexFile(child_abs, rel_child),
                else => {},
            }
        }
    }

    fn indexFile(self: *Workspace, absolute_path: []const u8, detail: []const u8) !void {
        const file = std.fs.openFileAbsolute(absolute_path, .{}) catch |err| switch (err) {
            error.FileNotFound => return,
            else => return err,
        };
        defer file.close();
        const contents = file.readToEndAlloc(self.allocator, max_source_bytes) catch |err| switch (err) {
            error.FileTooBig => return error.SourceTooLarge,
            else => return err,
        };
        defer self.allocator.free(contents);
        // var parser = runic.parser.Parser.init(self.allocator, contents);
        var parser = runic.parser.Parser.init(self.allocator, self.documents.documentStore());
        defer parser.deinit();
        const script = try parseFile(self.allocator, &self.diagnostics, &parser, absolute_path) orelse return;
        try symbols.collectSymbols(self.allocator, detail, script, &self.index);
    }
};

fn joinRelative(allocator: Allocator, base: []const u8, name: []const u8) ![]u8 {
    if (base.len == 0) {
        return allocator.dupe(u8, name);
    }
    return std.fs.path.join(allocator, &.{ base, name });
}
