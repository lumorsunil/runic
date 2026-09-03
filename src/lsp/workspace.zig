const std = @import("std");
const symbols = @import("symbols.zig");
const diag = @import("diagnostics.zig");
const runic = @import("runic");
const LspDocumentStore = @import("document.zig").LspDocumentStore;

const Allocator = std.mem.Allocator;

fn makeKeyword(
    allocator: Allocator,
    name: []const u8,
    documentation: []const u8,
    snippet: []const u8,
) !symbols.Symbol {
    return .{
        .detail = try allocator.dupe(u8, "keyword"),
        .documentation = try allocator.dupe(u8, documentation),
        .snippet = if (snippet.len > 0) try allocator.dupe(u8, snippet) else &[_]u8{},
        .name = try allocator.dupe(u8, name),
        .kind = .keyword,
        .span = .global,
    };
}

fn keywordSymbols(allocator: Allocator) ![]const symbols.Symbol {
    var list = std.ArrayList(symbols.Symbol).empty;

    try list.appendSlice(allocator, &.{
        try makeKeyword(allocator, "import", "# import\nImport a runic module.\n\n```\nconst lib = import \"lib.rn\"\n```", "const ${1:name} = import \"${2:module.rn}\""),
        try makeKeyword(allocator, "const", "Define a constant.\n\n```\nconst myConst = 5\n```", "const ${1:name} = ${2:value}"),
        try makeKeyword(allocator, "var", "Define a variable.\n\n```\nvar myConst = 5\nmyConst = 3\n```", "var ${1:name} = ${2:value}"),
        try makeKeyword(allocator, "fn", "Declare a function.\n\n```\nfn Void hello(name: String) Void {\n    echo \"hello ${name}\"\n}\n```", "fn ${1:Void} ${2:name}(${3:params}) ${4:Void} {\n\t$0\n}"),
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
    // .kw_try => "try",
    // .kw_catch => "catch",
    // .kw_true => "true",
    // .kw_false => "false",
    // .kw_null => "null",
}

pub const Workspace = struct {
    io: std.Io,
    allocator: Allocator,
    roots: std.ArrayList([]const u8) = .empty,
    index: std.ArrayList(symbols.Symbol) = .empty,
    diagnostics: std.ArrayList(diag.Diagnostic) = .empty,
    documents: *LspDocumentStore,
    type_checker: runic.semantic.TypeChecker,

    pub fn init(
        io: std.Io,
        allocator: Allocator,
        env_map: *std.process.Environ.Map,
        documentStore: *LspDocumentStore,
    ) !Workspace {
        var workspace = Workspace{
            .io = io,
            .allocator = allocator,
            .documents = documentStore,
            .type_checker = .init(
                io,
                allocator,
                &documentStore.document_store,
                env_map,
                false, // strict mode is a CLI opt-in, not applied to LSP analysis
            ),
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
        self.type_checker.deinit();
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
    }

    /// Walks the workspace roots and loads every `.rn` file into the document
    /// store so its symbols are available to workspace search and cross-file
    /// navigation. Called only for roots the client explicitly provided (never
    /// the current-directory fallback, which could be an arbitrarily large tree).
    pub fn indexWorkspace(self: *Workspace) void {
        for (self.roots.items) |root| {
            // A scan failure (e.g. an unreadable directory) must not abort
            // startup — the server still works with whatever was indexed.
            self.scanRoot(root) catch |err| {
                std.log.err("workspace scan failed for {s}: {}", .{ root, err });
            };
        }
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

    /// Directory names skipped during the workspace scan — build/output caches
    /// and dependency trees that contain no first-party source.
    const skip_dirs = [_][]const u8{ "zig-cache", ".zig-cache", "zig-out", "node_modules", "target", ".git" };

    fn shouldSkipDir(name: []const u8) bool {
        if (name.len > 0 and name[0] == '.') return true;
        for (skip_dirs) |skip| {
            if (std.mem.eql(u8, name, skip)) return true;
        }
        return false;
    }

    fn scanRoot(self: *Workspace, root: []const u8) !void {
        try self.walkDir(root);
    }

    fn walkDir(self: *Workspace, absolute_path: []const u8) !void {
        var dir = std.Io.Dir.openDirAbsolute(self.io, absolute_path, .{ .iterate = true }) catch |err| switch (err) {
            error.FileNotFound => return,
            else => return err,
        };
        defer dir.close(self.io);

        var it = dir.iterate();
        while (try it.next(self.io)) |entry| {
            switch (entry.kind) {
                .directory => {
                    if (shouldSkipDir(entry.name)) continue;
                    const child_abs = try std.fs.path.join(self.allocator, &.{ absolute_path, entry.name });
                    defer self.allocator.free(child_abs);
                    try self.walkDir(child_abs);
                },
                .file => {
                    if (!std.mem.endsWith(u8, entry.name, ".rn")) continue;
                    const child_abs = try std.fs.path.join(self.allocator, &.{ absolute_path, entry.name });
                    defer self.allocator.free(child_abs);
                    self.indexFile(child_abs);
                },
                else => {},
            }
        }
    }

    /// Loads a workspace file into the document store as a server-managed
    /// document so its symbols become available to navigation and workspace
    /// search. Reuses the store's parse + symbol collection, whose spans
    /// reference the document's long-lived path. Per-file failures are ignored.
    fn indexFile(self: *Workspace, absolute_path: []const u8) void {
        const uri = self.documents.resolveUri(absolute_path) catch return;
        defer self.allocator.free(uri);
        _ = self.documents.requestDocument(uri, absolute_path, .open_and_parse_only) catch return;
    }
};
