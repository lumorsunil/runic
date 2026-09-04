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
    env_map: *std.process.Environ.Map,
    type_checker: runic.semantic.TypeChecker,
    /// Whether the workspace roots should be scanned into the index. Set only
    /// for client-provided roots (never the current-directory fallback).
    should_index: bool = false,
    /// Whether the one-time workspace scan has run yet. The scan is lazy — it
    /// happens on the first workspace-wide request, not at initialize — so a
    /// slow or pathological file can never block startup.
    indexed: bool = false,

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
            .env_map = env_map,
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
        self.addPathCommandsToIndex();
        self.clearDiagnostics();
    }

    /// Indexes the executables found on `$PATH` so they can be completed when
    /// writing a command. Each becomes a symbol whose detail is the resolved
    /// path. Names are de-duplicated in `$PATH` order (first directory wins,
    /// matching shell lookup). Best-effort: unreadable directories are skipped.
    fn addPathCommandsToIndex(self: *Workspace) void {
        const path_var = self.env_map.get("PATH") orelse return;

        // The seen-set keys live in an arena freed when the scan finishes; the
        // index owns its own duplicated name/detail strings.
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        var seen = std.StringHashMap(void).init(arena.allocator());

        var dirs = std.mem.splitScalar(u8, path_var, ':');
        while (dirs.next()) |dir_path| {
            if (dir_path.len == 0) continue;
            var dir = std.Io.Dir.openDirAbsolute(self.io, dir_path, .{ .iterate = true }) catch continue;
            defer dir.close(self.io);

            var it = dir.iterate();
            while (it.next(self.io) catch break) |entry| {
                if (entry.kind == .directory) continue;
                if (seen.contains(entry.name)) continue;

                const full = std.fs.path.join(self.allocator, &.{ dir_path, entry.name }) catch continue;
                defer self.allocator.free(full);

                self.appendCommandSymbol(entry.name, full) catch continue;
                const key = arena.allocator().dupe(u8, entry.name) catch continue;
                seen.put(key, {}) catch {};
            }
        }
    }

    fn appendCommandSymbol(self: *Workspace, name: []const u8, path: []const u8) !void {
        var entry = symbols.Symbol{
            .name = try self.allocator.dupe(u8, name),
            .detail = try self.allocator.dupe(u8, path),
            .kind = .function,
            .span = .global,
        };
        errdefer entry.deinit(self.allocator);
        try self.index.append(self.allocator, entry);
    }

    /// Runs the one-time workspace scan if it is enabled and has not run yet.
    /// Called lazily from workspace-wide requests (symbol search, references,
    /// rename, cross-file definition) rather than at initialize, so the cost —
    /// or a pathological file — only ever affects the first such request, never
    /// startup or the per-file features (hover, completion, diagnostics).
    pub fn ensureIndexed(self: *Workspace) void {
        if (self.indexed or !self.should_index) return;
        self.indexed = true;
        self.indexWorkspace();
    }

    /// Walks the workspace roots and loads every `.rn` file into the document
    /// store so its symbols are available to workspace search and cross-file
    /// navigation. Scans only roots the client explicitly provided (never the
    /// current-directory fallback, which could be an arbitrarily large tree).
    fn indexWorkspace(self: *Workspace) void {
        for (self.roots.items) |root| {
            // A scan failure (e.g. an unreadable directory) must not abort the
            // request — the server still works with whatever was indexed.
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
