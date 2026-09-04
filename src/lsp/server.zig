const std = @import("std");
const workspace_mod = @import("workspace.zig");
const symbols = @import("symbols.zig");
const completion = @import("completion.zig");
const diag = @import("diagnostics.zig");
const types = @import("types.zig");
const json = @import("json.zig");
const document_mod = @import("document.zig");
const runic = @import("runic");

const Allocator = std.mem.Allocator;

/// Shared state threaded through the recursive parameter-hint walk.
const CallHintCtx = struct {
    arena: Allocator,
    hints: *std.ArrayList(types.InlayHint),
    fn_decls: *std.StringHashMap(runic.ast.FunctionDecl),
    range: types.Range,
    /// The document's module scope, used to resolve a `m.f` callee to the
    /// imported module (null when the document is not type-checked).
    scope: ?*runic.semantic.Scope,
};

const MAX_LINE = 16 * 1024;
const MAX_FILE = 4 * 1024 * 1024;
const MAX_OUT_CONTENT = 4 * 1024 * 1024;

pub const Server = struct {
    io: std.Io,
    allocator: Allocator,
    env_map: *std.process.Environ.Map,
    stdin_file: std.Io.File,
    stdout_file: std.Io.File,
    stderr_file: std.Io.File,
    stdin_reader: std.Io.File.Reader,
    reader: *std.Io.Reader,
    reader_buffer: [4096]u8 = undefined,
    stdout_writer: std.Io.File.Writer,
    writer: *std.Io.Writer,
    workspace: *workspace_mod.Workspace,
    documents: *document_mod.LspDocumentStore,
    initialized: bool = false,
    shutting_down: bool = false,
    log_enabled: bool = false,
    /// Set from the client's `completionItem.snippetSupport` capability at
    /// initialize time; gates whether completions emit snippet insert text.
    snippet_support: bool = false,

    pub fn init(
        io: std.Io,
        allocator: Allocator,
        environ_map: *std.process.Environ.Map,
        stdin_file: std.Io.File,
        stdout_file: std.Io.File,
        stderr_file: std.Io.File,
    ) !Server {
        var log_enabled = false;

        if (environ_map.get("RUNIC_LSP_LOG")) |value| {
            if (value.len != 0 and !std.mem.eql(u8, value, "0")) {
                log_enabled = true;
            }
        }

        const workspace = try allocator.create(workspace_mod.Workspace);
        const document_store = try allocator.create(document_mod.LspDocumentStore);

        workspace.* = try .init(io, allocator, environ_map, document_store);
        document_store.* = .init(io, allocator, environ_map, workspace);

        return .{
            .io = io,
            .allocator = allocator,
            .env_map = environ_map,
            .stdin_file = stdin_file,
            .stdout_file = stdout_file,
            .stderr_file = stderr_file,
            .stdin_reader = undefined,
            .reader = undefined,
            .stdout_writer = undefined,
            .writer = undefined,
            .workspace = workspace,
            .documents = document_store,
            .log_enabled = log_enabled,
        };
    }

    pub fn deinit(self: *Server) void {
        self.workspace.deinit();
        self.documents.deinit();
        self.allocator.destroy(self.workspace);
        self.allocator.destroy(self.documents);
    }

    pub fn initInterface(self: *Server) void {
        self.stdin_reader = self.stdin_file.readerStreaming(self.io, &self.reader_buffer);
        self.reader = &self.stdin_reader.interface;
        self.stdout_writer = self.stdout_file.writerStreaming(self.io, &.{});
        self.writer = &self.stdout_writer.interface;
    }

    pub fn run(self: *Server) !void {
        try self.log("runic-lsp server started", .{});
        while (true) {
            const payload = self.readMessage() catch |err| switch (err) {
                error.EndOfStream => break,
                else => {
                    try self.log("runic-lsp encountered an error: {}", .{err});
                    return err;
                },
            };
            defer self.allocator.free(payload);
            try self.log("Recieved message: {s}", .{payload});
            const continue_loop = try self.handleEnvelope(payload);
            if (!continue_loop) break;
            try self.flushDiagnostics();
        }
        try self.log("stdin ended", .{});
    }

    fn handleEnvelope(self: *Server, envelopePayload: []u8) !bool {
        const parsed: std.json.Parsed(types.ClientRequest) = try std.json.parseFromSlice(types.ClientRequest, self.allocator, envelopePayload, .{ .ignore_unknown_fields = true });
        defer parsed.deinit();

        const request = parsed.value;
        const payload = request.payload orelse {
            if (request.id) |id| {
                try self.sendError(id, -32601, "method not found");
            } else {
                try self.log("dropping notification for unknown method: {s}", .{request.method});
            }
            return true;
        };

        switch (payload) {
            .initialize => |params| {
                if (request.id) |id| try self.handleInitialize(id, params);
                return true;
            },
            .initialized => {
                try self.log("client signaled initialized", .{});
                return true;
            },
            .shutdown => {
                self.shutting_down = true;
                if (request.id) |id| try self.sendNullResult(id);
                return false;
            },
            .exit => {
                return false;
            },
            .@"textDocument/didOpen" => |params| {
                try self.handleDidOpen(params);
                return true;
            },
            .@"textDocument/didChange" => |params| {
                try self.handleDidChange(params);
                return true;
            },
            .@"textDocument/didClose" => |params| {
                try self.handleDidClose(params);
                return true;
            },
            .@"textDocument/completion" => |params| {
                if (request.id) |id| {
                    try self.handleCompletion(id, params);
                }
                return true;
            },
            .@"textDocument/hover" => |params| {
                if (request.id) |id| {
                    try self.handleHover(id, params);
                }
                return true;
            },
            .@"textDocument/definition" => |params| {
                if (request.id) |id| try self.handleDefinition(id, params);
                return true;
            },
            .@"textDocument/references" => |params| {
                if (request.id) |id| try self.handleReferences(id, params);
                return true;
            },
            .@"textDocument/documentHighlight" => |params| {
                if (request.id) |id| try self.handleDocumentHighlight(id, params);
                return true;
            },
            .@"textDocument/documentLink" => |params| {
                if (request.id) |id| try self.handleDocumentLink(id, params);
                return true;
            },
            .@"textDocument/inlayHint" => |params| {
                if (request.id) |id| try self.handleInlayHint(id, params);
                return true;
            },
            .@"textDocument/foldingRange" => |params| {
                if (request.id) |id| try self.handleFoldingRange(id, params);
                return true;
            },
            .@"textDocument/documentSymbol" => |params| {
                if (request.id) |id| try self.handleDocumentSymbols(id, params);
                return true;
            },
            .@"textDocument/rename" => |params| {
                if (request.id) |id| try self.handleRename(id, params);
                return true;
            },
            .@"textDocument/prepareRename" => |params| {
                if (request.id) |id| try self.handlePrepareRename(id, params);
                return true;
            },
            .@"textDocument/formatting" => |params| {
                if (request.id) |id| try self.handleFormatting(id, params);
                return true;
            },
            .@"workspace/symbol" => |params| {
                if (request.id) |id| try self.handleWorkspaceSymbol(id, params);
                return true;
            },
            .@"workspace/didChangeConfiguration", .@"workspace/didChangeWatchedFiles" => {
                try self.log("ignoring optional workspace notification: {s}", .{request.method});
                return true;
            },
            .@"$/cancelRequest" => {
                try self.log("client canceled request", .{});
                return true;
            },
        }
    }

    const DiagnosticPacket = struct {
        version: ?types.DocumentVersion = null,
        diagnostics: std.ArrayList(diag.Diagnostic) = .empty,

        pub fn deinit(self: *DiagnosticPacket, allocator: Allocator) void {
            self.diagnostics.deinit(allocator);
        }
    };

    fn groupWorkspaceDiagnostics(
        self: *Server,
        groups: *std.StringArrayHashMap(DiagnosticPacket),
    ) !void {
        for (self.workspace.diagnostics.items) |d| {
            const entry = try groups.getOrPut(d.uri);

            if (!entry.found_existing) {
                entry.value_ptr.* = .{};
            }

            try entry.value_ptr.diagnostics.append(self.allocator, d);
        }
    }

    fn groupDocumentsDiagnostics(
        self: *Server,
        groups: *std.hash_map.StringHashMap(DiagnosticPacket),
    ) !void {
        var docIt = self.documents.map.iterator();
        while (docIt.next()) |docEntry| {
            const document = docEntry.value_ptr.*;

            if (!document.shouldSendDiagnostics) continue;
            document.shouldSendDiagnostics = false;

            const diagnostics = document.diagnostics.items;
            const uri = try document.uri(self.allocator);
            const entry = try groups.getOrPut(uri);

            if (!entry.found_existing) {
                entry.value_ptr.* = .{};
            }

            try entry.value_ptr.diagnostics.appendSlice(self.allocator, diagnostics);
            entry.value_ptr.version = document.version;
        }
    }

    fn flushDiagnostics(self: *Server) !void {
        var groups: std.hash_map.StringHashMap(DiagnosticPacket) = .init(self.allocator);
        defer {
            var gIt = groups.iterator();
            while (gIt.next()) |entry| {
                entry.value_ptr.deinit(self.allocator);
                self.allocator.free(entry.key_ptr.*);
            }
            groups.deinit();
            self.workspace.clearDiagnostics();
            var docIt = self.documents.map.iterator();
            while (docIt.next()) |entry| entry.value_ptr.*.clearDiagnostics(self.allocator);
        }

        // try self.groupWorkspaceDiagnostics(&groups);
        try self.groupDocumentsDiagnostics(&groups);

        var it = groups.iterator();
        while (it.next()) |entry| {
            const uri = entry.key_ptr.*;
            const version = entry.value_ptr.version;
            const diagnostics = entry.value_ptr.diagnostics.items;
            try self.sendDiagnostics(uri, version, diagnostics);
        }
    }

    fn handleInitialize(self: *Server, id: types.RequestId, params: types.InitializeParams) !void {
        if (self.initialized) {
            try self.sendError(id, -32600, "Server already initialized");
            return;
        }

        var roots = std.ArrayList([]const u8).empty;
        defer {
            for (roots.items) |root| self.allocator.free(root);
            roots.deinit(self.allocator);
        }

        if (params.rootUri) |rootUri| {
            if (rootUri.len > 0) {
                try roots.append(self.allocator, try self.resolveUriPath(rootUri));
            }
        }
        if (roots.items.len == 0) {
            if (params.rootPath) |rootPath| {
                if (rootPath.len > 0) {
                    try roots.append(self.allocator, try self.absolutePath(rootPath));
                }
            }
        }
        if (params.workspaceFolders) |folders| {
            for (folders) |entry| {
                try roots.append(self.allocator, try self.resolveUriPath(entry.uri));
            }
        }

        // A client-provided root is indexed; the current-directory fallback is
        // not, since it could be an arbitrarily large tree unrelated to the
        // edited project.
        const has_explicit_root = roots.items.len > 0;
        if (roots.items.len == 0) {
            // `realPathFileAlloc` returns a sentinel-terminated `[:0]u8`; re-dupe it
            // into a plain slice so the workspace frees it with a matching size.
            const root = try std.Io.Dir.cwd().realPathFileAlloc(self.io, ".", self.allocator);
            defer self.allocator.free(root);
            try roots.append(self.allocator, try self.allocator.dupe(u8, root));
        }

        if (params.capabilities) |capabilities| {
            self.snippet_support = capabilities.snippetSupport();
        }

        try self.workspace.resetRoots(roots.items);
        try self.workspace.refresh();
        if (has_explicit_root) self.workspace.indexWorkspace();
        self.initialized = true;
        try self.log("workspace indexed {d} documents", .{self.documents.map.count()});
        try self.sendInitializeResult(id);
    }

    fn handleDidOpen(self: *Server, params: types.DidOpenTextDocumentParams) !void {
        const version = params.textDocument.version orelse 0;
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);
        try self.documents.openOrReplace(
            params.textDocument.uri,
            path,
            params.textDocument.text,
            version,
            .parse_and_type_check,
        );
    }

    fn handleDidChange(self: *Server, params: types.DidChangeTextDocumentParams) !void {
        if (params.contentChanges.len == 0) return;

        const uri = params.textDocument.uri;
        const version = params.textDocument.version orelse 0;
        const changes = params.contentChanges;

        _ = try self.documents.update(uri, changes, version, self.workspace);
    }

    fn handleDidClose(self: *Server, params: types.DidCloseTextDocumentParams) !void {
        try self.documents.close(params.textDocument.uri, self.workspace);
    }

    fn handleCompletion(
        self: *Server,
        id: types.RequestId,
        params: types.CompletionParams,
    ) !void {
        const doc = self.documents.get(params.textDocument.uri);
        const owned_path: []u8 = try self.resolveUriPath(params.textDocument.uri);
        var fallback_text: ?[]u8 = null;
        defer self.allocator.free(owned_path);
        defer if (fallback_text) |buffer| self.allocator.free(buffer);

        const text_slice: []const u8 = blk: {
            if (doc) |existing| break :blk existing.text;
            fallback_text = try readWholeFile(self.io, self.allocator, owned_path);
            break :blk fallback_text.?;
        };

        const line_val = params.position.line;
        const char_val = params.position.character;
        const line_index: usize = @as(usize, @intCast(line_val));
        const char_index: usize = @as(usize, @intCast(char_val));
        var loc = params.position.toLocation(owned_path);
        loc.offset = params.position.findIndex(text_slice) orelse 0;

        var scope = self.workspace.type_checker.getScopeFromLoc(loc);
        if (scope == null and loc.offset > 0) {
            var prev_loc = loc;
            prev_loc.offset -= 1;
            prev_loc.column -|= 1;
            scope = self.workspace.type_checker.getScopeFromLoc(prev_loc);
        }
        if (scope == null) {
            scope = self.workspace.type_checker.modules.get(owned_path);
        }

        // Trailing-dot recovery: a member access with an empty member (the cursor
        // right after `.`) is a syntax error, so the document never type-checked
        // and no scope is available. Re-check a repaired copy under a scratch
        // document to recover a scope for the object being completed. The scratch
        // document must stay open until after completion reads the scope (its
        // bindings reference the scratch AST), then it is closed here.
        var scratch_cleanup: ?[]const u8 = null;
        defer if (scratch_cleanup) |scratch_uri| {
            self.documents.close(scratch_uri, self.workspace) catch {};
            self.allocator.free(scratch_uri);
        };
        if (scope == null and trailingMemberDot(text_slice, loc.offset)) {
            scope = self.recoverTrailingDotScope(owned_path, text_slice, loc, &scratch_cleanup) catch null;
        }

        const doc_symbols = if (doc) |d| d.symbols.items else &[_]symbols.Symbol{};
        const workspace_symbols = self.workspace.symbolSlice();

        var matches = try completion.collectMatches(.{
            .io = self.io,
            .allocator = self.allocator,
            .env_map = self.env_map,
            .file = owned_path,
            .text_slice = text_slice,
            .line_index = line_index,
            .char_index = char_index,
            .doc_symbols = doc_symbols,
            .workspace_symbols = workspace_symbols,
            .scope = scope,
            .type_checker = &self.workspace.type_checker,
        });
        defer matches.deinit();

        try self.sendCompletionResult(id, matches.items.items);
    }

    /// True when `offset` sits immediately after a `.` that follows an
    /// identifier — i.e. the cursor is at an empty member access (`obj.`),
    /// the case that fails to parse and needs scope recovery.
    fn trailingMemberDot(text: []const u8, offset: usize) bool {
        if (offset == 0 or offset > text.len) return false;
        if (text[offset - 1] != '.') return false;
        if (offset < 2) return false;
        return runic.lexer.isIdentifierContinue(text[offset - 2]);
    }

    /// Recovers a scope for a completion whose document does not parse because
    /// of a trailing member dot. Inserts a placeholder identifier so the member
    /// access parses, type-checks the repaired text as a scratch document in the
    /// same directory (keeping relative imports resolvable), and returns the
    /// recovered scope. On success `*cleanup_out` receives the scratch document
    /// URI, which the caller must close and free once it has finished using the
    /// returned scope (its bindings reference the scratch document's AST).
    fn recoverTrailingDotScope(
        self: *Server,
        real_path: []const u8,
        text: []const u8,
        loc: runic.token.Location,
        cleanup_out: *?[]const u8,
    ) !?*runic.semantic.Scope {
        const placeholder = "__runic_lsp_completion__";
        const repaired = try std.fmt.allocPrint(self.allocator, "{s}{s}{s}", .{
            text[0..loc.offset],
            placeholder,
            text[loc.offset..],
        });
        defer self.allocator.free(repaired);

        const dir = std.fs.path.dirname(real_path) orelse ".";
        const scratch_path = try std.fs.path.join(self.allocator, &.{ dir, ".runic-lsp-completion.rn" });
        defer self.allocator.free(scratch_path);

        const scratch_uri = try self.documents.resolveUri(scratch_path);
        errdefer self.allocator.free(scratch_uri);

        try self.documents.openOrReplace(scratch_uri, scratch_path, repaired, 0, .parse_and_type_check);
        cleanup_out.* = scratch_uri;

        var scratch_loc = loc;
        scratch_loc.file = scratch_path;
        return self.workspace.type_checker.getScopeFromLoc(scratch_loc);
    }

    fn handleHover(
        self: *Server,
        id: types.RequestId,
        params: types.HoverParams,
    ) !void {
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);

        const doc = self.documents.get(params.textDocument.uri);

        var loc = params.position.toLocation(path);
        if (doc) |d| loc.offset = params.position.findIndex(d.text) orelse 0;
        const extracted_identifier = if (doc) |d| self.extractIdentifier(loc, d.text) else null;
        const extracted_member = if (doc) |d| self.extractMember(loc, d.text) else null;
        const scope = self.workspace.type_checker.getScopeFromLoc(loc);

        const binding: ?*runic.semantic.Scope.Binding = brk: {
            if (scope) |s| if (extracted_identifier) |i| {
                break :brk s.lookup(i.name);
            };

            break :brk null;
        };

        const member_binding: ?*runic.semantic.Scope.Binding = brk: {
            if (scope) |s| if (extracted_member) |m| {
                break :brk s.lookup(m.object_name);
            };

            break :brk null;
        };

        const range: types.Range = brk: {
            if (extracted_member) |m| break :brk .fromSpan(m.member_span);
            if (binding) |b| break :brk .fromSpan(b.identifier.span);
            if (extracted_identifier) |i| break :brk .fromSpan(i.span);
            break :brk .fromLocation(loc);
        };

        var alloc_writer = std.Io.Writer.Allocating.init(self.allocator);
        defer alloc_writer.deinit();

        if (extracted_member) |member| {
            if (member_binding) |b| {
                if (b.type_expr) |binding_type| {
                    self.writeHoverMember(&alloc_writer, binding_type, member.member_name);
                }
            }
        } else if (binding) |b| {
            self.writeHoverBinding(&alloc_writer, b);
        } else {
            // alloc_writer.writer.writeAll("Something went wrong.") catch {};
        }

        const result = types.Hover{
            .contents = .{
                .kind = .markdown,
                .value = alloc_writer.written(),
            },
            .range = range,
        };

        try self.sendJson(types.response(id, result));
    }

    const ExtractedIdentifier = struct {
        name: []const u8,
        span: runic.ast.Span,
    };

    const ExtractedMember = struct {
        object_name: []const u8,
        member_name: []const u8,
        member_span: runic.ast.Span,
    };

    fn findIdentifierRanges(
        self: *Server,
        text: []const u8,
        name: []const u8,
    ) !std.ArrayList(types.Range) {
        var ranges = std.ArrayList(types.Range).empty;
        if (name.len == 0) return ranges;

        var lexer = try runic.lexer.Lexer.init(self.io, self.allocator, self.env_map, "", text);
        defer lexer.deinit();

        while (true) {
            const tok = try lexer.next();
            switch (tok.tag) {
                .identifier => if (std.mem.eql(u8, tok.lexeme, name)) {
                    try ranges.append(self.allocator, types.Range.fromSpan(tok.span));
                },
                .eof => break,
                else => {},
            }
        }

        return ranges;
    }

    /// The declaration span of the binding that `name` resolves to at the given
    /// position in `doc_path`, or null when it does not resolve to a binding —
    /// e.g. a command name, an unbound identifier, or a document that was parsed
    /// but not type-checked. Used to make references/rename binding-aware rather
    /// than matching every same-named identifier.
    /// The declaration span the identifier `name` at `position` resolves to:
    /// for a member access `X.name`, the member's declaration (a struct field or
    /// an imported module's `pub` binding); otherwise the binding it names.
    /// Null when it does not resolve — a command, an unbound name, or a document
    /// that was parsed but not type-checked.
    fn declSpanAt(
        self: *Server,
        doc_path: []const u8,
        doc_text: []const u8,
        position: types.Position,
        name: []const u8,
    ) ?runic.ast.Span {
        // toLocation converts the 0-indexed LSP position to the 1-indexed
        // line/column the type checker's scopes use.
        var loc = position.toLocation(doc_path);
        loc.offset = position.findIndex(doc_text) orelse 0;

        // Workspace files loaded by the index are parsed but not type-checked, so
        // they have no scope until needed. Type-check on demand here (references
        // and rename are read-only, so nothing resets it until the next edit),
        // which lets member accesses in files that were never opened resolve.
        if (!self.workspace.type_checker.modules.contains(doc_path)) {
            _ = self.workspace.type_checker.typeCheck(doc_path) catch {};
        }

        if (self.extractMember(loc, doc_text)) |member| {
            if (self.workspace.type_checker.getScopeFromLoc(loc)) |scope| {
                if (self.resolveMemberFieldSpan(scope, member)) |span| return span;
            }
        }
        if (self.workspace.type_checker.getScopeFromLoc(loc)) |scope| {
            if (scope.lookup(name)) |binding| return binding.identifier.span;
        }
        return null;
    }

    /// Whether `range`'s start is exactly `decl`'s start position in `doc_path` —
    /// i.e. this occurrence IS the declaration. Catches declaration sites that
    /// do not themselves resolve to a scope binding (struct fields, a `pub`
    /// binding in a module that was only parsed).
    fn rangeIsDecl(doc_path: []const u8, range: types.Range, decl: runic.ast.Span) bool {
        if (!std.mem.eql(u8, doc_path, decl.start.file)) return false;
        const decl_range = types.Range.fromSpan(decl);
        return range.start.line == decl_range.start.line and range.start.character == decl_range.start.character;
    }

    /// Two declaration spans identify the same binding when they begin at the
    /// same position in the same file.
    fn sameDecl(a: runic.ast.Span, b: runic.ast.Span) bool {
        return a.start.line == b.start.line and
            a.start.column == b.start.column and
            std.mem.eql(u8, a.start.file, b.start.file);
    }

    fn extractIdentifier(
        self: *@This(),
        loc: runic.token.Location,
        text: []const u8,
    ) ?ExtractedIdentifier {
        const ch = text[loc.offset];

        if (!runic.lexer.isIdentifierStart(ch) and !runic.lexer.isIdentifierContinue(ch)) {
            return null;
        }

        var start = loc.offset -| 1;
        while (start > 0) : (start -|= 1) {
            if (runic.lexer.isIdentifierContinue(text[start])) continue;
            break;
        }

        if (!runic.lexer.isIdentifierStart(text[start])) start += 1;

        var lexer = runic.lexer.Lexer.init(self.io, self.allocator, self.env_map, loc.file, text[start..]) catch return null;
        defer lexer.deinit();
        const tok = lexer.next() catch return null;

        return switch (tok.tag) {
            .identifier => blk: {
                const start_column = loc.column -| (loc.offset - start);
                break :blk .{
                    .name = tok.lexeme,
                    .span = .{
                        .start = .{
                            .file = loc.file,
                            .line = loc.line,
                            .column = start_column,
                            .offset = start,
                        },
                        .end = .{
                            .file = loc.file,
                            .line = loc.line,
                            .column = start_column + tok.lexeme.len,
                            .offset = start + tok.lexeme.len,
                        },
                    },
                };
            },
            else => null,
        };
    }

    fn extractMember(
        self: *@This(),
        loc: runic.token.Location,
        text: []const u8,
    ) ?ExtractedMember {
        const member = self.extractIdentifier(loc, text) orelse return null;
        if (member.span.start.offset == 0) return null;
        if (text[member.span.start.offset - 1] != '.') return null;
        if (member.span.start.offset < 2) return null;

        const object_loc = runic.token.Location{
            .file = loc.file,
            .line = member.span.start.line,
            .column = member.span.start.column -| 2,
            .offset = member.span.start.offset - 2,
        };
        const object = self.extractIdentifier(object_loc, text) orelse return null;

        return .{
            .object_name = object.name,
            .member_name = member.name,
            .member_span = member.span,
        };
    }

    fn writeHoverBinding(
        _: *Server,
        alloc_writer: *std.Io.Writer.Allocating,
        binding: *const runic.semantic.Scope.Binding,
    ) void {
        alloc_writer.writer.writeAll("```\n") catch {};
        if (binding.is_mutable) {
            alloc_writer.writer.writeAll("var ") catch {};
        } else {
            alloc_writer.writer.writeAll("const ") catch {};
        }
        alloc_writer.writer.writeAll(binding.identifier.name) catch {};
        if (binding.identifier.isTypeIdentifier()) {
            alloc_writer.writer.writeAll(" = ") catch {};
        } else {
            alloc_writer.writer.writeAll(": ") catch {};
        }
        alloc_writer.writer.print("{?f}\n", .{binding.type_expr}) catch {};
        alloc_writer.writer.writeAll("```") catch {};
    }

    fn writeHoverMember(
        self: *Server,
        alloc_writer: *std.Io.Writer.Allocating,
        binding_type: *const runic.ast.TypeExpr,
        member_name: []const u8,
    ) void {
        const resolved_type = switch (binding_type.*) {
            .alias => |alias_type| self.workspace.type_checker.resolveAliasType(&alias_type),
            else => binding_type,
        };

        alloc_writer.writer.writeAll("```\n") catch {};
        alloc_writer.writer.print("const {s}: ", .{member_name}) catch {};

        switch (resolved_type.*) {
            .execution => {
                if (std.mem.eql(u8, member_name, "stdout") or std.mem.eql(u8, member_name, "stderr")) {
                    alloc_writer.writer.writeAll("[]Byte\n") catch {};
                } else if (std.mem.eql(u8, member_name, "exit_code")) {
                    alloc_writer.writer.writeAll("Int\n") catch {};
                } else if (std.mem.eql(u8, member_name, "wait")) {
                    alloc_writer.writer.writeAll("ExecutionResult\n") catch {};
                }
            },
            .thread => {
                if (std.mem.eql(u8, member_name, "wait")) {
                    alloc_writer.writer.writeAll("ExecutionResult\n") catch {};
                }
            },
            .struct_type => |struct_type| {
                if (struct_type.memberType(member_name)) |member_type| {
                    alloc_writer.writer.print("{f}\n", .{member_type}) catch {};
                }
            },
            else => {},
        }

        alloc_writer.writer.writeAll("```") catch {};
    }

    fn handleDefinition(
        self: *Server,
        id: types.RequestId,
        params: types.DefinitionParams,
    ) !void {
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);

        const doc = self.documents.get(params.textDocument.uri);

        var loc = params.position.toLocation(path);
        if (doc) |d| loc.offset = params.position.findIndex(d.text) orelse 0;
        const extracted_identifier = if (doc) |d| self.extractIdentifier(loc, d.text) else null;
        const extracted_member = if (doc) |d| self.extractMember(loc, d.text) else null;
        const scope = self.workspace.type_checker.getScopeFromLoc(loc);

        // Member access takes precedence: the cursor on `p.x` should resolve to
        // `x`'s field declaration in the struct, not to an unrelated `x` binding
        // that might happen to be in scope. Only jumps when the field/decl is
        // actually found, otherwise falls through to identifier resolution.
        if (scope) |s| if (extracted_member) |member| {
            if (self.resolveMemberFieldSpan(s, member)) |field_span| {
                try self.sendDefinitionSpan(id, field_span);
                return;
            }
        };

        const binding: ?*runic.semantic.Scope.Binding = brk: {
            if (scope) |s| if (extracted_identifier) |i| {
                break :brk s.lookup(i.name);
            };
            break :brk null;
        };

        if (binding) |b| {
            try self.sendDefinitionSpan(id, b.identifier.span);
            return;
        }

        if (extracted_identifier) |identifier| {
            var found_location: ?types.Location = null;
            var it = self.documents.map.iterator();
            while (it.next()) |entry| {
                const uri = entry.key_ptr.*;
                const ref_doc = entry.value_ptr.*;

                for (ref_doc.symbols.items) |sym| {
                    if (!std.mem.eql(u8, sym.name, identifier.name)) continue;
                    const location = types.Location{
                        .uri = uri,
                        .range = types.Range.fromSpan(sym.span),
                    };
                    if (found_location != null and !std.meta.eql(found_location.?, location)) {
                        try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
                        return;
                    }
                    found_location = location;
                }
            }

            if (found_location) |location| {
                try self.sendJson(types.response(id, location));
                return;
            }
        }

        try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
    }

    /// Sends a go-to-definition Location for `span`, resolving the target
    /// document URI from the span's source file.
    fn sendDefinitionSpan(self: *Server, id: types.RequestId, span: runic.ast.Span) !void {
        const definition_uri = try self.documents.resolveUri(span.start.file);
        defer self.allocator.free(definition_uri);
        const result = types.Location{
            .uri = definition_uri,
            .range = types.Range.fromSpan(span),
        };
        try self.sendJson(types.response(id, result));
    }

    /// Resolves a member access (`object.member`) to the span of the member's
    /// declaration in the object's struct type — a struct field's name, or a
    /// struct decl (method/const member). Returns null when the object is not a
    /// struct, has no such member, or has no known type (e.g. builtin members
    /// like `.stdout` on an execution result, which have no source declaration).
    fn resolveMemberFieldSpan(
        self: *Server,
        scope: *runic.semantic.Scope,
        member: ExtractedMember,
    ) ?runic.ast.Span {
        const binding = scope.lookup(member.object_name) orelse return null;
        const binding_type = binding.type_expr orelse return null;
        const resolved = switch (binding_type.*) {
            .alias => |alias_type| self.workspace.type_checker.resolveAliasType(&alias_type),
            else => binding_type,
        };
        switch (resolved.*) {
            .struct_type => |struct_type| {
                for (struct_type.fields) |field| {
                    if (std.mem.eql(u8, field.name.name, member.member_name)) {
                        return field.name.span;
                    }
                }
                for (struct_type.decls) |decl| {
                    if (std.mem.eql(u8, decl.name.name, member.member_name)) {
                        return decl.name.span;
                    }
                }
                return null;
            },
            // A module member (`m.foo`) resolves to the `pub` declaration in the
            // imported module file.
            .module => |module_type| {
                const module_scope = (self.workspace.type_checker.resolveModuleScopeForMemberCompletion(module_type) catch return null) orelse return null;
                const member_binding = module_scope.lookup(member.member_name) orelse return null;
                return member_binding.identifier.span;
            },
            else => return null,
        }
    }

    fn handleReferences(
        self: *Server,
        id: types.RequestId,
        params: types.ReferenceParams,
    ) !void {
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);

        const doc = self.documents.get(params.textDocument.uri);

        var loc = params.position.toLocation(path);
        if (doc) |d| loc.offset = params.position.findIndex(d.text) orelse 0;
        const extracted_identifier = if (doc) |d| self.extractIdentifier(loc, d.text) else null;

        var locations = std.ArrayList(types.Location).empty;
        defer locations.deinit(self.allocator);

        const reference_name = if (extracted_identifier) |i| i.name else null;

        if (reference_name) |name| {
            // Resolve the symbol under the cursor to a declaration (a binding, or
            // a struct field / imported module member for a member access). Keep
            // only occurrences resolving to that same declaration — plus the
            // declaration site itself. When it does not resolve (a command, an
            // unbound name, an un-type-checked file) fall back to matching by
            // name.
            const target_decl: ?runic.ast.Span = if (doc) |d| self.declSpanAt(path, d.text, params.position, name) else null;
            const declaration_file = if (target_decl) |td| td.start.file else null;
            const declaration_range = if (target_decl) |td| types.Range.fromSpan(td) else null;

            var it = self.documents.map.iterator();
            while (it.next()) |entry| {
                const uri = entry.key_ptr.*;
                const ref_doc = entry.value_ptr.*;
                var ranges = try self.findIdentifierRanges(ref_doc.text, name);
                defer ranges.deinit(self.allocator);

                for (ranges.items) |range| {
                    if (target_decl) |td| {
                        const occ = self.declSpanAt(ref_doc.path, ref_doc.text, range.start, name);
                        const is_match = (occ != null and sameDecl(occ.?, td)) or rangeIsDecl(ref_doc.path, range, td);
                        if (!is_match) continue;
                    }

                    if (!params.context.includeDeclaration and declaration_range != null and declaration_file != null and
                        std.mem.eql(u8, ref_doc.path, declaration_file.?) and std.meta.eql(range, declaration_range.?))
                    {
                        continue;
                    }

                    try locations.append(self.allocator, .{
                        .uri = uri,
                        .range = range,
                    });
                }
            }
        }

        try self.sendJson(types.response(id, locations.items));
    }

    fn handleDocumentHighlight(
        self: *Server,
        id: types.RequestId,
        params: types.DocumentHighlightParams,
    ) !void {
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);

        const doc = self.documents.get(params.textDocument.uri);

        var loc = params.position.toLocation(path);
        if (doc) |d| loc.offset = params.position.findIndex(d.text) orelse 0;
        const extracted_identifier = if (doc) |d| self.extractIdentifier(loc, d.text) else null;

        if (extracted_identifier == null or doc == null) {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        }

        // Highlight every occurrence of the identifier in the current document.
        // findIdentifierRanges lexes the source, so occurrences inside strings
        // and comments are excluded.
        var ranges = try self.findIdentifierRanges(doc.?.text, extracted_identifier.?.name);
        defer ranges.deinit(self.allocator);

        var highlights = std.ArrayList(types.DocumentHighlight).empty;
        defer highlights.deinit(self.allocator);
        for (ranges.items) |range| {
            try highlights.append(self.allocator, .{ .range = range, .kind = .text });
        }

        try self.sendJson(types.response(id, highlights.items));
    }

    fn handleDocumentLink(
        self: *Server,
        id: types.RequestId,
        params: types.DocumentLinkParams,
    ) !void {
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);

        const doc = self.documents.get(params.textDocument.uri) orelse {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        };

        // The response holds owned target strings; build it in an arena so the
        // whole set is freed in one shot after serialization.
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_allocator = arena.allocator();

        var links = std.ArrayList(types.DocumentLink).empty;

        // Lex the document for `import "<path>"` and turn each module path into a
        // link to the resolved file. Lexing (rather than the AST) means links
        // still resolve when the rest of the document does not parse.
        var lexer = try runic.lexer.Lexer.init(self.io, self.allocator, self.env_map, path, doc.text);
        defer lexer.deinit();

        while (true) {
            const tok = lexer.next() catch break;
            if (tok.tag == .eof) break;
            if (tok.tag != .kw_import) continue;

            var next = lexer.next() catch break;
            if (next.tag == .l_paren) next = lexer.next() catch break;
            if (next.tag != .string_start) continue;
            const string_text = lexer.next() catch break;
            if (string_text.tag != .string_text) continue;

            const module_path = runic.document.resolveModulePath(self.io, self.allocator, path, string_text.lexeme) catch continue;
            defer self.allocator.free(module_path);
            // Skip the embedded `std` namespace (a `:std/...` virtual path with no
            // file to open).
            if (module_path.len > 0 and module_path[0] == ':') continue;

            const target = self.documents.resolveUri(module_path) catch continue;
            defer self.allocator.free(target);

            try links.append(arena_allocator, .{
                .range = types.Range.fromSpan(string_text.span),
                .target = try arena_allocator.dupe(u8, target),
            });
        }

        try self.sendJson(types.response(id, links.items));
    }

    fn positionInRange(pos: types.Position, range: types.Range) bool {
        if (pos.line < range.start.line or (pos.line == range.start.line and pos.character < range.start.character)) return false;
        if (pos.line > range.end.line or (pos.line == range.end.line and pos.character > range.end.character)) return false;
        return true;
    }

    fn handleInlayHint(
        self: *Server,
        id: types.RequestId,
        params: types.InlayHintParams,
    ) !void {
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);

        const doc = self.documents.get(params.textDocument.uri) orelse {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        };
        const script = doc.ast orelse {
            try self.sendJson(types.response(id, &[_]types.InlayHint{}));
            return;
        };
        const module_scope = self.workspace.type_checker.modules.get(path);

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_allocator = arena.allocator();

        var hints = std.ArrayList(types.InlayHint).empty;

        // Show the inferred type after each top-level `const`/`var` that has no
        // explicit annotation, e.g. `const x«: Int» = 5`.
        for (script.statements) |stmt| {
            const binding_decl = switch (stmt.*) {
                .binding_decl => |b| b,
                else => continue,
            };
            if (binding_decl.annotation != null) continue;
            const identifier = switch (binding_decl.pattern.*) {
                .identifier => |i| i,
                else => continue,
            };
            const scope = module_scope orelse continue;
            const binding = scope.lookup(identifier.name) orelse continue;
            const type_expr = binding.type_expr orelse continue;

            const pos = types.Range.fromSpan(identifier.span).end;
            if (!positionInRange(pos, params.range)) continue;

            const label = try std.fmt.allocPrint(arena_allocator, ": {f}", .{type_expr});
            try hints.append(arena_allocator, .{
                .position = pos,
                .label = label,
                .kind = .type,
            });
        }

        // Parameter-name hints for calls: map each argument of a call to a
        // top-level function's parameter name (`greet «name:» "x"`). Build a
        // name -> declaration map first so a call to a function declared later
        // is still annotated.
        var fn_decls = std.StringHashMap(runic.ast.FunctionDecl).init(arena_allocator);
        for (script.statements) |stmt| {
            const expr_stmt = switch (stmt.*) {
                .expression => |e| e,
                else => continue,
            };
            switch (expr_stmt.expression.*) {
                .fn_decl => |fn_decl| if (fn_decl.name) |n| try fn_decls.put(n.name, fn_decl),
                else => {},
            }
        }
        const ctx = CallHintCtx{
            .arena = arena_allocator,
            .hints = &hints,
            .fn_decls = &fn_decls,
            .range = params.range,
            .scope = module_scope,
        };
        for (script.statements) |stmt| try self.walkStmtCalls(ctx, stmt);

        try self.sendJson(types.response(id, hints.items));
    }

    /// Recursively visits statements looking for calls to annotate with
    /// parameter-name hints — through binding initializers, control flow, and
    /// nested blocks, not just top-level statements.
    fn walkStmtCalls(self: *Server, ctx: CallHintCtx, stmt: *const runic.ast.Statement) (Allocator.Error)!void {
        switch (stmt.*) {
            .expression => |es| try self.walkExprCalls(ctx, es.expression),
            .binding_decl => |bd| try self.walkExprCalls(ctx, bd.initializer),
            .yield_stmt => |ys| try self.walkExprCalls(ctx, ys.value),
            .exit_stmt => |xs| if (xs.value) |v| try self.walkExprCalls(ctx, v),
            .while_stmt => |ws| {
                try self.walkExprCalls(ctx, ws.condition);
                for (ws.body.statements) |s| try self.walkStmtCalls(ctx, s);
            },
            .bash_block, .type_binding_decl => {},
        }
    }

    /// Recursively visits an expression, emitting parameter hints at each call
    /// and descending into pipelines, operands, call arguments, control-flow
    /// bodies, function bodies, and string interpolations.
    fn walkExprCalls(self: *Server, ctx: CallHintCtx, expr: *const runic.ast.Expression) (Allocator.Error)!void {
        switch (expr.*) {
            .call => |call| {
                try self.appendCallParamHints(ctx, call);
                try self.walkExprCalls(ctx, call.callee);
                for (call.arguments) |arg| try self.walkExprCalls(ctx, arg);
            },
            .pipeline => |p| for (p.stages) |s| try self.walkExprCalls(ctx, s),
            .binary => |b| {
                try self.walkExprCalls(ctx, b.left);
                try self.walkExprCalls(ctx, b.right);
            },
            .unary => |u| try self.walkExprCalls(ctx, u.operand),
            .assignment => |a| try self.walkExprCalls(ctx, a.expr),
            .block => |b| for (b.statements) |s| try self.walkStmtCalls(ctx, s),
            .fn_decl => |fd| try self.walkExprCalls(ctx, fd.body),
            .if_expr => |i| try self.walkIfExprCalls(ctx, &i),
            .for_expr => |f| {
                for (f.sources) |s| try self.walkExprCalls(ctx, s);
                try self.walkExprCalls(ctx, f.body);
            },
            .match_expr => |m| {
                try self.walkExprCalls(ctx, m.subject);
                for (m.cases) |c| for (c.body.statements) |s| try self.walkStmtCalls(ctx, s);
            },
            .literal => |lit| switch (lit) {
                .string => |s| for (s.segments) |seg| switch (seg) {
                    .interpolation => |e| try self.walkExprCalls(ctx, e),
                    else => {},
                },
                else => {},
            },
            else => {},
        }
    }

    fn walkIfExprCalls(self: *Server, ctx: CallHintCtx, if_expr: *const runic.ast.IfExpr) (Allocator.Error)!void {
        try self.walkExprCalls(ctx, if_expr.condition);
        try self.walkExprCalls(ctx, if_expr.then_expr);
        if (if_expr.else_branch) |else_branch| switch (else_branch) {
            .expr => |e| try self.walkExprCalls(ctx, e),
            .if_expr => |nested| try self.walkIfExprCalls(ctx, nested),
            .condition => {},
        };
    }

    fn appendCallParamHints(
        self: *Server,
        ctx: CallHintCtx,
        call: runic.ast.CallExpr,
    ) !void {
        const arena = ctx.arena;
        const hints = ctx.hints;
        const range = ctx.range;

        const params = self.resolveCallParams(ctx, call) orelse return;

        for (call.arguments, 0..) |arg, i| {
            if (i >= params.len) break;
            const param_name = switch (params[i].pattern.*) {
                .identifier => |identifier| identifier.name,
                else => continue,
            };
            const pos = types.Range.fromSpan(arg.span()).start;
            if (!positionInRange(pos, range)) continue;

            const label = try std.fmt.allocPrint(arena, "{s}:", .{param_name});
            try hints.append(arena, .{
                .position = pos,
                .label = label,
                .kind = .parameter,
                .paddingRight = true,
            });
        }
    }

    /// Resolves the parameter list for a call's callee: a same-file top-level
    /// function (identifier callee), or an imported module's function
    /// (`m.f` member callee, resolved through the module's AST). Returns null
    /// for anything else or a variadic function.
    fn resolveCallParams(self: *Server, ctx: CallHintCtx, call: runic.ast.CallExpr) ?[]const *runic.ast.Parameter {
        switch (call.callee.*) {
            .identifier => |identifier| {
                const fn_decl = ctx.fn_decls.get(identifier.name) orelse return null;
                return nonVariadicParams(fn_decl);
            },
            // Member access (`m.f`) is a binary expression with the `.member`
            // operator: left is the object, right is the member name.
            .binary => |binary| {
                if (std.meta.activeTag(binary.op) != .member) return null;
                const object_name = switch (binary.left.*) {
                    .identifier => |i| i.name,
                    else => return null,
                };
                const member_name = switch (binary.right.*) {
                    .identifier => |i| i.name,
                    else => return null,
                };
                const scope = ctx.scope orelse return null;
                const binding = scope.lookup(object_name) orelse return null;
                const binding_type = binding.type_expr orelse return null;
                const resolved = switch (binding_type.*) {
                    .alias => |alias_type| self.workspace.type_checker.resolveAliasType(&alias_type),
                    else => binding_type,
                };
                const module_type = switch (resolved.*) {
                    .module => |m| m,
                    else => return null,
                };
                const script = (self.documents.document_store.getAst(module_type.path) catch return null) orelse return null;
                const fn_decl = findTopLevelFn(script, member_name) orelse return null;
                return nonVariadicParams(fn_decl);
            },
            else => return null,
        }
    }

    fn nonVariadicParams(fn_decl: runic.ast.FunctionDecl) ?[]const *runic.ast.Parameter {
        return switch (fn_decl.params) {
            ._non_variadic => |ps| ps,
            ._variadic => null,
        };
    }

    fn findTopLevelFn(script: runic.ast.Script, name: []const u8) ?runic.ast.FunctionDecl {
        for (script.statements) |stmt| switch (stmt.*) {
            .expression => |es| switch (es.expression.*) {
                .fn_decl => |fn_decl| if (fn_decl.name) |n| {
                    if (std.mem.eql(u8, n.name, name)) return fn_decl;
                },
                else => {},
            },
            else => {},
        };
        return null;
    }

    fn handleFoldingRange(
        self: *Server,
        id: types.RequestId,
        params: types.FoldingRangeParams,
    ) !void {
        const doc = self.documents.get(params.textDocument.uri) orelse {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        };
        const script = doc.ast orelse {
            try self.sendJson(types.response(id, &[_]types.FoldingRange{}));
            return;
        };

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        var ranges = std.ArrayList(types.FoldingRange).empty;

        // Fold every top-level statement that spans more than one line — that
        // covers function bodies, struct definitions, and multi-line control
        // flow without needing to enumerate each node kind.
        for (script.statements) |stmt| {
            const range = types.Range.fromSpan(stmt.span());
            if (range.end.line > range.start.line) {
                try ranges.append(arena.allocator(), .{
                    .startLine = range.start.line,
                    .endLine = range.end.line,
                });
            }
        }

        try self.sendJson(types.response(id, ranges.items));
    }

    fn handleWorkspaceSymbol(
        self: *Server,
        id: types.RequestId,
        params: types.WorkspaceSymbolParams,
    ) !void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_allocator = arena.allocator();

        var results = std.ArrayList(types.SymbolInformation).empty;

        // The document store holds every workspace file indexed at startup plus
        // any open documents, so a single pass over it is the whole workspace.
        var it = self.documents.map.iterator();
        while (it.next()) |entry| {
            const uri = entry.key_ptr.*;
            for (entry.value_ptr.*.symbols.items) |sym| {
                try appendWorkspaceSymbol(arena_allocator, &results, uri, sym, null, params.query);
            }
        }

        try self.sendJson(types.response(id, results.items));
    }

    fn appendWorkspaceSymbol(
        arena: Allocator,
        results: *std.ArrayList(types.SymbolInformation),
        uri: []const u8,
        sym: symbols.Symbol,
        container: ?[]const u8,
        query: []const u8,
    ) !void {
        if (query.len == 0 or std.ascii.indexOfIgnoreCase(sym.name, query) != null) {
            try results.append(arena, .{
                .name = sym.name,
                .kind = documentSymbolKind(sym.kind),
                .location = .{ .uri = uri, .range = types.Range.fromSpan(sym.span) },
                .containerName = container,
            });
        }
        // Nested symbols (struct fields, function parameters) are searchable too,
        // tagged with their enclosing symbol as the container.
        for (sym.children) |child| {
            try appendWorkspaceSymbol(arena, results, uri, child, sym.name, query);
        }
    }

    fn handleDocumentSymbols(
        self: *Server,
        id: types.RequestId,
        params: types.DocumentSymbolParams,
    ) !void {
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);

        _ = self.documents.get(params.textDocument.uri) orelse {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        };

        // Build the (possibly nested) outline in an arena so the tree of child
        // slices is freed in one shot after serialization.
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_allocator = arena.allocator();

        var doc_symbols = std.ArrayList(types.DocumentSymbol).empty;

        if (self.workspace.documents.get(params.textDocument.uri)) |ws_doc| {
            for (ws_doc.symbols.items) |sym| {
                try doc_symbols.append(arena_allocator, try toDocumentSymbol(arena_allocator, sym));
            }
        }

        try self.sendJson(types.response(id, doc_symbols.items));
    }

    fn documentSymbolKind(kind: symbols.SymbolKind) types.SymbolKind {
        return switch (kind) {
            .function => .function,
            .method => .method,
            .variable => .variable,
            .field => .field,
            .module => .module,
            .keyword => .keyword,
            .@"struct" => .@"struct",
        };
    }

    fn toDocumentSymbol(arena: Allocator, sym: symbols.Symbol) !types.DocumentSymbol {
        const children = try arena.alloc(types.DocumentSymbol, sym.children.len);
        for (sym.children, 0..) |child, i| {
            children[i] = try toDocumentSymbol(arena, child);
        }
        return .{
            .name = sym.name,
            .kind = documentSymbolKind(sym.kind),
            .detail = sym.detail,
            .range = types.Range.fromSpan(sym.range_span orelse sym.span),
            .selectionRange = types.Range.fromSpan(sym.span),
            .children = if (children.len == 0) null else children,
        };
    }

    fn handleRename(
        self: *Server,
        id: types.RequestId,
        params: types.RenameParams,
    ) !void {
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);

        const doc = self.documents.get(params.textDocument.uri);

        var loc = params.position.toLocation(path);
        if (doc) |d| loc.offset = params.position.findIndex(d.text) orelse 0;
        const extracted_identifier = if (doc) |d| self.extractIdentifier(loc, d.text) else null;

        if (extracted_identifier == null) {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        }

        if (doc == null) {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        }
        const name = extracted_identifier.?.name;

        // Resolve the symbol under the cursor to a binding. When it resolves,
        // rename only occurrences that resolve to that same binding, so distinct
        // same-named symbols in other scopes or files are left alone. When it
        // does not resolve (a command, an unbound name, an un-type-checked file)
        // fall back to a lexical rename by name across every indexed document.
        // Either way findIdentifierRanges skips strings and comments, and editors
        // preview the edit before applying it.
        const target_decl: ?runic.ast.Span = self.declSpanAt(path, doc.?.text, params.position, name);

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_allocator = arena.allocator();

        var changes = std.ArrayList(types.DocumentChangeOperation).empty;

        var it = self.documents.map.iterator();
        while (it.next()) |entry| {
            const doc_uri = entry.key_ptr.*;
            const ref_doc = entry.value_ptr.*;
            var ranges = try self.findIdentifierRanges(ref_doc.text, name);
            defer ranges.deinit(self.allocator);
            if (ranges.items.len == 0) continue;

            var edits = std.ArrayList(types.TextEdit).empty;
            for (ranges.items) |range| {
                if (target_decl) |td| {
                    const occ = self.declSpanAt(ref_doc.path, ref_doc.text, range.start, name);
                    const is_match = (occ != null and sameDecl(occ.?, td)) or rangeIsDecl(ref_doc.path, range, td);
                    if (!is_match) continue;
                }
                try edits.append(arena_allocator, .{ .range = range, .newText = params.newName });
            }
            if (edits.items.len == 0) continue;

            try changes.append(arena_allocator, .{ .textDocumentEdit = .{
                .textDocument = .{ .uri = doc_uri, .version = null },
                .edits = edits.items,
            } });
        }

        if (changes.items.len == 0) {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        }

        const result = types.WorkspaceEdit{ .documentChanges = changes.items };
        try self.sendJson(types.response(id, result));
    }

    fn handlePrepareRename(
        self: *Server,
        id: types.RequestId,
        params: types.PrepareRenameParams,
    ) !void {
        const path = try self.resolveUriPath(params.textDocument.uri);
        defer self.allocator.free(path);

        const doc = self.documents.get(params.textDocument.uri) orelse {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        };

        var loc = params.position.toLocation(path);
        loc.offset = params.position.findIndex(doc.text) orelse 0;

        // Only an identifier is renameable; anywhere else returns null so the
        // client blocks the rename instead of offering an invalid one.
        const extracted = self.extractIdentifier(loc, doc.text) orelse {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        };

        try self.sendJson(types.response(id, types.PrepareRenameResult{
            .range = types.Range.fromSpan(extracted.span),
            .placeholder = extracted.name,
        }));
    }

    fn handleFormatting(
        self: *Server,
        id: types.RequestId,
        params: types.DocumentFormattingParams,
    ) !void {
        const doc = self.documents.get(params.textDocument.uri) orelse {
            try self.sendJson(types.response(id, std.json.Value{ .null = {} }));
            return;
        };

        const text = doc.text;
        var formatted = std.ArrayList(u8).empty;
        defer formatted.deinit(self.allocator);

        var i: usize = 0;
        var indent_level: usize = 0;
        var in_string = false;
        var string_char: u8 = '"';

        while (i < text.len) : (i += 1) {
            const ch = text[i];

            if (!in_string and (ch == '"' or ch == '\'')) {
                in_string = true;
                string_char = ch;
                try formatted.append(self.allocator, ch);
                continue;
            }

            if (in_string and ch == string_char and (i == 0 or text[i - 1] != '\\')) {
                in_string = false;
                try formatted.append(self.allocator, ch);
                continue;
            }

            if (in_string) {
                try formatted.append(self.allocator, ch);
                continue;
            }

            if (ch == '#') {
                try formatted.append(self.allocator, ch);
                while (i + 1 < text.len and text[i + 1] != '\n') : (i += 1) {
                    try formatted.append(self.allocator, text[i + 1]);
                }
                if (i + 1 < text.len and text[i + 1] == '\n') {
                    i += 1;
                    try formatted.append(self.allocator, '\n');
                    var j: usize = 0;
                    while (j < indent_level) : (j += 1) {
                        try formatted.append(self.allocator, ' ');
                        try formatted.append(self.allocator, ' ');
                    }
                }
                continue;
            }

            if (ch == '{') {
                try formatted.append(self.allocator, ch);
                try formatted.append(self.allocator, ' ');
                indent_level += 1;
                continue;
            }

            if (ch == '}') {
                indent_level -= 1;
                if (formatted.items.len > 1 and formatted.items[formatted.items.len - 1] == ' ') {
                    _ = formatted.pop();
                }
                try formatted.append(self.allocator, ch);
                continue;
            }

            if (ch == '\n') {
                try formatted.append(self.allocator, ch);
                var j: usize = 0;
                while (j < indent_level) : (j += 1) {
                    try formatted.append(self.allocator, ' ');
                    try formatted.append(self.allocator, ' ');
                }
                continue;
            }

            if (ch == ' ' or ch == '\t') {
                if (formatted.items.len == 0 or formatted.items[formatted.items.len - 1] == ' ' or formatted.items[formatted.items.len - 1] == '\n' or formatted.items[formatted.items.len - 1] == '{') {
                    continue;
                }
                try formatted.append(self.allocator, ch);
                continue;
            }

            try formatted.append(self.allocator, ch);
        }

        const text_edit = types.TextEdit{
            .range = types.Range{
                .start = .{ .line = 0, .character = 0 },
                .end = .{ .line = @intCast(std.mem.count(u8, text, "\n")), .character = 0 },
            },
            .newText = formatted.items,
        };

        try self.sendJson(types.response(id, &.{text_edit}));
    }

    fn sendInitializeResult(self: *Server, id: types.RequestId) !void {
        const result = types.InitializeResult{
            .capabilities = .{
                .textDocumentSync = .{ .payload = .{
                    .textDocumentSyncOptions = .{
                        .openClose = true,
                        .change = .incremental,
                    },
                } },
                .completionProvider = .{
                    .triggerCharacters = &.{ ".", ":", "\"", "/" },
                    .resolveProvider = false,
                },
                .hoverProvider = .{ .payload = .{
                    .hoverOptions = .{
                        .workDoneProgress = false,
                    },
                } },
                .definitionProvider = .{ .payload = .{
                    .definitionOptions = .{
                        .workDoneProgress = false,
                    },
                } },
                .referencesProvider = .{ .payload = .{
                    .referenceOptions = .{
                        .workDoneProgress = false,
                    },
                } },
                .documentHighlightProvider = .{ .payload = .{
                    .documentHighlightOptions = .{
                        .workDoneProgress = false,
                    },
                } },
                .documentLinkProvider = .{
                    .resolveProvider = false,
                },
                .inlayHintProvider = .{ .payload = .{
                    .inlayHintOptions = .{
                        .resolveProvider = false,
                    },
                } },
                .foldingRangeProvider = .{ .payload = .{
                    .foldingRangeOptions = .{},
                } },
                .workspaceSymbolProvider = .{ .payload = .{
                    .workspaceSymbolOptions = .{
                        .workDoneProgress = false,
                    },
                } },
                .documentSymbolProvider = .{ .payload = .{
                    .documentSymbolOptions = .{
                        .workDoneProgress = false,
                    },
                } },
                .renameProvider = .{ .payload = .{
                    .renameOptions = .{
                        .prepareProvider = true,
                    },
                } },
                .documentFormattingProvider = .{ .payload = .{
                    .documentFormattingOptions = .{
                        .workDoneProgress = false,
                    },
                } },
            },
            .serverInfo = .{
                .name = "runic-lsp",
                .version = "0.1",
            },
        };

        try self.sendJson(types.response(id, result));
    }

    fn sendCompletionResult(self: *Server, id: types.RequestId, items: []const completion.Match) !void {
        var completionItems = try self.allocator.alloc(types.CompletionItem, items.len);
        defer self.allocator.free(completionItems);
        for (items, 0..) |item, i| completionItems[i] = .fromSymbol(item.symbol.get(), self.snippet_support);

        const result = types.CompletionList{
            .isIncomplete = false,
            .items = completionItems,
        };

        try self.sendJson(types.response(id, result));
    }

    fn sendDiagnostics(
        self: *Server,
        uri: []const u8,
        version: ?types.DocumentVersion,
        diagnostics: []const diag.Diagnostic,
    ) !void {
        try self.log("sending diagnostics: {} version: {?} uri: {s}", .{ diagnostics.len, version, uri });
        if (diagnostics.len == 1) {
            try self.log("{f}", .{std.json.fmt(diagnostics[0], .{})});
        }

        var diagnosticsResult = try self.allocator.alloc(types.Diagnostic, diagnostics.len);
        defer self.allocator.free(diagnosticsResult);
        for (diagnostics, 0..) |d, i| diagnosticsResult[i] = .fromDiag(d);

        const result: types.PublishDiagnosticsParams = .{
            .uri = uri,
            .version = version,
            .diagnostics = diagnosticsResult,
        };

        try self.sendJson(types.methodResponse("textDocument/publishDiagnostics", result));
    }

    fn sendNullResult(self: *Server, id: types.RequestId) !void {
        try self.sendJson(types.response(id, @as(?u32, null)));
    }

    fn sendError(self: *Server, id: types.RequestId, code: i32, message: []const u8) !void {
        try self.sendJson(types.responseError(id, code, message));
    }

    fn readMessage(self: *Server) ![]u8 {
        const content_length = try self.readHeaders();
        var buffer = try self.allocator.alloc(u8, content_length);
        errdefer self.allocator.free(buffer);
        var filled: usize = 0;
        while (filled < buffer.len) {
            const bytes_read = try self.reader.readSliceShort(buffer[filled..]);
            filled += bytes_read;
        }
        return buffer;
    }

    fn readHeaders(self: *Server) !usize {
        var content_length: ?usize = null;
        while (true) {
            const line = try readLine(self.allocator, self.reader);
            defer self.allocator.free(line);
            const trimmed = std.mem.trimEnd(u8, line, "\r");
            if (trimmed.len == 0) break;
            if (std.mem.startsWith(u8, trimmed, "Content-Length")) {
                const sep = std.mem.indexOfScalar(u8, trimmed, ':') orelse return error.ProtocolError;
                const value = std.mem.trim(u8, trimmed[sep + 1 ..], " ");
                content_length = try std.fmt.parseInt(usize, value, 10);
            }
        }
        return content_length orelse error.ProtocolError;
    }

    fn sendJson(self: *Server, json_body: anytype) !void {
        try self.log("Sent JSON: {f}", .{std.json.fmt(json_body, .{ .emit_null_optional_fields = false })});

        var buffer: [MAX_OUT_CONTENT]u8 = undefined;
        var writer = std.Io.Writer.fixed(&buffer);
        try writer.print("{f}", .{std.json.fmt(json_body, .{})});
        const body = writer.buffered();

        try self.writer.print("Content-Length: {d}\r\n\r\n", .{body.len});
        try self.writer.writeAll(body);
        try self.writer.flush();
    }

    fn log(self: *Server, comptime fmt: []const u8, args: anytype) !void {
        if (!self.log_enabled) return;
        var stderr = self.stderr_file.writer(self.io, &.{});
        try stderr.interface.print("[runic-lsp] ", .{});
        try stderr.interface.print(fmt, args);
        try stderr.interface.writeByte('\n');
        try stderr.interface.flush();
    }

    /// Returns absolute path
    fn resolveUriPath(self: *Server, uri: []const u8) ![]u8 {
        if (std.mem.startsWith(u8, uri, "file://")) {
            const decoded = try percentDecode(self.allocator, uri[7..]);
            defer self.allocator.free(decoded);
            return try self.absolutePath(decoded);
        }
        return try self.absolutePath(uri);
    }

    fn absolutePath(self: *Server, path: []const u8) ![]u8 {
        if (std.fs.path.isAbsolute(path)) {
            return try self.allocator.dupe(u8, path);
        }
        // `realPathFileAlloc` returns a sentinel-terminated `[:0]u8`; re-dupe it
        // into a plain slice so callers can free it with a matching size.
        const real = try std.Io.Dir.cwd().realPathFileAlloc(self.io, path, self.allocator);
        defer self.allocator.free(real);
        return try self.allocator.dupe(u8, real);
    }
};

fn readLine(allocator: Allocator, reader: *std.Io.Reader) ![]u8 {
    // `takeDelimiterInclusive` consumes and returns the trailing '\n'; drop it so
    // callers see the bare line (they still trim the '\r').
    const line = try reader.takeDelimiterInclusive('\n');
    if (line.len > MAX_LINE) return error.ProtocolError;
    return try allocator.dupe(u8, line[0 .. line.len - 1]);
}

fn percentDecode(allocator: Allocator, text: []const u8) ![]u8 {
    var buffer = try allocator.alloc(u8, text.len);
    var out: usize = 0;
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        const ch = text[i];
        if (ch == '%' and i + 2 < text.len) {
            const high = parseHexDigit(text[i + 1]) orelse break;
            const low = parseHexDigit(text[i + 2]) orelse break;
            buffer[out] = @as(u8, (high << 4) | low);
            out += 1;
            i += 2;
            continue;
        }
        buffer[out] = ch;
        out += 1;
    }
    return buffer[0..out];
}

fn parseHexDigit(ch: u8) ?u8 {
    if (ch >= '0' and ch <= '9') return ch - '0';
    if (ch >= 'a' and ch <= 'f') return 10 + (ch - 'a');
    if (ch >= 'A' and ch <= 'F') return 10 + (ch - 'A');
    return null;
}

fn readWholeFile(io: std.Io, allocator: Allocator, path: []const u8) ![]u8 {
    const file = try std.Io.Dir.openFileAbsolute(io, path, .{});
    defer file.close(io);
    var buffer: [1024]u8 = undefined;
    var reader = file.reader(io, &buffer);
    return reader.interface.allocRemaining(allocator, .limited(MAX_FILE));
}
