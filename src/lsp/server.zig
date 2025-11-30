const std = @import("std");
const workspace_mod = @import("workspace.zig");
const symbols = @import("symbols.zig");
const completion = @import("completion.zig");
const diag = @import("diagnostics.zig");
const runic = @import("runic");
const token = runic.token;
const parseFile = @import("parser.zig").parseFile;
const Parser = @import("parser.zig").Parser;
const types = @import("types.zig");
const json = @import("json.zig");
const document_mod = @import("document.zig");

const Allocator = std.mem.Allocator;

const MAX_LINE = 16 * 1024;
const MAX_FILE = 4 * 1024 * 1024;
const MAX_OUT_CONTENT = 4 * 1024 * 1024;

pub const Server = struct {
    allocator: Allocator,
    stdin_file: std.fs.File,
    stdout_file: std.fs.File,
    stderr_file: std.fs.File,
    stdin_reader: std.fs.File.Reader,
    reader: *std.Io.Reader,
    reader_buffer: [4096]u8 = undefined,
    stdout_writer: std.fs.File.Writer,
    writer: *std.Io.Writer,
    workspace: workspace_mod.Workspace,
    documents: document_mod.DocumentStore,
    initialized: bool = false,
    shutting_down: bool = false,
    log_enabled: bool = false,

    pub fn init(
        allocator: Allocator,
        stdin_file: std.fs.File,
        stdout_file: std.fs.File,
        stderr_file: std.fs.File,
    ) Server {
        var log_enabled = false;
        if (std.process.getEnvVarOwned(allocator, "RUNIC_LSP_LOG")) |value| {
            defer allocator.free(value);
            if (value.len != 0 and !std.mem.eql(u8, value, "0")) {
                log_enabled = true;
            }
        } else |_| {}

        return .{
            .allocator = allocator,
            .stdin_file = stdin_file,
            .stdout_file = stdout_file,
            .stderr_file = stderr_file,
            .stdin_reader = undefined,
            .reader = undefined,
            .stdout_writer = undefined,
            .writer = undefined,
            .workspace = workspace_mod.Workspace.init(allocator, undefined),
            .documents = document_mod.DocumentStore.init(allocator),
            .log_enabled = log_enabled,
        };
    }

    pub fn deinit(self: *Server) void {
        self.workspace.deinit();
        self.documents.deinit();
    }

    pub fn initInterface(self: *Server) void {
        self.workspace.documents = &self.documents;
        self.stdin_reader = self.stdin_file.readerStreaming(&self.reader_buffer);
        self.reader = &self.stdin_reader.interface;
        self.stdout_writer = self.stdout_file.writerStreaming(&.{});
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
                return true;
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
                    // try self.handleCompletion(id, params);
                    self.handleCompletion(id, params) catch |err| {
                        std.log.err("Could not handleCompletion: {}, writer error: {?}", .{ err, self.stdout_writer.err });
                        return err;
                    };
                }
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
        groups: *std.StringArrayHashMap(DiagnosticPacket),
    ) !void {
        var docIt = self.documents.map.iterator();
        while (docIt.next()) |docEntry| {
            if (!docEntry.value_ptr.shouldSendDiagnostics) continue;
            docEntry.value_ptr.shouldSendDiagnostics = false;

            const diagnostics = docEntry.value_ptr.diagnostics.items;
            const uri = try docEntry.value_ptr.uri(self.allocator);
            const entry = try groups.getOrPut(uri);

            if (!entry.found_existing) {
                entry.value_ptr.* = .{};
            }

            try entry.value_ptr.diagnostics.appendSlice(self.allocator, diagnostics);
            entry.value_ptr.version = docEntry.value_ptr.version;
        }
    }

    fn flushDiagnostics(self: *Server) !void {
        var groups: std.StringArrayHashMap(DiagnosticPacket) = .init(self.allocator);
        defer {
            var gIt = groups.iterator();
            while (gIt.next()) |entry| {
                entry.value_ptr.deinit(self.allocator);
                self.allocator.free(entry.key_ptr.*);
            }
            groups.deinit();
            self.workspace.clearDiagnostics();
            var docIt = self.documents.map.iterator();
            while (docIt.next()) |entry| entry.value_ptr.clearDiagnostics(self.allocator);
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

        var roots = std.ArrayList([]const u8){};
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

        if (roots.items.len == 0) {
            try roots.append(self.allocator, try std.fs.cwd().realpathAlloc(self.allocator, "."));
        }

        try self.workspace.resetRoots(roots.items);
        try self.workspace.refresh();
        self.initialized = true;
        try self.log("workspace scan indexed {d} symbols", .{self.workspace.symbolCount()});
        try self.sendInitializeResult(id);
    }

    fn handleDidOpen(self: *Server, params: types.DidOpenTextDocumentParams) !void {
        const version = params.textDocument.version orelse 0;
        const path = try self.resolveUriPath(params.textDocument.uri);
        errdefer self.allocator.free(path);
        try self.documents.openOrReplace(params.textDocument.uri, path, params.textDocument.text, version, &self.workspace);
    }

    fn handleDidChange(self: *Server, params: types.DidChangeTextDocumentParams) !void {
        if (params.contentChanges.len == 0) return;

        const uri = params.textDocument.uri;
        const version = params.textDocument.version orelse 0;
        const changes = params.contentChanges;

        _ = try self.documents.update(uri, changes, version, &self.workspace);
    }

    fn handleDidClose(self: *Server, params: types.DidCloseTextDocumentParams) !void {
        self.documents.close(params.textDocument.uri);
    }

    fn handleCompletion(
        self: *Server,
        id: types.RequestId,
        params: types.CompletionParams,
    ) !void {
        const doc = self.documents.get(params.textDocument.uri);
        var owned_path: ?[]u8 = null;
        var fallback_text: ?[]u8 = null;
        defer if (owned_path) |path| self.allocator.free(path);
        defer if (fallback_text) |buffer| self.allocator.free(buffer);

        const text_slice: []const u8 = blk: {
            if (doc) |existing| break :blk existing.text;
            owned_path = try self.resolveUriPath(params.textDocument.uri);
            fallback_text = try readWholeFile(self.allocator, owned_path.?);
            break :blk fallback_text.?;
        };

        const line_val = params.position.line;
        const char_val = params.position.character;
        const line_index: usize = if (line_val < 0) 0 else @as(usize, @intCast(line_val));
        const char_index: usize = if (char_val < 0) 0 else @as(usize, @intCast(char_val));
        const prefix = extractPrefix(text_slice, line_index, char_index);
        const doc_symbols = if (doc) |d| d.symbols.items else &[_]symbols.Symbol{};
        const workspace_symbols = self.workspace.symbolSlice();

        var matches = try completion.collectMatches(self.allocator, prefix, doc_symbols, workspace_symbols);
        defer matches.deinit();

        try self.sendCompletionResult(id, matches.items.items);
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
                    .triggerCharacters = &.{ ".", ":" },
                    .resolveProvider = false,
                },
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
        for (items, 0..) |item, i| completionItems[i] = .fromSymbol(item.symbol.*);

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
        std.log.err("sending diagnostics: {} version: {?} uri: {s}", .{ diagnostics.len, version, uri });

        if (diagnostics.len == 1) {
            std.log.err("{f}", .{std.json.fmt(diagnostics[0], .{})});
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
            const trimmed = std.mem.trimRight(u8, line, "\r");
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
        try self.log("Sent JSON: {f}", .{std.json.fmt(json_body, .{})});

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
        var stderr = self.stderr_file.writer(&.{});
        try stderr.interface.print("[runic-lsp] ", .{});
        try stderr.interface.print(fmt, args);
        try stderr.interface.writeByte('\n');
        try stderr.interface.flush();
    }

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
        return try std.fs.cwd().realpathAlloc(self.allocator, path);
    }
};

fn extractPrefix(text: []const u8, line: usize, character: usize) []const u8 {
    var offset: usize = 0;
    var current_line: usize = 0;
    while (offset < text.len and current_line < line) {
        if (text[offset] == '\n') current_line += 1;
        offset += 1;
    }
    var cursor = offset;
    var consumed: usize = 0;
    while (cursor < text.len and consumed < character) {
        if (text[cursor] == '\n') break;
        cursor += 1;
        consumed += 1;
    }
    var start = cursor;
    while (start > offset) {
        const ch = text[start - 1];
        if (symbols.isIdentifierChar(ch)) {
            start -= 1;
        } else break;
    }
    return text[start..cursor];
}

fn readLine(allocator: Allocator, reader: *std.Io.Reader) ![]u8 {
    const line = try reader.takeDelimiterExclusive('\n');
    if (line.len > MAX_LINE) return error.ProtocolError;
    return try allocator.dupe(u8, line);
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

fn readWholeFile(allocator: Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, MAX_FILE);
}
