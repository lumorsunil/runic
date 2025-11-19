const std = @import("std");
const workspace_mod = @import("workspace.zig");
const symbols = @import("symbols.zig");
const completion = @import("completion.zig");

const Allocator = std.mem.Allocator;

pub const Server = struct {
    allocator: Allocator,
    stdin_file: std.fs.File,
    stdout_file: std.fs.File,
    stderr_file: std.fs.File,
    reader_buffer: [4096]u8 = undefined,
    workspace: workspace_mod.Workspace,
    documents: DocumentStore,
    initialized: bool = false,
    shutting_down: bool = false,
    log_enabled: bool = false,

    pub fn init(
        allocator: Allocator,
        stdin_file: std.fs.File,
        stdout_file: std.fs.File,
        stderr_file: std.fs.File,
    ) !Server {
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
            .workspace = workspace_mod.Workspace.init(allocator),
            .documents = DocumentStore.init(allocator),
            .log_enabled = log_enabled,
        };
    }

    pub fn deinit(self: *Server) void {
        self.workspace.deinit();
        self.documents.deinit();
    }

    fn getReader(self: *Server) *std.Io.Reader {
        return &self.stdin_file.reader(&self.reader_buffer).interface;
    }

    pub fn run(self: *Server) !void {
        while (true) {
            const payload = self.readMessage() catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };
            defer self.allocator.free(payload);
            const continue_loop = try self.handleEnvelope(payload);
            if (!continue_loop) break;
        }
    }

    fn handleEnvelope(self: *Server, payload: []u8) !bool {
        const parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, payload, .{});
        defer parsed.deinit();

        const root = parsed.value;
        const obj = switch (root) {
            .object => |o| o,
            else => return true,
        };

        const method_value = obj.get("method") orelse return true;
        const method = try expectString(method_value, "method");
        const params = obj.get("params");

        var id = RequestId{ .none = {} };
        if (obj.get("id")) |value| {
            id = try RequestId.fromJson(self.allocator, value);
        }
        defer id.deinit(self.allocator);

        if (std.mem.eql(u8, method, "initialize")) {
            if (!id.present()) return true;
            try self.handleInitialize(id, params);
            return true;
        }
        if (std.mem.eql(u8, method, "initialized")) {
            try self.log("client signaled initialized", .{});
            return true;
        }
        if (std.mem.eql(u8, method, "shutdown")) {
            self.shutting_down = true;
            if (id.present()) try self.sendNullResult(id);
            return true;
        }
        if (std.mem.eql(u8, method, "exit")) {
            return false;
        }
        if (std.mem.eql(u8, method, "textDocument/didOpen")) {
            try self.handleDidOpen(params);
            return true;
        }
        if (std.mem.eql(u8, method, "textDocument/didChange")) {
            try self.handleDidChange(params);
            return true;
        }
        if (std.mem.eql(u8, method, "textDocument/didClose")) {
            try self.handleDidClose(params);
            return true;
        }
        if (std.mem.eql(u8, method, "textDocument/completion")) {
            if (!id.present()) return true;
            try self.handleCompletion(id, params);
            return true;
        }
        if (std.mem.eql(u8, method, "workspace/didChangeConfiguration") or
            std.mem.eql(u8, method, "workspace/didChangeWatchedFiles"))
        {
            try self.log("ignoring optional workspace notification: {s}", .{method});
            return true;
        }
        if (std.mem.eql(u8, method, "$/cancelRequest")) {
            try self.log("client canceled request", .{});
            return true;
        }

        if (id.present()) {
            try self.sendError(id, -32601, "method not found");
        } else {
            try self.log("dropping notification for unknown method: {s}", .{method});
        }
        return true;
    }

    fn handleInitialize(self: *Server, id: RequestId, params_value: ?std.json.Value) !void {
        if (self.initialized) {
            try self.sendError(id, -32600, "Server already initialized");
            return;
        }

        const params = try expectObject(params_value orelse std.json.Value{ .object = .init(self.allocator) }, "initialize params");
        var roots = std.ArrayList([]const u8){};
        defer {
            for (roots.items) |root| self.allocator.free(root);
            roots.deinit(self.allocator);
        }

        if (params.get("rootUri")) |root_uri_value| {
            if (root_uri_value == .string and root_uri_value.string.len > 0) {
                try roots.append(self.allocator, try self.resolveUriPath(root_uri_value.string));
            }
        }
        if (roots.items.len == 0) {
            if (params.get("rootPath")) |root_path_value| {
                if (root_path_value == .string and root_path_value.string.len > 0) {
                    try roots.append(self.allocator, try self.absolutePath(root_path_value.string));
                }
            }
        }
        if (params.get("workspaceFolders")) |folders_value| {
            if (folders_value == .array) {
                for (folders_value.array.items) |entry| {
                    const folder = try expectObject(entry, "workspace folder");
                    const uri_value = folder.get("uri") orelse continue;
                    const uri = try expectString(uri_value, "workspace folder uri");
                    try roots.append(self.allocator, try self.resolveUriPath(uri));
                }
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

    fn handleDidOpen(self: *Server, params_value: ?std.json.Value) !void {
        const params = try expectObject(params_value orelse return, "didOpen params");
        const text_doc = try expectObjectField(params, "textDocument");
        const uri = try expectStringField(text_doc, "uri");
        const text = try expectStringField(text_doc, "text");
        const version = parseOptionalInt(text_doc.get("version")) orelse 0;
        const path = try self.resolveUriPath(uri);
        var release_path = true;
        errdefer if (release_path) self.allocator.free(path);
        try self.documents.openOrReplace(uri, path, text, version, &self.workspace);
        release_path = false;
    }

    fn handleDidChange(self: *Server, params_value: ?std.json.Value) !void {
        const params = try expectObject(params_value orelse return, "didChange params");
        const text_doc = try expectObjectField(params, "textDocument");
        const uri = try expectStringField(text_doc, "uri");
        const version = parseOptionalInt(text_doc.get("version")) orelse 0;
        const changes = try expectArrayField(params, "contentChanges");
        if (changes.items.len == 0) return;
        const change_value = changes.items[changes.items.len - 1];
        const change = try expectObject(change_value, "content change");
        if (change.get("range")) |_| {
            try self.log("skipping incremental update for {s}; send full text", .{uri});
            return;
        }
        const text = try expectStringField(change, "text");
        _ = try self.documents.update(uri, text, version, &self.workspace);
    }

    fn handleDidClose(self: *Server, params_value: ?std.json.Value) !void {
        const params = try expectObject(params_value orelse return, "didClose params");
        const text_doc = try expectObjectField(params, "textDocument");
        const uri = try expectStringField(text_doc, "uri");
        self.documents.close(uri);
    }

    fn handleCompletion(self: *Server, id: RequestId, params_value: ?std.json.Value) !void {
        const params = try expectObject(params_value orelse return, "completion params");
        const text_doc = try expectObjectField(params, "textDocument");
        const uri = try expectStringField(text_doc, "uri");
        const position = try expectObjectField(params, "position");
        const line_val = parseRequiredInt(position.get("line"));
        const char_val = parseRequiredInt(position.get("character"));

        const doc = self.documents.get(uri);
        var owned_path: ?[]u8 = null;
        var fallback_text: ?[]u8 = null;
        defer if (owned_path) |path| self.allocator.free(path);
        defer if (fallback_text) |buffer| self.allocator.free(buffer);

        const text_slice: []const u8 = blk: {
            if (doc) |existing| break :blk existing.text;
            owned_path = try self.resolveUriPath(uri);
            fallback_text = try readWholeFile(self.allocator, owned_path.?);
            break :blk fallback_text.?;
        };

        const line_index: usize = if (line_val < 0) 0 else @as(usize, @intCast(line_val));
        const char_index: usize = if (char_val < 0) 0 else @as(usize, @intCast(char_val));
        const prefix = extractPrefix(text_slice, line_index, char_index);
        const doc_symbols = if (doc) |d| d.symbols.items else &[_]symbols.Symbol{};
        const workspace_symbols = self.workspace.symbolSlice();

        var matches = try completion.collectMatches(self.allocator, prefix, doc_symbols, workspace_symbols);
        defer matches.deinit();

        try self.sendCompletionResult(id, matches.items.items);
    }

    fn sendInitializeResult(self: *Server, id: RequestId) !void {
        var body = std.ArrayList(u8){};
        defer body.deinit(self.allocator);
        var writer = body.writer(self.allocator);
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try id.writeJson(&writer);
        try writer.writeAll(",\"result\":{\"capabilities\":{\"textDocumentSync\":{\"openClose\":true,\"change\":2}," ++
            "\"completionProvider\":{\"triggerCharacters\":[\".\",\":\"],\"resolveProvider\":false}}," ++
            "\"serverInfo\":{\"name\":\"runic-lsp\",\"version\":\"0.1\"}}}");
        try self.sendBody(body.items);
    }

    fn sendCompletionResult(self: *Server, id: RequestId, items: []const completion.Match) !void {
        var body = std.ArrayList(u8){};
        defer body.deinit(self.allocator);
        var writer = body.writer(self.allocator);
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try id.writeJson(&writer);
        try writer.writeAll(",\"result\":{\"isIncomplete\":false,\"items\":[");
        var first = true;
        for (items) |entry| {
            if (!first) try writer.writeByte(',');
            first = false;
            try writeCompletionItem(&writer, entry.symbol);
        }
        try writer.writeAll("]}}");
        try self.sendBody(body.items);
    }

    fn sendNullResult(self: *Server, id: RequestId) !void {
        var body = std.ArrayList(u8){};
        defer body.deinit(self.allocator);
        var writer = body.writer(self.allocator);
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try id.writeJson(&writer);
        try writer.writeAll(",\"result\":null}");
        try self.sendBody(body.items);
    }

    fn sendError(self: *Server, id: RequestId, code: i64, message: []const u8) !void {
        var body = std.ArrayList(u8){};
        defer body.deinit(self.allocator);
        var writer = body.writer(self.allocator);
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try id.writeJson(&writer);
        try writer.writeAll(",\"error\":{\"code\":");
        try writer.print("{d}", .{code});
        try writer.writeAll(",\"message\":");
        try writeJsonString(&writer, message);
        try writer.writeAll("}}");
        try self.sendBody(body.items);
    }

    fn writeCompletionItem(writer: anytype, symbol: *const symbols.Symbol) !void {
        try writer.writeAll("{\"label\":");
        try writeJsonString(writer, symbol.name);
        try writer.writeAll(",\"kind\":");
        try writer.print("{d}", .{completionKind(symbol.kind)});
        if (symbol.detail.len != 0) {
            try writer.writeAll(",\"detail\":");
            try writeJsonString(writer, symbol.detail);
        }
        if (symbol.documentation.len != 0) {
            try writer.writeAll(",\"documentation\":");
            try writeJsonString(writer, symbol.documentation);
        }
        try writer.writeAll(",\"sortText\":");
        try writeSortKey(writer, symbol.name);
        try writer.writeByte('}');
    }

    fn completionKind(kind: symbols.SymbolKind) u8 {
        return switch (kind) {
            .function => 3,
            .variable => 6,
            .module => 9,
        };
    }

    fn writeSortKey(writer: anytype, label: []const u8) !void {
        try writer.writeByte('"');
        for (label) |ch| {
            const lower = std.ascii.toLower(ch);
            try writer.writeByte(lower);
        }
        try writer.writeByte('"');
    }

    fn readMessage(self: *Server) ![]u8 {
        var reader = self.stdin_file.reader(&self.reader_buffer);
        const content_length = try self.readHeaders(&reader.interface);
        var buffer = try self.allocator.alloc(u8, content_length);
        var filled: usize = 0;
        while (filled < buffer.len) {
            const read_bytes = try reader.read(buffer[filled..]);
            if (read_bytes == 0) return error.EndOfStream;
            filled += read_bytes;
        }
        return buffer;
    }

    fn readHeaders(self: *Server, reader: *std.Io.Reader) !usize {
        var content_length: ?usize = null;
        while (true) {
            const line = try readLine(self.allocator, reader);
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

    fn sendBody(self: *Server, body: []const u8) !void {
        var writer = self.stdout_file.writer(&.{});
        try writer.interface.print("Content-Length: {d}\r\n\r\n", .{body.len});
        try writer.interface.writeAll(body);
    }

    fn log(self: *Server, comptime fmt: []const u8, args: anytype) !void {
        if (!self.log_enabled) return;
        var writer = self.stderr_file.writer(&.{});
        try writer.interface.print("[runic-lsp] ", .{});
        try writer.interface.print(fmt, args);
        try writer.interface.writeByte('\n');
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

const DocumentStore = struct {
    allocator: Allocator,
    map: std.StringHashMap(Document),

    fn init(allocator: Allocator) DocumentStore {
        return .{ .allocator = allocator, .map = .init(allocator) };
    }

    fn deinit(self: *DocumentStore) void {
        var it = self.map.iterator();
        while (it.next()) |entry| {
            self.allocator.free(@constCast(entry.key_ptr.*));
            entry.value_ptr.deinit(self.allocator);
        }
        self.map.deinit();
    }

    fn openOrReplace(
        self: *DocumentStore,
        uri: []const u8,
        path: []u8,
        text: []const u8,
        version: i64,
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
        try document.rebuildSymbols(self.allocator, workspace.describePath(document.path));
        try self.map.put(key, document);
    }

    fn update(
        self: *DocumentStore,
        uri: []const u8,
        text: []const u8,
        version: i64,
        workspace: *workspace_mod.Workspace,
    ) !bool {
        if (self.map.getPtr(uri)) |doc| {
            try doc.setText(self.allocator, text, version);
            try doc.rebuildSymbols(self.allocator, workspace.describePath(doc.path));
            return true;
        }
        return false;
    }

    fn close(self: *DocumentStore, uri: []const u8) void {
        if (self.map.fetchRemove(uri)) |removed| {
            self.allocator.free(@constCast(removed.key));
            var document = removed.value;
            document.deinit(self.allocator);
        }
    }

    fn get(self: *DocumentStore, uri: []const u8) ?*Document {
        return self.map.getPtr(uri);
    }
};

const Document = struct {
    path: []u8,
    text: []u8,
    version: i64,
    symbols: std.ArrayList(symbols.Symbol),

    fn init(allocator: Allocator, path: []u8, text: []const u8, version: i64) !Document {
        return .{
            .path = path,
            .text = try allocator.dupe(u8, text),
            .version = version,
            .symbols = .{},
        };
    }

    fn deinit(self: *Document, allocator: Allocator) void {
        allocator.free(self.path);
        allocator.free(self.text);
        for (self.symbols.items) |*entry| entry.deinit(allocator);
        self.symbols.deinit(allocator);
        self.* = undefined;
    }

    fn setText(self: *Document, allocator: Allocator, text: []const u8, version: i64) !void {
        allocator.free(self.text);
        self.text = try allocator.dupe(u8, text);
        self.version = version;
    }

    fn rebuildSymbols(self: *Document, allocator: Allocator, detail: []const u8) !void {
        for (self.symbols.items) |*entry| entry.deinit(allocator);
        self.symbols.clearRetainingCapacity();
        try symbols.collectSymbols(allocator, detail, self.text, &self.symbols);
    }
};

const RequestId = union(enum) {
    none,
    string: []const u8,
    integer: i64,

    fn present(self: RequestId) bool {
        return switch (self) {
            .none => false,
            else => true,
        };
    }

    fn fromJson(allocator: Allocator, value: std.json.Value) !RequestId {
        return switch (value) {
            .string => |slice| RequestId{ .string = try allocator.dupe(u8, slice) },
            .integer => |num| RequestId{ .integer = num },
            else => RequestId{ .none = {} },
        };
    }

    fn deinit(self: *RequestId, allocator: Allocator) void {
        if (self.* == .string) allocator.free(self.string);
        self.* = .none;
    }

    fn writeJson(self: RequestId, writer: anytype) !void {
        switch (self) {
            .none => try writer.writeAll("null"),
            .integer => |value| try writer.print("{d}", .{value}),
            .string => |value| try writeJsonString(writer, value),
        }
    }
};

fn expectObject(value: std.json.Value, context: []const u8) !std.json.ObjectMap {
    _ = context;
    return switch (value) {
        .object => |obj| obj,
        else => error.InvalidRequest,
    };
}

fn expectObjectField(obj: std.json.ObjectMap, name: []const u8) !std.json.ObjectMap {
    const value = obj.get(name) orelse return error.InvalidRequest;
    return try expectObject(value, name);
}

fn expectArrayField(obj: std.json.ObjectMap, name: []const u8) !std.json.Array {
    const value = obj.get(name) orelse return error.InvalidRequest;
    return switch (value) {
        .array => |arr| arr,
        else => error.InvalidRequest,
    };
}

fn expectString(value: std.json.Value, _: []const u8) ![]const u8 {
    return switch (value) {
        .string => |slice| slice,
        else => error.InvalidRequest,
    };
}

fn expectStringField(obj: std.json.ObjectMap, name: []const u8) ![]const u8 {
    const value = obj.get(name) orelse return error.InvalidRequest;
    return try expectString(value, name);
}

fn parseOptionalInt(value_opt: ?std.json.Value) ?i64 {
    if (value_opt) |value| {
        return parseJsonInt(value);
    }
    return null;
}

fn parseRequiredInt(value_opt: ?std.json.Value) i64 {
    return parseJsonInt(value_opt orelse return 0) orelse 0;
}

fn parseJsonInt(value: std.json.Value) ?i64 {
    return switch (value) {
        .integer => |num| num,
        .float => |num| @as(i64, @intFromFloat(num)),
        else => null,
    };
}

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

fn writeJsonString(writer: anytype, text: []const u8) !void {
    try writer.writeByte('"');
    for (text) |ch| switch (ch) {
        '"' => try writer.writeAll("\\\""),
        '\\' => try writer.writeAll("\\\\"),
        '\n' => try writer.writeAll("\\n"),
        '\r' => try writer.writeAll("\\r"),
        '\t' => try writer.writeAll("\\t"),
        0...8, 11...12, 14...31 => try writer.print("\\u{X:0>4}", .{@as(u16, @intCast(ch))}),
        else => try writer.writeByte(ch),
    };
    try writer.writeByte('"');
}

fn readLine(allocator: Allocator, reader: *std.Io.Reader) ![]u8 {
    const line = try reader.takeDelimiterExclusive('\n');
    if (line.len > 16 * 1024) return error.ProtocolError;
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
    return try file.readToEndAlloc(allocator, 4 * 1024 * 1024);
}
