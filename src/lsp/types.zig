const std = @import("std");
const Allocator = std.mem.Allocator;
const json = @import("json.zig");

pub const ClientRequest = struct {
    jsonrpc: []const u8,
    id: ?RequestId = null,
    method: []const u8,
    payload: ?ClientRequestPayload,

    pub fn jsonParse(
        allocator: Allocator,
        source: anytype,
        options: std.json.ParseOptions,
    ) std.json.ParseError(@TypeOf(source.*))!ClientRequest {
        const ClientRequestPoly = struct {
            jsonrpc: []const u8,
            id: ?RequestId = null,
            method: []const u8,
            params: std.json.Value,
        };

        const poly: ClientRequestPoly = try std.json.innerParse(ClientRequestPoly, allocator, source, options);

        inline for (std.meta.fields(ClientRequestPayload)) |field| {
            if (std.mem.eql(u8, field.name, poly.method)) {
                const params = if (field.type == void) ({}) else try std.json.innerParseFromValue(field.type, allocator, poly.params, options);
                return .{
                    .jsonrpc = poly.jsonrpc,
                    .id = poly.id,
                    .method = poly.method,
                    .payload = @unionInit(ClientRequestPayload, field.name, params),
                };
            }
        }

        return .{
            .jsonrpc = poly.jsonrpc,
            .id = poly.id,
            .method = poly.method,
            .payload = null,
        };
    }
};

pub const RequestId = union(enum) {
    string: []const u8,
    integer: u32,

    fn fromJson(allocator: Allocator, value: std.json.Value) !RequestId {
        return switch (value) {
            .string => |slice| RequestId{ .string = try allocator.dupe(u8, slice) },
            .integer => |num| RequestId{ .integer = @intCast(if (num < 0) 0 else num) },
            else => return error.UnexpectedToken,
        };
    }

    pub fn jsonParse(
        allocator: Allocator,
        source: anytype,
        options: std.json.ParseOptions,
    ) std.json.ParseError(@TypeOf(source.*))!RequestId {
        const value = try std.json.innerParse(std.json.Value, allocator, source, options);
        return try fromJson(allocator, value);
    }

    pub fn jsonParseFromValue(
        allocator: Allocator,
        source: std.json.Value,
        _: std.json.ParseOptions,
    ) std.json.ParseFromValueError!RequestId {
        return try fromJson(allocator, source);
    }

    fn deinit(self: *RequestId, allocator: Allocator) void {
        if (self.* == .string) allocator.free(self.string);
        self.* = .none;
    }

    pub fn writeJson(self: RequestId, writer: *std.Io.Writer) !void {
        switch (self) {
            .integer => |value| try writer.print("{d}", .{value}),
            .string => |value| try json.writeJsonString(writer, value),
        }
    }
};

pub const ClientRequestPayload = union(enum) {
    initialize: InitializeParams,
    initialized: InitializedParams,
    shutdown,
    exit,
    @"textDocument/didOpen": DidOpenTextDocumentParams,
    @"textDocument/didChange": DidChangeTextDocumentParams,
    @"textDocument/didClose": DidCloseTextDocumentParams,
    @"textDocument/completion": CompletionParams,
    @"workspace/didChangeConfiguration": DidChangeConfigurationParams,
    @"workspace/didChangeWatchedFiles": DidChangeWatchedFilesParams,
    @"$/cancelRequest",
};

pub const DocumentVersion = u32;

pub const InitializeParams = struct {
    rootUri: ?[]const u8 = null,
    rootPath: ?[]const u8 = null,
    workspaceFolders: ?[]const WorkspaceFolder = null,
    workDoneToken: ?ProgressToken = null,
};

pub const WorkspaceFolder = struct {
    uri: []const u8,
    name: []const u8,
};

pub const InitializedParams = struct {};

pub const DidOpenTextDocumentParams = struct {
    textDocument: TextDocumentItem,
};

pub const DidChangeTextDocumentParams = struct {
    textDocument: VersionedTextDocumentIdentifier,
    contentChanges: []const TextDocumentContentChangeEvent,
};

pub const DidCloseTextDocumentParams = struct {
    textDocument: TextDocumentIdentifier,
};

// {"params":{"textDocument":{"uri":"file:\\/\\/\\/home\\/lumorsunil\\/repos\\/runic\\/test.rn"},"position":{"line":7,"character":1},"context":{"triggerKind":1}},"jsonrpc":"2.0","method":"textDocument\\/completion","id":2}
pub const CompletionParams = struct {
    textDocument: TextDocumentIdentifier,
    position: Position,
    workDoneToken: ?ProgressToken = null,
    partialResultToken: ?ProgressToken = null,
    context: ?CompletionContext = null,
};

pub const ProgressToken = IntegerOrString;

pub const DidChangeConfigurationParams = struct {
    settings: std.json.Value,
};

pub const DidChangeWatchedFilesParams = struct {
    changes: []const FileEvent,
};

pub const FileEvent = struct {
    uri: []const u8,
    type: FileChangeType,
};

pub const FileChangeType = enum(u32) {
    created = 1,
    changed = 2,
    deleted = 3,
};

pub const CompletionContext = struct {
    triggerKind: CompletionTriggerKind,
    triggerCharacter: ?[]const u8 = null,
};

pub const CompletionTriggerKind = enum(u32) {
    invoked = 1,
    triggerCharacter = 2,
    triggerForIncompleteCompletions = 3,
};

pub const IntegerOrString = union(enum) {
    integer: u32,
    string: []const u8,

    pub fn jsonParse(
        allocator: Allocator,
        source: anytype,
        options: std.json.ParseOptions,
    ) std.json.ParseError(@TypeOf(source.*))!IntegerOrString {
        const value: std.json.Value = try std.json.innerParse(std.json.Value, allocator, source, options);

        return switch (value) {
            .integer => |int| .{ .integer = int },
            .string => |str| .{ .string = str },
            else => return error.UnexpectedToken,
        };
    }

    pub fn jsonParseFromValue(
        _: Allocator,
        source: std.json.Value,
        _: std.json.ParseOptions,
    ) std.json.ParseFromValueError!IntegerOrString {
        return switch (source) {
            .integer => |int| .{ .integer = @intCast(if (int < 0) 0 else int) },
            .string => |str| .{ .string = str },
            else => return error.UnexpectedToken,
        };
    }
};

pub const TextDocumentItem = struct {
    uri: []const u8,
    languageId: []const u8,
    version: ?u32 = null,
    text: []const u8,
};

pub const TextDocumentIdentifier = struct {
    uri: []const u8,
};

pub const VersionedTextDocumentIdentifier = struct {
    version: ?DocumentVersion = null,
    uri: []const u8,
};

pub const TextDocumentContentChangeEvent = union(enum) {
    text: []const u8,
    range: Range_,

    const Range_ = struct {
        range: Range,
        rangeLength: ?u32 = null,
        text: []const u8,
    };

    pub fn jsonParse(
        allocator: Allocator,
        source: anytype,
        options: std.json.ParseOptions,
    ) std.json.ParseError(@TypeOf(source.*))!TextDocumentContentChangeEvent {
        const value: std.json.Value = try std.json.innerParse(std.json.Value, allocator, source, options);
        return try jsonParseFromValue(allocator, value, options);
    }

    pub fn jsonParseFromValue(
        allocator: Allocator,
        source: std.json.Value,
        options: std.json.ParseOptions,
    ) std.json.ParseFromValueError!TextDocumentContentChangeEvent {
        const obj: std.json.ObjectMap = source.object;

        if (obj.get("range")) |_| {
            const inner = try std.json.innerParseFromValue(Range_, allocator, .{ .object = obj }, options);
            return .{ .range = inner };
        }

        if (obj.get("text")) |textValue| {
            const text = try std.json.innerParseFromValue([]const u8, allocator, textValue, options);
            return .{ .text = text };
        }

        return error.UnexpectedToken;
    }
};

pub const Range = struct {
    start: Position,
    end: Position,
};

pub const Position = struct {
    line: u32,
    character: u32,

    pub fn findIndex(self: Position, source: []const u8) ?u32 {
        var column: u32 = 0;
        var line: u32 = 0;
        var i: u32 = 0;

        while (column != self.character or line != self.line) : (i += 1) {
            switch (source[i]) {
                '\\' => {
                    if (i < source.len - 1) {
                        const next = source[i + 1];

                        switch (next) {
                            'n' => {
                                line += 1;
                                column = 0;
                            },
                            else => column += 1,
                        }
                    } else {
                        column += 1;
                    }
                },
                '\r' => {
                    if (i < source.len - 1 and source[i + 1] == '\n') {
                        i += 1;
                    }

                    line += 1;
                    column = 0;
                },
                '\n' => {
                    if (i < source.len - 1 and source[i + 1] == '\r') {
                        i += 1;
                    }

                    line += 1;
                    column = 0;
                },
                else => column += 1,
            }

            if (column > self.character and line > self.line) return null;
        }

        if (column == self.character and line == self.line) {
            return i;
        }

        return null;
    }
};
