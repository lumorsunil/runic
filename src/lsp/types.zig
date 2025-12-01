const std = @import("std");
const Allocator = std.mem.Allocator;
const json = @import("json.zig");
const symbols = @import("symbols.zig");
const diag = @import("diagnostics.zig");
const runic = @import("runic");

pub const Uri = []const u8;

pub const DocumentUri = []const u8;

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
            params: ?std.json.Value = null,
        };

        const poly: ClientRequestPoly = try std.json.innerParse(ClientRequestPoly, allocator, source, options);

        inline for (std.meta.fields(ClientRequestPayload)) |field| {
            if (std.mem.eql(u8, field.name, poly.method)) {
                const params = if (field.type == void) ({}) else if (poly.params) |ps| try std.json.innerParseFromValue(field.type, allocator, ps, options) else undefined;
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

    pub fn jsonStringify(self: @This(), stringify: *std.json.Stringify) std.json.Stringify.Error!void {
        switch (self) {
            .string => |s| try stringify.write(s),
            .integer => |i| try stringify.write(i),
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
    uri: DocumentUri,
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
    uri: DocumentUri,
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
    uri: DocumentUri,
    languageId: []const u8,
    version: ?u32 = null,
    text: []const u8,
};

pub const TextDocumentIdentifier = struct {
    uri: DocumentUri,
};

pub const VersionedTextDocumentIdentifier = struct {
    version: ?DocumentVersion = null,
    uri: DocumentUri,
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

    pub fn fromSpan(span: runic.ast.Span) @This() {
        return .{
            .start = .fromLocation(span.start),
            .end = .fromLocation(span.end),
        };
    }
};

pub const Position = struct {
    line: u32,
    character: u32,

    pub fn fromLocation(location: runic.token.Location) @This() {
        return .{
            .line = @intCast(location.line - 1),
            .character = @intCast(location.column - 1),
        };
    }

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

pub const InitializeResult = struct {
    /// The capabilities the language server provides.
    capabilities: ServerCapabilities,
    /// Information about the server.
    ///
    /// @since 3.15.0
    serverInfo: ?ServerInfo = null,
};

pub fn Either(comptime types: anytype) type {
    var fields: []const std.builtin.Type.UnionField = &.{};
    var tags: []const std.builtin.Type.EnumField = &.{};

    for (std.meta.fields(@TypeOf(types))) |f| {
        const T = @field(types, f.name);
        var tName: []const u8 = @typeName(T);
        const name: []const u8 = brk: {
            if (std.mem.startsWith(u8, tName, "types.")) {
                tName = tName[6..];
            }

            if (tName.len == 0) @compileError("type name has 0 length");
            const c = std.ascii.toLower(tName[0]);
            if (tName.len == 1) break :brk c;
            break :brk [_]u8{c} ++ tName[1..];
        };

        fields = fields ++ [_]std.builtin.Type.UnionField{
            std.builtin.Type.UnionField{
                .type = @field(types, f.name),
                .name = @ptrCast(name),
                .alignment = @alignOf(T),
            },
        };

        const i = tags.len;
        tags = tags ++ [_]std.builtin.Type.EnumField{
            .{
                .name = @ptrCast(name),
                .value = i,
            },
        };
    }

    const Enum = @Type(.{
        .@"enum" = .{
            .tag_type = usize,
            .decls = &.{},
            .fields = tags,
            .is_exhaustive = true,
        },
    });

    const Payload = @Type(.{
        .@"union" = .{
            .fields = fields,
            .decls = &.{},
            .layout = .auto,
            .tag_type = Enum,
        },
    });

    return struct {
        payload: Payload,

        pub fn jsonParse(
            allocator: Allocator,
            source: anytype,
            options: std.json.ParseOptions,
        ) std.json.ParseError(@TypeOf(source.*))!@This() {
            inline for (std.meta.fields(Payload)) |field| {
                const maybePayload = std.json.innerParse(field.type, allocator, source, options);
                if (maybePayload) |payload| {
                    return @unionInit(@This(), field.name, payload);
                }
            }

            return error.UnexpectedToken;
        }

        pub fn jsonParseFromValue(
            allocator: Allocator,
            source: std.json.Value,
            options: std.json.ParseOptions,
        ) std.json.ParseFromValueError!@This() {
            inline for (std.meta.fields(Payload)) |field| {
                const maybePayload = std.json.innerParseFromValue(field.type, allocator, source, options);
                if (maybePayload) |payload| {
                    return @unionInit(@This(), field.name, payload);
                }
            }

            return error.UnexpectedToken;
        }

        pub fn jsonStringify(self: @This(), stringify: *std.json.Stringify) std.json.Stringify.Error!void {
            switch (self.payload) {
                inline else => |p| try stringify.write(p),
            }
        }
    };
}

pub const ServerCapabilities = struct {
    /// The position encoding the server picked from the encodings offered by the client via the client capability `general.positionEncodings`.
    ///
    /// If the client didn't provide any position encodings the only valid value that a server can return is 'utf-16'.
    ///
    /// If omitted it defaults to 'utf-16'.
    ///
    /// @since 3.17.0
    positionEncoding: ?PositionEncodingKind = null,

    /// Defines how text documents are synced. Is either a detailed structure defining each notification or for backwards compatibility the TextDocumentSyncKind number. If omitted it defaults to `TextDocumentSyncKind.None`.
    textDocumentSync: ?Either(.{ TextDocumentSyncOptions, TextDocumentSyncKind }) = null,

    /// Defines how notebook documents are synced.
    ///
    /// @since 3.17.0
    notebookDocumentSync: ?Either(.{ NotebookDocumentSyncOptions, NotebookDocumentSyncRegistrationOptions }) = null,

    /// The server provides completion support.
    completionProvider: ?CompletionOptions = null,

    /// The server provides hover support.
    hoverProvider: ?Either(.{ bool, HoverOptions }) = null,

    /// The server provides signature help support.
    signatureHelpProvider: ?SignatureHelpOptions = null,

    /// The server provides go to declaration support.
    ///
    /// @since 3.14.0
    declarationProvider: ?Either(.{ bool, DeclarationOptions, DeclarationRegistrationOptions }) = null,

    /// The server provides goto definition support.
    definitionProvider: ?Either(.{ bool, DefinitionOptions }) = null,

    /// The server provides goto type definition support.
    ///
    /// @since 3.6.0
    typeDefinitionProvider: ?Either(.{ bool, TypeDefinitionOptions, TypeDefinitionRegistrationOptions }) = null,

    /// The server provides goto implementation support.
    ///
    /// @since 3.6.0
    implementationProvider: ?Either(.{ bool, ImplementationOptions, ImplementationRegistrationOptions }) = null,

    /// The server provides find references support.
    referencesProvider: ?Either(.{ bool, ReferenceOptions }) = null,

    /// The server provides document highlight support.
    documentHighlightProvider: ?Either(.{ bool, DocumentHighlightOptions }) = null,

    /// The server provides document symbol support.
    documentSymbolProvider: ?Either(.{ bool, DocumentSymbolOptions }) = null,

    /// The server provides code actions. The `CodeActionOptions` return type is only valid if the client signals code action literal support via the property `textDocument.codeAction.codeActionLiteralSupport`.
    codeActionProvider: ?Either(.{ bool, CodeActionOptions }) = null,

    /// The server provides code lens.
    codeLensProvider: ?CodeLensOptions = null,

    /// The server provides document link support.
    documentLinkProvider: ?DocumentLinkOptions = null,

    /// The server provides color provider support.
    ///
    /// @since 3.6.0
    colorProvider: ?Either(.{ bool, DocumentColorOptions, DocumentColorRegistrationOptions }) = null,

    /// The server provides document formatting.
    documentFormattingProvider: ?Either(.{ bool, DocumentFormattingOptions }) = null,

    /// The server provides document range formatting.
    documentRangeFormattingProvider: ?Either(.{ bool, DocumentRangeFormattingOptions }) = null,

    /// The server provides document formatting on typing.
    documentOnTypeFormattingProvider: ?DocumentOnTypeFormattingOptions = null,

    /// The server provides rename support. RenameOptions may only be specified if the client states that it supports `prepareSupport` in its initial `initialize` request.
    renameProvider: ?Either(.{ bool, RenameOptions }) = null,

    /// The server provides folding provider support.
    ///
    /// @since 3.10.0
    foldingRangeProvider: ?Either(.{ bool, FoldingRangeOptions, FoldingRangeRegistrationOptions }) = null,

    /// The server provides execute command support.
    executeCommandProvider: ?ExecuteCommandOptions = null,

    /// The server provides selection range support.
    ///
    /// @since 3.15.0
    selectionRangeProvider: ?Either(.{ bool, SelectionRangeOptions, SelectionRangeRegistrationOptions }) = null,

    /// The server provides linked editing range support.
    ///
    /// @since 3.16.0
    linkedEditingRangeProvider: ?Either(.{ bool, LinkedEditingRangeOptions, LinkedEditingRangeRegistrationOptions }) = null,

    /// The server provides call hierarchy support.
    ///
    /// @since 3.16.0
    callHierarchyProvider: ?Either(.{ bool, CallHierarchyOptions, CallHierarchyRegistrationOptions }) = null,

    /// The server provides semantic tokens support.
    ///
    /// @since 3.16.0
    semanticTokensProvider: ?Either(.{ SemanticTokensOptions, SemanticTokensRegistrationOptions }) = null,

    /// Whether server provides moniker support.
    ///
    /// @since 3.16.0
    monikerProvider: ?Either(.{ bool, MonikerOptions, MonikerRegistrationOptions }) = null,

    /// The server provides type hierarchy support.
    ///
    /// @since 3.17.0
    typeHierarchyProvider: ?Either(.{ bool, TypeHierarchyOptions, TypeHierarchyRegistrationOptions }) = null,

    /// The server provides inline values.
    ///
    /// @since 3.17.0
    inlineValueProvider: ?Either(.{ bool, InlineValueOptions, InlineValueRegistrationOptions }) = null,

    /// The server provides inlay hints.
    ///
    /// @since 3.17.0
    inlayHintProvider: ?Either(.{ bool, InlayHintOptions, InlayHintRegistrationOptions }) = null,

    /// The server has support for pull model diagnostics.
    ///
    /// @since 3.17.0
    diagnosticProvider: ?Either(.{ DiagnosticOptions, DiagnosticRegistrationOptions }) = null,

    /// The server provides workspace symbol support.
    workspaceSymbolProvider: ?Either(.{ bool, WorkspaceSymbolOptions }) = null,

    /// Workspace specific server capabilities
    workspace: ?struct {
        /// The server supports workspace folder.
        ///
        /// @since 3.6.0
        workspaceFolders: ?WorkspaceFoldersServerCapabilities = null,

        /// The server is interested in file notifications/requests.
        ///
        /// @since 3.16.0
        fileOperations: ?struct {
            /// The server is interested in receiving didCreateFiles notifications.
            didCreate: ?FileOperationRegistrationOptions = null,

            /// The server is interested in receiving willCreateFiles requests.
            willCreate: ?FileOperationRegistrationOptions = null,

            /// The server is interested in receiving didRenameFiles notifications.
            didRename: ?FileOperationRegistrationOptions = null,

            /// The server is interested in receiving willRenameFiles requests.
            willRename: ?FileOperationRegistrationOptions = null,

            /// The server is interested in receiving didDeleteFiles file notifications.
            didDelete: ?FileOperationRegistrationOptions = null,

            /// The server is interested in receiving willDeleteFiles file requests.
            willDelete: ?FileOperationRegistrationOptions = null,
        } = null,
    } = null,

    /// Experimental server capabilities.
    experimental: ?std.json.Value = null,
};

/// A type indicating how positions are encoded, specifically what column offsets mean. Either `"utf-8"`, `"utf-16"` or `"utf-32"`.
pub const PositionEncodingKind = []const u8;

pub const TextDocumentSyncOptions = struct {
    /// Open and close notifications are sent to the server. If omitted open close notifications should not be sent.
    openClose: ?bool = null,

    /// Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full and TextDocumentSyncKind.Incremental. If omitted it defaults to TextDocumentSyncKind.None.
    change: ?TextDocumentSyncKind = null,
};

/// Defines how the host (editor) should sync document changes to the language server.
pub const TextDocumentSyncKind = enum(u32) {
    /// Documents should not be synced at all.
    none = 0,
    /// Documents are synced by always sending the full content of the document.
    full = 1,
    /// Documents are synced by sending the full content on open. After that only incremental updates to the document are sent.
    incremental = 2,
};

/// Options specific to a notebook plus its cells to be synced to the server.
///
/// If a selector provides a notebook document filter but no cell selector all cells of a matching notebook document will be synced.
///
/// If a selector provides no notebook document filter but only a cell selector all notebook documents that contain at least one matching cell will be synced.
///
/// @since 3.17.0
pub const NotebookDocumentSyncOptions = struct {
    /// The notebooks to be synced
    notebookSelector: []const NotebookSelector,

    /// Whether save notification should be forwarded to the server. Will only be honored if mode === `notebook`.
    save: ?bool = null,
};

pub const NotebookSelector = struct {
    /// The notebook to be synced. If a string value is provided it matches against the notebook type. '*' matches every notebook.
    notebook: ?Either(.{ []const u8, NotebookDocumentFilter }) = null,

    /// The cells of the matching notebook to be synced.
    cells: ?[]const struct { language: []const u8 } = null,
};

/// A notebook document filter denotes a notebook document by different properties.
///
/// @since 3.17.0
pub const NotebookDocumentFilter = struct {
    /// The type of the enclosing notebook.
    notebookType: []const u8,

    /// A Uri scheme, like `file` or `untitled`.
    scheme: ?[]const u8 = null,

    /// A glob pattern.
    pattern: ?[]const u8 = null,
};

/// Static registration options to be returned in the initialize request.
pub const StaticRegistrationOptions = struct {
    /// The id used to register the request. The id can be used to deregister the request again. See also Registration#id.
    id: ?[]const u8 = null,
};

/// Registration options specific to a notebook.
///
/// @since 3.17.0
pub const NotebookDocumentSyncRegistrationOptions = struct {
    /// The notebooks to be synced
    notebookSelector: []const NotebookSelector,

    /// Whether save notification should be forwarded to the server. Will only be honored if mode === `notebook`.
    save: ?bool = null,

    /// The id used to register the request. The id can be used to deregister the request again. See also Registration#id.
    id: ?[]const u8 = null,
};

/// Completion options.
pub const CompletionOptions = struct {
    /// The additional characters, beyond the defaults provided by the client (typically [a-zA-Z]), that should automatically trigger a completion request. For example `.` in JavaScript represents the beginning of an object property or method and is thus a good candidate for triggering a completion request.
    ///
    /// Most tools trigger a completion request automatically without explicitly requesting it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user starts to type an identifier. For example if the user types `c` in a JavaScript file code complete will automatically pop up present `console` besides others as a completion item. Characters that make up identifiers don't need to be listed here.
    triggerCharacters: ?[]const []const u8 = null,

    /// The list of all possible characters that commit a completion. This field can be used if clients don't support individual commit characters per completion item. See client capability `completion.completionItem.commitCharactersSupport`.
    ///
    /// If a server provides both `allCommitCharacters` and commit characters on an individual completion item the ones on the completion item win.
    ///
    /// @since 3.2.0
    allCommitCharacters: ?[]const []const u8 = null,

    /// The server provides support to resolve additional information for a completion item.
    resolveProvider: ?bool = null,

    /// The server supports the following `CompletionItem` specific capabilities.
    ///
    /// @since 3.17.0
    completionItem: ?struct {
        /// The server has support for completion item label details (see also `CompletionItemLabelDetails`) when receiving a completion item in a resolve call.
        ///
        /// @since 3.17.0
        labelDetailsSupport: ?bool = null,
    } = null,

    workDoneProgress: ?bool = null,
};

pub const HoverOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const SignatureHelpOptions = struct {
    /// The characters that trigger signature help automatically.
    ///
    triggerCharacters: ?[]const []const u8 = null,

    /// List of characters that re-trigger signature help.
    ///
    /// These trigger characters are only active when signature help is already showing. All trigger characters are also counted as re-trigger characters.
    ///
    /// @since 3.15.0
    retriggerCharacters: ?[]const []const u8 = null,

    workDoneProgress: ?bool = null,
};

pub const DeclarationOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const DeclarationRegistrationOptions = struct {
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
    id: ?[]const u8 = null,
};

pub const DocumentSelector = []const DocumentFilter;

pub const DocumentFilter = struct {
    /// A language id, like `typescript`.
    language: ?[]const u8 = null,

    /// A Uri scheme, like `file` or `untitled`.
    scheme: ?[]const u8 = null,

    /// A glob pattern, like `*.{ts,js}`.
    ///
    /// Glob patterns can have the following syntax:
    /// - `*` to match zero or more characters in a path segment
    /// - `?` to match on one character in a path segment
    /// - `**` to match any number of path segments, including none
    /// - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}`
    ///   matches all TypeScript and JavaScript files)
    /// - `[]` to declare a range of characters to match in a path segment
    ///   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
    /// - `[!...]` to negate a range of characters to match in a path segment
    ///   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but
    ///   not `example.0`)
    pattern: ?[]const u8 = null,
};

pub const DefinitionOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const TypeDefinitionOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const TypeDefinitionRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

pub const ImplementationOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const ImplementationRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

pub const ReferenceOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const DocumentHighlightOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const DocumentSymbolOptions = struct {
    /// A human-readable string that is shown when multiple outlines trees are shown for the same document.
    ///
    /// @since 3.16.0
    label: ?[]const u8 = null,

    workDoneProgress: ?bool = null,
};

pub const CodeActionOptions = struct {
    /// CodeActionKinds that this server may return.
    ///
    /// The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server may list out every specific kind they provide.
    codeActionKinds: ?[]const CodeActionKind = null,

    /// The server provides support to resolve additional information for a code action.
    ///
    /// @since 3.16.0
    resolveProvider: ?bool = null,

    workDoneProgress: ?bool = null,
};

/// The kind of a code action.
///
/// Kinds are a hierarchical list of identifiers separated by `.`, e.g. `"refactor.extract.function"`.
///
/// The set of kinds is open and client needs to announce the kinds it supports to the server during initialization.
pub const CodeActionKind = []const u8;

pub const CodeLensOptions = struct {
    /// Code lens has a resolve provider as well.
    resolveProvider: ?bool = null,

    workDoneProgress: ?bool = null,
};

pub const DocumentLinkOptions = struct {
    ///
    /// Document links have a resolve provider as well.
    ///
    resolveProvider: ?bool = null,

    workDoneProgress: ?bool = null,
};

pub const DocumentColorOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const DocumentColorRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

pub const DocumentFormattingOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const DocumentRangeFormattingOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const DocumentOnTypeFormattingOptions = struct {
    /// A character on which formatting should be triggered, like `{`.
    firstTriggerCharacter: []const u8,

    /// More trigger characters.
    moreTriggerCharacter: ?[]const []const u8 = null,
};

pub const RenameOptions = struct {
    /// Renames should be checked and tested before being executed.
    prepareProvider: ?bool = null,

    workDoneProgress: ?bool = null,
};

pub const FoldingRangeOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const FoldingRangeRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

pub const ExecuteCommandOptions = struct {
    /// The commands to be executed on the server
    commands: []const []const u8,

    workDoneProgress: ?bool = null,
};

pub const SelectionRangeOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const SelectionRangeRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

pub const LinkedEditingRangeOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const LinkedEditingRangeRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

pub const CallHierarchyOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const CallHierarchyRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

pub const SemanticTokensOptions = struct {
    /// The legend used by the server
    legend: SemanticTokensLegend,

    /// Server supports providing semantic tokens for a specific range of a document.
    range: ?bool,

    /// Server supports providing semantic tokens for a full document.
    full: ?Either(.{ bool, struct {
        /// The server supports deltas for full documents.
        delta: ?bool = null,
    } }) = null,

    workDoneProgress: ?bool = null,
};

pub const SemanticTokensRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,

    /// The legend used by the server
    legend: SemanticTokensLegend,

    /// Server supports providing semantic tokens for a specific range of a document.
    range: ?bool,

    /// Server supports providing semantic tokens for a full document.
    full: ?Either(.{ bool, struct {
        /// The server supports deltas for full documents.
        delta: ?bool = null,
    } }) = null,
};

pub const SemanticTokensLegend = struct {
    /// The token types a server uses.
    tokenTypes: []const []const u8,

    /// The token modifiers a server uses.
    tokenModifiers: []const []const u8,
};

pub const MonikerOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const MonikerRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

pub const TypeHierarchyOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const TypeHierarchyRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

/// Inline value options used during static registration.
///
/// @since 3.17.0
pub const InlineValueOptions = struct {
    workDoneProgress: ?bool = null,
};

pub const InlineValueRegistrationOptions = struct {
    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

/// Inlay hint options used during static registration.
///
/// @since 3.17.0
pub const InlayHintOptions = struct {
    /// The server provides support to resolve additional information for an inlay hint item.
    resolveProvider: ?bool = null,

    workDoneProgress: ?bool = null,
};

pub const InlayHintRegistrationOptions = struct {
    /// The server provides support to resolve additional information for an inlay hint item.
    resolveProvider: ?bool = null,

    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

/// Diagnostic options.
///
/// @since 3.17.0
pub const DiagnosticOptions = struct {
    /// An optional identifier under which the diagnostics are managed by the client.
    identifier: ?[]const u8 = null,

    /// Whether the language has inter file dependencies meaning that editing code in one file can result in a different diagnostic set in another file. Inter file dependencies are common for most programming languages and typically uncommon for linters.
    interFileDependencies: bool,

    /// The server provides support for workspace diagnostics as well.
    workspaceDiagnostics: bool,

    workDoneProgress: ?bool = null,
};

/// Diagnostic registration options.
///
/// @since 3.17.0
pub const DiagnosticRegistrationOptions = struct {
    /// An optional identifier under which the diagnostics are managed by the client.
    identifier: ?[]const u8 = null,

    /// Whether the language has inter file dependencies meaning that editing code in one file can result in a different diagnostic set in another file. Inter file dependencies are common for most programming languages and typically uncommon for linters.
    interFileDependencies: bool,

    /// The server provides support for workspace diagnostics as well.
    workspaceDiagnostics: bool,

    id: ?[]const u8 = null,
    documentSelector: ?DocumentSelector = null,
    workDoneProgress: ?bool = null,
};

pub const WorkspaceSymbolOptions = struct {
    /// The server provides support to resolve additional information for a workspace symbol.
    ///
    /// @since 3.17.0
    resolveProvider: ?bool = null,

    workDoneProgress: ?bool = null,
};

const WorkspaceFoldersServerCapabilities = struct {
    /// The server has support for workspace folders
    supported: ?bool = null,

    /// Whether the server wants to receive workspace folder change notifications.
    ///
    /// If a string is provided, the string is treated as an ID under which the notification is registered on the client side. The ID can be used to unregister for these events using the `client/unregisterCapability` request.
    changeNotifications: ?Either(.{ []const u8, bool }),
};

/// The options to register for file operations.
///
/// @since 3.16.0
pub const FileOperationRegistrationOptions = struct {
    /// The actual filters.
    filters: []const FileOperationFilter,
};

/// A filter to describe in which file operation requests or notifications the server is interested in.
///
/// @since 3.16.0
pub const FileOperationFilter = struct {
    /// A Uri like `file` or `untitled`.
    scheme: ?[]const u8 = null,

    /// The actual file operation pattern.
    pattern: FileOperationPattern,
};

/// A pattern to describe in which file operation requests or notifications the server is interested in.
///
/// @since 3.16.0
pub const FileOperationPattern = struct {
    /// The glob pattern to match. Glob patterns can have the following syntax:
    /// - `*` to match zero or more characters in a path segment
    /// - `?` to match on one character in a path segment
    /// - `**` to match any number of path segments, including none
    /// - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}`
    ///   matches all TypeScript and JavaScript files)
    /// - `[]` to declare a range of characters to match in a path segment
    ///   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
    /// - `[!...]` to negate a range of characters to match in a path segment
    ///   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but
    ///   not `example.0`)
    glob: []const u8,

    /// Whether to match files or folders with this pattern.
    ///
    /// Matches both if undefined.
    matches: ?FileOperationPatternKind = null,

    /// Additional options used during matching.
    options: ?FileOperationPatternOptions = null,
};

/// A pattern kind describing if a glob pattern matches a file a folder or both.
///
/// @since 3.16.0
pub const FileOperationPatternKind = []const u8;

/// Matching options for the file operation pattern.
///
/// @since 3.16.0
pub const FileOperationPatternOptions = struct {
    /// The pattern should be matched ignoring casing.
    ignoreCase: ?bool = null,
};

/// Information about the server.
///
/// @since 3.15.0
pub const ServerInfo = struct {
    /// The name of the server as defined by the server.
    name: []const u8,
    /// The server's version as defined by the server.
    version: ?[]const u8 = null,
};

/// Represents a collection of [completion items](#CompletionItem) to be presented in the editor.
pub const CompletionList = struct {
    /// This list is not complete. Further typing should result in recomputing this list.
    ///
    /// Recomputed lists have all their items replaced (not appended) in the incomplete completion sessions.
    isIncomplete: bool,

    /// In many cases the items of an actual completion result share the same value for properties like `commitCharacters` or the range of a text edit. A completion list can therefore define item defaults which will be used if a completion item itself doesn't specify the value.
    ///
    /// If a completion list specifies a default value and a completion item also specifies a corresponding value the one from the item is used.
    ///
    /// Servers are only allowed to return default values if the client signals support for this via the `completionList.itemDefaults` capability.
    ///
    /// @since 3.17.0
    itemDefaults: ?struct {
        /// A default commit character set.
        ///
        /// @since 3.17.0
        commitCharacters: ?[]const []const u8 = null,

        /// A default edit range
        ///
        /// @since 3.17.0
        editRange: ?Either(.{ Range, struct {
            insert: Range,
            replace: Range,
        } }) = null,

        /// A default insert text format
        ///
        /// @since 3.17.0
        insertTextFormat: ?InsertTextFormat = null,

        /// A default insert text mode
        ///
        /// @since 3.17.0
        insertTextMode: ?InsertTextMode = null,

        /// A default data value.
        ///
        /// @since 3.17.0
        data: ?std.json.Value = null,
    } = null,

    /// The completion items.
    items: []const CompletionItem,
};

/// Defines whether the insert text in a completion item should be interpreted as plain text or a snippet.
pub const InsertTextFormat = enum(u32) {
    /// The primary text to be inserted is treated as a plain string.
    plainText = 1,
    /// The primary text to be inserted is treated as a snippet.
    ///
    /// A snippet can define tab stops and placeholders with `$1`, `$2` and `${3:foo}`. `$0` defines the final tab stop, it defaults to the end of the snippet. Placeholders with equal identifiers are linked, that is typing in one will update others too.
    snippet = 2,
};

/// How whitespace and indentation is handled during completion item insertion.
///
/// @since 3.16.0
pub const InsertTextMode = enum(u32) {
    /// The insertion or replace strings is taken as it is. If the value is multi line the lines below the cursor will be inserted using the indentation defined in the string value. The client will not apply any kind of adjustments to the string.
    asIs = 1,

    /// The editor adjusts leading whitespace of new lines so that they match the indentation up to the cursor of the line for which the item is accepted.
    ///
    /// Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a multi line completion item is indented using 2 tabs and all following lines inserted will be indented using 2 tabs as well.
    adjustIndentation = 2,
};

pub const CompletionItem = struct {
    /// The label of this completion item.
    ///
    /// The label property is also by default the text that is inserted when selecting this completion.
    ///
    /// If label details are provided the label itself should be an unqualified name of the completion item.
    label: []const u8,

    /// Additional details for the label
    ///
    /// @since 3.17.0
    labelDetails: ?CompletionItemLabelDetails = null,

    /// The kind of this completion item. Based of the kind an icon is chosen by the editor. The standardized set of available values is defined in `CompletionItemKind`.
    kind: ?CompletionItemKind = null,

    /// Tags for this completion item.
    ///
    /// @since 3.15.0
    tags: ?[]const CompletionItemTag = null,

    /// A human-readable string with additional information about this item, like type or symbol information.
    detail: ?[]const u8 = null,

    /// A human-readable string that represents a doc-comment.
    documentation: ?Either(.{ []const u8, MarkupContent }) = null,

    /// Indicates if this item is deprecated.
    ///
    /// @deprecated Use `tags` instead if supported.
    deprecated: ?bool = null,

    /// Select this item when showing.
    ///
    /// *Note* that only one completion item can be selected and that the tool / client decides which item that is. The rule is that the *first* item of those that match best is selected.
    preselect: ?bool = null,

    /// A string that should be used when comparing this item with other items. When omitted the label is used as the sort text for this item.
    sortText: ?[]const u8 = null,

    /// A string that should be used when filtering a set of completion items. When omitted the label is used as the filter text for this item.
    filterText: ?[]const u8 = null,

    /// A string that should be inserted into a document when selecting this completion. When omitted the label is used as the insert text for this item.
    ///
    /// The `insertText` is subject to interpretation by the client side. Some tools might not take the string literally. For example VS Code when code complete is requested in this example `con<cursor position>` and a completion item with an `insertText` of `console` is provided it will only insert `sole`. Therefore it is recommended to use `textEdit` instead since it avoids additional client side interpretation.
    insertText: ?[]const u8 = null,

    /// The format of the insert text. The format applies to both the `insertText` property and the `newText` property of a provided `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.
    ///
    /// Please note that the insertTextFormat doesn't apply to `additionalTextEdits`.
    insertTextFormat: ?InsertTextFormat = null,

    /// How whitespace and indentation is handled during completion item insertion. If not provided the client's default value depends on the `textDocument.completion.insertTextMode` client capability.
    ///
    /// @since 3.16.0
    /// @since 3.17.0 - support for `textDocument.completion.insertTextMode`
    insertTextMode: ?InsertTextMode = null,

    /// An edit which is applied to a document when selecting this completion. When an edit is provided the value of `insertText` is ignored.
    ///
    /// *Note:* The range of the edit must be a single line range and it must contain the position at which completion has been requested.
    ///
    /// Most editors support two different operations when accepting a completion item. One is to insert a completion text and the other is to replace an existing text with a completion text. Since this can usually not be predetermined by a server it can report both ranges. Clients need to signal support for `InsertReplaceEdit`s via the `textDocument.completion.completionItem.insertReplaceSupport` client capability property.
    ///
    /// *Note 1:* The text edit's range as well as both ranges from an insert replace edit must be a [single line] and they must contain the position at which completion has been requested. *Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range must be a prefix of the edit's replace range, that means it must be contained and starting at the same position.
    ///
    /// @since 3.16.0 additional type `InsertReplaceEdit`
    textEdit: ?Either(.{ TextEdit, InsertReplaceEdit }) = null,

    /// The edit text used if the completion item is part of a CompletionList and CompletionList defines an item default for the text edit range.
    ///
    /// Clients will only honor this property if they opt into completion list item defaults using the capability `completionList.itemDefaults`.
    ///
    /// If not provided and a list's default range is provided the label property is used as a text.
    ///
    /// @since 3.17.0
    textEditText: ?[]const u8 = null,

    /// An optional array of additional text edits that are applied when selecting this completion. Edits must not overlap (including the same insert position) with the main edit nor with themselves.
    ///
    /// Additional text edits should be used to change text unrelated to the current cursor position (for example adding an import statement at the top of the file if the completion item will insert an unqualified type).
    additionalTextEdits: ?[]const TextEdit = null,

    /// An optional set of characters that when pressed while this completion is active will accept it first and then type that character. *Note* that all commit characters should have `length=1` and that superfluous characters will be ignored.
    commitCharacters: ?[]const []const u8 = null,

    /// An optional command that is executed *after* inserting this completion. *Note* that additional modifications to the current document should be described with the additionalTextEdits-property.
    command: ?Command = null,

    /// A data entry field that is preserved on a completion item between a completion and a completion resolve request.
    data: ?std.json.Value = null,

    pub fn fromSymbol(symbol: symbols.Symbol) CompletionItem {
        const kind = std.meta.stringToEnum(CompletionItemKind, @tagName(symbol.kind));

        const detail = if (symbol.detail.len == 0) null else symbol.detail;
        const documentation = if (symbol.documentation.len == 0) null else symbol.documentation;

        return .{
            .label = symbol.name,
            .kind = kind,
            .detail = detail,
            .documentation = if (documentation) |d| .{ .payload = .{ .@"[]const u8" = d } } else null,
        };
    }
};

/// Additional details for a completion item label.
///
/// @since 3.17.0
pub const CompletionItemLabelDetails = struct {
    /// An optional string which is rendered less prominently directly after {@link CompletionItem.label label}, without any spacing. Should be used for function signatures or type annotations.
    detail: ?[]const u8 = null,

    /// An optional string which is rendered less prominently after {@link CompletionItemLabelDetails.detail}. Should be used for fully qualified names or file path.
    description: ?[]const u8 = null,
};

/// The kind of a completion entry.
pub const CompletionItemKind = enum(u32) {
    text = 1,
    method = 2,
    function = 3,
    constructor = 4,
    field = 5,
    variable = 6,
    class = 7,
    interface = 8,
    module = 9,
    property = 10,
    unit = 11,
    value = 12,
    @"enum" = 13,
    keyword = 14,
    snippet = 15,
    color = 16,
    file = 17,
    reference = 18,
    folder = 19,
    enumMember = 20,
    constant = 21,
    @"struct" = 22,
    event = 23,
    operator = 24,
    typeParameter = 25,
};

/// Completion item tags are extra annotations that tweak the rendering of a completion item.
///
/// @since 3.15.0
pub const CompletionItemTag = enum(u32) {
    /// Render a completion as obsolete, usually using a strike-out.
    deprecated = 1,
};

/// A `MarkupContent` literal represents a string value which content is interpreted base on its kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
///
/// If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
///
/// Here is an example how such a string can be constructed using JavaScript / TypeScript:
/// ```typescript
/// let markdown: MarkdownContent = {
///     kind: MarkupKind.Markdown,
///     value: [
///         '# Header',
///         'Some text',
///         '```typescript',
///         'someCode();',
///         '```'
///     ].join('\n')
/// };
/// ```
///
/// *Please Note* that clients might sanitize the return markdown. A client could decide to remove HTML from the markdown to avoid script execution.
///
pub const MarkupContent = struct {
    /// The type of the Markup
    kind: MarkupKind,

    /// The content itself
    value: []const u8,
};

/// Describes the content type that a client supports in various result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
///
/// Please note that `MarkupKinds` must not start with a `$`. This kinds are reserved for internal usage.
pub const MarkupKind = []const u8;

/// Plain text is supported as a content format
pub const MarkupKindPlainText = "plaintext";

/// Markdown is supported as a content format
pub const MarkupKindMarkdown = "markdown";

pub const TextEdit = struct {
    /// The range of the text document to be manipulated. To insert text into a document create a range where start === end.
    range: Range,

    /// The string to be inserted. For delete operations use an empty string.
    newText: []const u8,
};

/// A special text edit to provide an insert and a replace operation.
///
/// @since 3.16.0
pub const InsertReplaceEdit = struct {
    /// The string to be inserted.
    newText: []const u8,

    /// The range if the insert is requested
    insert: Range,

    /// The range if the replace is requested.
    replace: Range,
};

pub const Command = struct {
    /// Title of the command, like `save`.
    title: []const u8,
    /// The identifier of the actual command handler.
    command: []const u8,
    /// Arguments that the command handler should be invoked with.
    arguments: ?[]const std.json.Value = null,
};

pub const PublishDiagnosticsParams = struct {
    /// The URI for which diagnostic information is reported.
    uri: DocumentUri,

    /// Optional the version number of the document the diagnostics are published for.
    ///
    /// @since 3.15.0
    version: ?u32 = null,

    /// An array of diagnostic information items.
    diagnostics: []const Diagnostic,
};

pub const Diagnostic = struct {
    /// The range at which the message applies.
    range: Range,

    /// The diagnostic's severity. To avoid interpretation mismatches when a server is used with different clients it is highly recommended that servers always provide a severity value. If omitted, it’s recommended for the client to interpret it as an Error severity.
    severity: ?DiagnosticSeverity = null,

    /// The diagnostic's code, which might appear in the user interface.
    code: ?Either(.{ u32, []const u8 }) = null,

    /// An optional property to describe the error code.
    ///
    /// @since 3.16.0
    codeDescription: ?CodeDescription = null,

    /// A human-readable string describing the source of this diagnostic, e.g. 'typescript' or 'super lint'.
    source: ?[]const u8 = null,

    /// The diagnostic's message.
    message: []const u8,

    /// Additional metadata about the diagnostic.
    ///
    /// @since 3.15.0
    tags: ?[]const DiagnosticTag = null,

    /// An array of related diagnostic information, e.g. when symbol-names within a scope collide all definitions can be marked via this property.
    relatedInformation: ?[]const DiagnosticRelatedInformation = null,

    /// A data entry field that is preserved between a `textDocument/publishDiagnostics` notification and `textDocument/codeAction` request.
    ///
    /// @since 3.16.0
    data: ?std.json.Value = null,

    pub fn fromDiag(diagnostic: diag.Diagnostic) @This() {
        return .{
            .range = .fromSpan(diagnostic.span),
            .severity = diagnostic.severity,
            .source = "runic",
            .message = diagnostic.message,
        };
    }
};

/// Represents a related message and source code location for a diagnostic. This should be used to point to code locations that cause or are related to a diagnostics, e.g when duplicating a symbol in a scope.
pub const DiagnosticRelatedInformation = struct {
    /// The location of this related diagnostic information.
    location: Location,

    /// The message of this related diagnostic information.
    message: []const u8,
};

pub const Location = struct {
    uri: DocumentUri,
    range: Range,
};

pub const DiagnosticSeverity = enum(u32) {
    /// Reports an error.
    @"error" = 1,
    /// Reports a warning.
    warning = 2,
    /// Reports an information.
    information = 3,
    /// Reports a hint.
    hint = 4,
};

/// Structure to capture a description for an error code.
///
/// @since 3.16.0
pub const CodeDescription = struct {
    /// An URI to open with more information about the diagnostic error.
    href: Uri,
};

/// The diagnostic tags.
///
/// @since 3.15.0
pub const DiagnosticTag = enum(u32) {
    /// Unused or unnecessary code.
    ///
    /// Clients are allowed to render diagnostics with this tag faded out instead of having an error squiggle.
    unnecessary = 1,
    /// Deprecated or obsolete code.
    ///
    /// Clients are allowed to rendered diagnostics with this tag strike through.
    deprecated = 2,
};

pub const ResponseError = struct {
    /// A number indicating the error type that occurred.
    code: i32,

    /// A string providing a short description of the error.
    message: []const u8,

    /// A primitive or structured value that contains additional information about the error. Can be omitted.
    data: ?std.json.Value = null,
};

pub const ErrorCodes = struct {
    // Defined by JSON-RPC
    pub const parseError = -32700;
    pub const invalidRequest = -32600;
    pub const methodNotFound = -32601;
    pub const invalidParams = -32602;
    pub const internalError = -32603;

    /// This is the start range of JSON-RPC reserved error codes. It doesn't denote a real error code. No LSP error codes should be defined between the start and end range. For backwards compatibility the `ServerNotInitialized` and the `UnknownErrorCode` are left in the range.
    ///
    /// @since 3.16.0
    pub const jsonrpcReservedErrorRangeStart = -32099;
    /// @deprecated use jsonrpcReservedErrorRangeStart */
    pub const serverErrorStart = jsonrpcReservedErrorRangeStart;

    /// Error code indicating that a server received a notification or request before the server received the `initialize` request.
    pub const serverNotInitialized = -32002;
    pub const unknownErrorCode = -32001;

    /// This is the end range of JSON-RPC reserved error codes. It doesn't denote a real error code.
    ///
    /// @since 3.16.0
    pub const jsonrpcReservedErrorRangeEnd = -32000;
    /// @deprecated use jsonrpcReservedErrorRangeEnd */
    pub const serverErrorEnd = jsonrpcReservedErrorRangeEnd;

    /// This is the start range of LSP reserved error codes. It doesn't denote a real error code.
    ///
    /// @since 3.16.0
    pub const lspReservedErrorRangeStart = -32899;

    /// A request failed but it was syntactically correct, e.g the method name was known and the parameters were valid. The error message should contain human readable information about why the request failed.
    ///
    /// @since 3.17.0
    pub const requestFailed = -32803;

    /// The server cancelled the request. This error code should only be used for requests that explicitly support being server cancellable.
    ///
    /// @since 3.17.0
    pub const serverCancelled = -32802;

    /// The server detected that the content of a document got modified outside normal conditions. A server should NOT send this error code if it detects a content change in its unprocessed messages. The result even computed on an older state might still be useful for the client.
    ///
    /// If a client decides that a result is not of any use anymore the client should cancel the request.
    pub const contentModified = -32801;

    /// The client has canceled a request and a server has detected the cancel.
    pub const requestCancelled = -32800;

    /// This is the end range of LSP reserved error codes. It doesn't denote a real error code.
    ///
    /// @since 3.16.0
    pub const lspReservedErrorRangeEnd = -32800;
};

pub fn response(id: ?RequestId, result: anytype) ResultResponse(@TypeOf(result)) {
    return .init(id, result);
}

pub fn responseError(id: ?RequestId, code: i32, message: []const u8) ResultResponse(u32) {
    return .err(id, code, message);
}

pub fn ResultResponse(comptime T: type) type {
    return struct {
        jsonrpc: []const u8 = "2.0",
        id: ?RequestId,
        result: ?T = null,
        @"error": ?ResponseError = null,

        pub fn init(id: ?RequestId, result: T) @This() {
            return .{
                .id = id,
                .result = result,
            };
        }

        pub fn err(id: ?RequestId, code: i32, message: []const u8) @This() {
            return .{
                .id = id,
                .@"error" = .{
                    .code = code,
                    .message = message,
                },
            };
        }
    };
}

pub fn methodResponse(method: []const u8, result: anytype) MethodResponse(@TypeOf(result)) {
    return .init(method, result);
}

pub fn MethodResponse(comptime T: type) type {
    return struct {
        jsonrpc: []const u8 = "2.0",
        method: []const u8,
        params: T,

        pub fn init(method: []const u8, params: T) @This() {
            return .{
                .method = method,
                .params = params,
            };
        }
    };
}
