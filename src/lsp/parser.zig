const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const diag = @import("diagnostics.zig");
const ast = runic.ast;
const token = runic.token;
const DocumentStore = @import("document.zig").DocumentStore;

pub const Parser = runic.parser.Parser(DocumentStore, getCachedAst, putCachedAst, getSource);

fn getCachedAst(self: *DocumentStore, path: []const u8) !?runic.ast.Script {
    const uri = try self.resolveUri(path);
    const document = self.get(uri) orelse return null;
    return document.ast;
}

fn putCachedAst(self: *DocumentStore, path: []const u8, script: runic.ast.Script) !void {
    const uri = try self.resolveUri(path);
    const document = self.get(uri) orelse return error.DocumentNotFound;
    document.ast = script;
}

fn getSource(self: *DocumentStore, path: []const u8) ![]const u8 {
    const uri = try self.resolveUri(path);
    const document = self.get(uri);
    if (document) |d| return d.text;
    return error.DocumentNotFound;
    // const file = try std.fs.cwd().openFile(path, .{});
    // defer file.close();
    // var buffer: [512]u8 = undefined;
    // var reader = file.reader(&buffer);
    // const source = try reader.interface.readAlloc(self.allocator, max_source_bytes);
    // return source;
}
pub fn parseFile(
    allocator: Allocator,
    list: *std.ArrayList(diag.Diagnostic),
    parser: *Parser,
    absolute_path: []const u8,
) !?ast.Script {
    return parser.parseScript(absolute_path) catch |err| {
        var message = std.Io.Writer.Allocating.init(allocator);
        const loc = try writeParseError(parser, &message.writer, err);
        try list.append(allocator, .{
            .uri = try std.fmt.allocPrint(allocator, "file://{s}", .{absolute_path}),
            .message = try message.toOwnedSlice(),
            .span = .fromLocs(loc, loc),
            .severity = .err,
        });

        return null;
    };
}

fn writeParseError(
    parser: *const Parser,
    writer: *std.Io.Writer,
    err: anyerror,
) !token.Location {
    try writer.print("Error: {}\n", .{err});
    if (parser.unexpected_token) |tok| {
        try writer.print("Unexpected token {}, ", .{tok});
    }
    const expected = parser.expectedTokens();
    if (expected.len > 0) {
        try writer.writeAll("expected tokens ");
        _ = try parser.writeExpectedTokens(writer);
        try writer.writeByte('\n');
    }
    try writer.writeByte('\n');
    try writer.flush();

    return .{
        .file = parser.stream.lexer.file,
        .line = parser.stream.lexer.line,
        .column = parser.stream.lexer.column,
        .offset = parser.stream.lexer.index,
    };
}
