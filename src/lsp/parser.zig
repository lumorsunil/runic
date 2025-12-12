const std = @import("std");
const runic = @import("runic");
const ast = runic.ast;
const token = runic.token;
const reportErrors = @import("reporter.zig").reportErrors;
const Workspace = @import("workspace.zig").Workspace;

pub fn parseFile(
    workspace: *Workspace,
    parser: *runic.parser.Parser,
    absolute_path: []const u8,
) !?ast.Script {
    const result = parser.parseScript(absolute_path);

    switch (result) {
        .success => |script| return script,
        .err => |err_info| try reportErrors(workspace, err_info),
    }

    return null;
}

fn writeParseError(
    parser: *const runic.parser.Parser,
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
