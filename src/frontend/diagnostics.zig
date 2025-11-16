const std = @import("std");
const token = @import("token.zig");

/// Options that control how syntax errors get rendered.
pub const RenderOptions = struct {
    source_name: []const u8 = "<stdin>",
    source: []const u8,
    message: []const u8,
    span: token.Span,
};

/// Renders a syntax diagnostic using a caret-style pointer that calls out the
/// offending line/column.
pub fn renderSyntaxError(writer: anytype, options: RenderOptions) !void {
    const line = options.span.start.line;
    const column = options.span.start.column;
    const digits = countDigits(line);

    try writer.print("error: {s}\n", .{options.message});
    try writer.print(" --> {s}:{d}:{d}\n", .{ options.source_name, line, column });

    const line_range = findLineRange(options.source, line);
    const line_slice = options.source[line_range.start..line_range.end];
    const pointer = caretInfo(line_range, options.span);

    try writer.print("{s: >{}} |\n", .{ "", digits });
    try writer.print("{d: >{}} | {s}\n", .{ line, digits, line_slice });
    try writer.print("{s: >{}} | ", .{ "", digits });
    try writeRepeated(writer, ' ', pointer.offset);
    try writeRepeated(writer, '^', pointer.width);
    try writer.writeAll("\n");
    if (@hasDecl(@TypeOf(writer), "flush")) {
        try writer.flush();
    }
}

const CaretInfo = struct {
    offset: usize,
    width: usize,
};

fn caretInfo(line_range: LineRange, span: token.Span) CaretInfo {
    const line_len = line_range.end - line_range.start;
    if (line_len == 0) return .{ .offset = 0, .width = 1 };

    const relative_start = if (span.start.offset <= line_range.start)
        0
    else
        span.start.offset - line_range.start;

    const span_end = if (span.end.offset <= line_range.start)
        line_range.start
    else
        span.end.offset;

    const clamped_end = if (span_end > line_range.end) line_range.end else span_end;
    const raw_width = if (clamped_end <= line_range.start + relative_start)
        1
    else
        clamped_end - (line_range.start + relative_start);

    return .{
        .offset = if (relative_start > line_len) line_len else relative_start,
        .width = std.math.max(1, std.math.min(raw_width, line_len)),
    };
}

const LineRange = struct {
    start: usize,
    end: usize,
};

fn findLineRange(source: []const u8, line_number: usize) LineRange {
    if (line_number == 0) return .{ .start = 0, .end = 0 };

    var current: usize = 1;
    var line_start: usize = 0;
    var idx: usize = 0;

    while (idx <= source.len) : (idx += 1) {
        if (idx == source.len or source[idx] == '\n') {
            if (current == line_number) {
                var end_index = idx;
                if (end_index > line_start and source[end_index - 1] == '\r') {
                    end_index -= 1;
                }
                return .{ .start = line_start, .end = end_index };
            }
            current += 1;
            line_start = idx + 1;
        }
    }

    return .{ .start = source.len, .end = source.len };
}

fn countDigits(value: usize) usize {
    var digits: usize = 1;
    var v = value;
    while (v >= 10) : (v /= 10) {
        digits += 1;
    }
    return digits;
}

fn writeRepeated(writer: anytype, byte: u8, count: usize) !void {
    var remaining = count;
    var buffer: [64]u8 = undefined;
    while (remaining > 0) {
        const emit = if (remaining > buffer.len) buffer.len else remaining;
        @memset(buffer[0..emit], byte);
        try writer.writeAll(buffer[0..emit]);
        remaining -= emit;
    }
}

test "renderSyntaxError prints caret pointers for a single-line issue" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var buffer = std.ArrayList(u8).init(arena.allocator());
    defer buffer.deinit();

    const src =
        \\let greeting = "hi"
        \\let broken = 
    ;

    try renderSyntaxError(buffer.writer(), .{
        .source_name = "script.rn",
        .source = src,
        .message = "expected expression after '='",
        .span = .{
            .start = .{ .line = 2, .column = 15, .offset = 32 },
            .end = .{ .line = 2, .column = 15, .offset = 32 },
        },
    });

    const expected =
        \\error: expected expression after '='
        \\ --> script.rn:2:15
        \\   |
        \\ 2 | let broken = 
        \\   |              ^
        \\
    ;
    try std.testing.expectEqualStrings(expected, buffer.items);
}

test "renderSyntaxError clamps caret width for multi-line spans" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var buffer = std.ArrayList(u8).init(arena.allocator());
    defer buffer.deinit();

    const src =
        \\fn demo() {
        \\    let x = 1
        \\}
    ;

    try renderSyntaxError(buffer.writer(), .{
        .source_name = "demo.rn",
        .source = src,
        .message = "missing semicolon",
        .span = .{
            .start = .{ .line = 2, .column = 5, .offset = 13 },
            .end = .{ .line = 3, .column = 1, .offset = 25 },
        },
    });

    const expected =
        \\error: missing semicolon
        \\ --> demo.rn:2:5
        \\   |
        \\ 2 |     let x = 1
        \\   |     ^^^^^^^^
        \\
    ;
    try std.testing.expectEqualStrings(expected, buffer.items);
}
