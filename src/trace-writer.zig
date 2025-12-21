const std = @import("std");
const rainbow = @import("rainbow.zig");

const log_enabled = false;

const prefix_color = rainbow.beginColor(.blue);
const end_color = rainbow.endColor();

fn log(w: *std.Io.Writer, comptime fmt: []const u8, args: anytype) void {
    if (!log_enabled) return;
    const parent = TraceWriter.getParent(w);
    std.log.debug("[{s}({s}) {*} --> {*}{s}]", .{
        prefix_color,
        parent.label,
        w,
        parent.wrapped,
        end_color,
    });
    std.log.debug(fmt, args);
}

fn logWithoutParent(w: *std.Io.Writer, comptime fmt: []const u8, args: anytype) void {
    if (!log_enabled) return;
    std.log.debug("[{s}{*}{s}]", .{
        prefix_color,
        w,
        end_color,
    });
    std.log.debug(fmt, args);
}

pub const TraceWriter = struct {
    writer: std.Io.Writer,
    wrapped: *std.Io.Writer,
    label: []const u8,

    const vtable = std.Io.Writer.VTable{
        .drain = drain,
        .sendFile = sendFile,
        .flush = flush,
        .rebase = rebase,
    };

    pub fn init(buffer: []u8, w: *std.Io.Writer, label: []const u8) @This() {
        logWithoutParent(w, @src().fn_name ++ " ({s})", .{label});
        return .{
            .writer = .{ .vtable = &vtable, .buffer = buffer },
            .wrapped = w,
            .label = label,
        };
    }

    fn getParent(w: *std.Io.Writer) *@This() {
        return @fieldParentPtr("writer", w);
    }

    fn drain(w: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        log(w, @src().fn_name ++ " (data.len={}, data[0].len={}, splat={})", .{ data.len, data[0].len, splat });
        const parent = getParent(w);
        const buffer_written = try parent.wrapped.writeSplat(&.{w.buffered()}, 1);
        w.end -= buffer_written;

        var written: usize = 0;

        if (w.end == 0) {
            written = try parent.wrapped.writeSplat(data, splat);
        }

        try parent.wrapped.flush();
        return written;
    }

    fn sendFile(
        w: *std.Io.Writer,
        file_reader: *std.fs.File.Reader,
        limit: std.Io.Limit,
    ) std.Io.Writer.FileError!usize {
        log(w, @src().fn_name ++ " ({*}, limit={})", .{ &file_reader.interface, limit });
        const parent = getParent(w);
        return parent.wrapped.sendFile(file_reader, limit);
    }

    fn flush(w: *std.Io.Writer) std.Io.Writer.Error!void {
        log(w, @src().fn_name, .{});
        return w.defaultFlush();
    }

    fn rebase(w: *std.Io.Writer, preserve: usize, capacity: usize) std.Io.Writer.Error!void {
        log(w, @src().fn_name ++ " (preserve={}, capacity={})", .{ preserve, capacity });
        return w.defaultRebase(preserve, capacity);
    }
};
