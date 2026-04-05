const std = @import("std");

pub const ExitCode = union(enum) {
    success,
    err: anyerror,
    term: std.process.Child.Term,

    pub fn fromByte(byte: u8) ExitCode {
        if (byte == 0) return .success;
        return .{ .term = .{ .Exited = byte } };
    }

    pub fn fromTerm(term: std.process.Child.SpawnError!std.process.Child.Term) ExitCode {
        if (term) |t| {
            return switch (t) {
                .Exited => |exit_code| fromByte(exit_code),
                else => .{ .term = t },
            };
        } else |err| {
            return .{ .err = err };
        }
    }

    pub fn fromBoolean(boolean: bool) ExitCode {
        return .fromByte(if (boolean) 0 else 1);
    }

    pub fn toBoolean(self: @This()) bool {
        return self == .success;
    }

    pub fn negate(self: @This()) @This() {
        return .fromBoolean(!self.toBoolean());
    }

    pub fn getErrorCode(self: ExitCode) u8 {
        return switch (self) {
            .success => 0,
            .term => |term| switch (term) {
                .Exited => |exit_code| exit_code,
                else => 1,
            },
            .err => 1,
        };
    }

    pub fn deserialize(r: *std.Io.Reader) std.Io.Reader.Error!@This() {
        return std.mem.bytesAsValue(@This(), try r.takeArray(@sizeOf(@This()))).*;
    }

    pub fn format(self: ExitCode, writer: *std.Io.Writer) !void {
        try switch (self) {
            .success, .term => writer.print("{}", .{self.getErrorCode()}),
            .err => |err| writer.print("{} ({})", .{ self.getErrorCode(), err }),
        };
    }
};
