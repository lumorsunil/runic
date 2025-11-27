const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const token = runic.token;

pub const Diagnostic = struct {
    uri: []const u8,
    message: []const u8,
    span: token.Span,
    severity: Severity,

    pub fn deinit(self: Diagnostic, allocator: Allocator) void {
        allocator.free(self.uri);
        allocator.free(self.message);
    }

    pub const Severity = enum {
        err,
        warning,
        info,
        hint,

        pub fn format(self: Severity, writer: *std.Io.Writer) !void {
            const n: u8 = switch (self) {
                .err => 1,
                .warning => 2,
                .info => 3,
                .hint => 4,
            };

            try writer.print("{}", .{n});
        }
    };
};
