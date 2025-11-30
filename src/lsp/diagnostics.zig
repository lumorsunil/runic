const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const token = runic.token;
const types = @import("types.zig");

pub const Diagnostic = struct {
    uri: types.Uri,
    message: []const u8,
    span: token.Span,
    severity: types.DiagnosticSeverity,

    pub fn deinit(self: Diagnostic, allocator: Allocator) void {
        allocator.free(self.uri);
        allocator.free(self.message);
    }
};
