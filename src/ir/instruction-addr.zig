const std = @import("std");

pub const ResolvedInstructionAddr = struct {
    instr_set: usize,
    local_addr: usize,

    pub fn init(instr_set: usize, addr: usize) @This() {
        return .{ .instr_set = instr_set, .local_addr = addr };
    }

    pub fn inc(self: *@This()) void {
        self.local_addr += 1;
    }

    pub fn equals(self: @This(), other: @This()) bool {
        return std.meta.eql(self, other);
    }

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("<{}:{x}>", .{ self.instr_set, self.local_addr });
    }
};

pub const InstructionAddr = struct {
    instr_set: usize,
    local_addr: Local,

    pub fn initRel(instr_set: usize, rel: isize) @This() {
        return .{ .instr_set = instr_set, .local_addr = .{ .rel = rel } };
    }

    pub fn initAbs(instr_set: usize, abs: usize) @This() {
        return .{ .instr_set = instr_set, .local_addr = .{ .abs = abs } };
    }

    pub fn initLabel(instr_set: usize, label: LabelKey) @This() {
        return .{ .instr_set = instr_set, .local_addr = .{ .label = label } };
    }

    pub const Local = union(enum) {
        rel: isize,
        abs: usize,
        label: LabelKey,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            switch (self) {
                .rel => |a| try w.print("+{x}", .{a}),
                .abs => |a| try w.print("{x}", .{a}),
                .label => |a| try w.print(":{f}", .{a}),
            }
        }
    };

    pub const LabelKey = struct {
        counter: usize,
        name: []const u8,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("{s}_{}", .{ self.name, self.counter });
        }
    };

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("<{}:{f}>", .{ self.instr_set, self.local_addr });
    }
};
