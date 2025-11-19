const std = @import("std");
const Value = @import("value.zig").Value;

fn ArrayListManaged(comptime T: type) type {
    return std.array_list.Managed(T);
}

/// ScopeStack manages lexical bindings for the interpreter. Each frame stores
/// immutable/mutable bindings, and lookups walk outward so inner frames can
/// shadow outer declarations.
pub const ScopeStack = struct {
    allocator: std.mem.Allocator,
    frames: ArrayListManaged(Frame),

    pub const Error = std.mem.Allocator.Error || error{
        ScopeUnderflow,
        DuplicateBinding,
    };

    const Frame = struct {
        bindings: ArrayListManaged(Binding),

        fn init(allocator: std.mem.Allocator) Frame {
            return .{ .bindings = .init(allocator) };
        }

        fn deinit(self: *Frame, allocator: std.mem.Allocator) void {
            for (self.bindings.items) |*binding| {
                allocator.free(binding.name);
                binding.value.deinit(allocator);
            }
            self.bindings.deinit();
            self.* = undefined;
        }
    };

    const Binding = struct {
        name: []u8,
        value: Value,
        is_mutable: bool,
    };

    pub const BindingRef = struct {
        value: *Value,
        is_mutable: bool,
    };

    pub fn init(allocator: std.mem.Allocator) ScopeStack {
        return .{
            .allocator = allocator,
            .frames = .init(allocator),
        };
    }

    pub fn deinit(self: *ScopeStack) void {
        for (self.frames.items) |*frame| {
            frame.deinit(self.allocator);
        }
        self.frames.deinit();
        self.* = undefined;
    }

    pub fn pushFrame(self: *ScopeStack) Error!void {
        try self.frames.append(Frame.init(self.allocator));
    }

    pub fn popFrame(self: *ScopeStack) Error!void {
        const frame = self.frames.pop() orelse return Error.ScopeUnderflow;
        var frame_copy = frame;
        frame_copy.deinit(self.allocator);
    }

    pub fn declare(self: *ScopeStack, name: []const u8, value: *Value, is_mutable: bool) Error!void {
        var frame = try self.currentFrame();
        if (findInFrame(frame, name) != null) return Error.DuplicateBinding;

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);

        const moved_value = value.move();
        try frame.bindings.append(.{
            .name = name_owned,
            .value = moved_value,
            .is_mutable = is_mutable,
        });
    }

    pub fn lookup(self: *ScopeStack, name: []const u8) ?BindingRef {
        var index = self.frames.items.len;
        while (index > 0) {
            index -= 1;
            const frame = &self.frames.items[index];
            if (findInFrame(frame, name)) |binding| {
                return .{
                    .value = &binding.value,
                    .is_mutable = binding.is_mutable,
                };
            }
        }
        return null;
    }

    fn currentFrame(self: *ScopeStack) Error!*Frame {
        if (self.frames.items.len == 0) return Error.ScopeUnderflow;
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn findInFrame(frame: *Frame, name: []const u8) ?*Binding {
        for (frame.bindings.items) |*binding| {
            if (std.mem.eql(u8, binding.name, name)) return binding;
        }
        return null;
    }
};
