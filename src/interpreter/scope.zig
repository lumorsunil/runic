const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;
const RC = @import("../mem/root.zig").RC;
const RCError = @import("../mem/root.zig").RCError;

/// ScopeStack manages lexical bindings for the interpreter. Each frame stores
/// immutable/mutable bindings, and lookups walk outward so inner frames can
/// shadow outer declarations.
pub const ScopeStack = struct {
    allocator: Allocator,
    frames: std.ArrayList(Frame) = .empty,

    pub const Error = Allocator.Error || RCError || error{
        ScopeUnderflow,
        DuplicateBinding,
    };

    const Frame = struct {
        bindings: std.ArrayList(Binding) = .empty,
        options: FrameOptions,

        fn init(options: FrameOptions) Frame {
            return .{ .options = options };
        }

        fn deinit(self: *Frame, allocator: Allocator) void {
            for (self.bindings.items) |*binding| binding.deinit(allocator);
            self.bindings.deinit(allocator);
            self.* = undefined;
        }
    };

    const FrameOptions = struct {
        blocking: bool = false,
    };

    const Binding = union(enum) {
        constant: BindingValue,
        mutable: RC(BindingValue).Ref,

        pub const BindingValue = struct {
            name: []const u8,
            value: Value,

            pub fn deinit(self: *BindingValue, allocator: Allocator) void {
                allocator.free(self.name);
                self.value.deinit(allocator);
            }
        };

        pub fn init(
            allocator: Allocator,
            name: []const u8,
            value: Value,
            is_mutable: bool,
        ) RCError!Binding {
            if (is_mutable) {
                return .{ .mutable = try .init(allocator, .{
                    .name = name,
                    .value = value,
                }) };
            } else {
                return .{ .constant = .{
                    .name = name,
                    .value = value,
                } };
            }
        }

        pub fn ref(self: Binding) RCError!Binding {
            return switch (self) {
                .constant => RCError.InvalidRef,
                .mutable => |m| .{ .mutable = try m.ref() },
            };
        }

        pub fn deinit(self: *Binding, allocator: Allocator) void {
            switch (self.*) {
                .constant => |*c| c.deinit(allocator),
                .mutable => |*m| m.release(),
            }
        }

        pub fn get(self: Binding) RCError!BindingValue {
            return switch (self) {
                .constant => |c| c,
                .mutable => |m| m.get(),
            };
        }

        pub fn getPtr(self: *Binding) RCError!*BindingValue {
            return switch (self.*) {
                .constant => |*c| c,
                .mutable => |m| m.getPtr(),
            };
        }

        pub fn getValue(self: Binding) RCError!Value {
            return switch (self) {
                .constant => |c| c.value,
                .mutable => |m| (try m.get()).value,
            };
        }

        pub fn getValuePtr(self: *Binding) RCError!*Value {
            return switch (self.*) {
                .constant => |*c| &c.value,
                .mutable => |m| &(try m.getPtr()).value,
            };
        }

        pub fn getName(self: Binding) RCError![]const u8 {
            return switch (self) {
                .constant => |c| c.name,
                .mutable => |m| (try m.get()).name,
            };
        }
    };

    pub const BindingRef = struct {
        rc: ?RC(Binding.BindingValue).Ref,
        value: *Value,
        is_mutable: bool,
    };

    pub fn init(allocator: Allocator) ScopeStack {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *ScopeStack) void {
        for (self.frames.items) |*frame| frame.deinit(self.allocator);
        self.frames.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn pushFrame(self: *ScopeStack, options: FrameOptions) Error!void {
        try self.frames.append(self.allocator, .init(options));
    }

    pub fn popFrame(self: *ScopeStack) Error!void {
        const frame = self.frames.pop() orelse return Error.ScopeUnderflow;
        var frame_copy = frame;
        frame_copy.deinit(self.allocator);
    }

    pub fn declare(
        self: *ScopeStack,
        name: []const u8,
        value: *Value,
        is_mutable: bool,
    ) Error!void {
        var frame = try self.currentFrame();
        if (try findInFrame(frame, name) != null) return Error.DuplicateBinding;

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);

        const moved_value = value.move();
        try frame.bindings.append(self.allocator, try .init(
            self.allocator,
            name_owned,
            moved_value,
            is_mutable,
        ));
    }

    pub fn declareClosureRef(
        self: *ScopeStack,
        binding: Binding,
    ) Error!void {
        var frame = try self.currentFrame();
        const name = try binding.getName();
        if (try findInFrame(frame, name) != null) return Error.DuplicateBinding;

        switch (binding) {
            .constant => |c| {
                const name_owned = try self.allocator.dupe(u8, name);
                errdefer self.allocator.free(name_owned);
                var value_owned = try c.value.clone(self.allocator);
                errdefer value_owned.deinit(self.allocator);

                try frame.bindings.append(self.allocator, try .init(
                    self.allocator,
                    name_owned,
                    value_owned,
                    false,
                ));
            },
            .mutable => {
                try frame.bindings.append(self.allocator, try binding.ref());
            },
        }
    }
    pub fn lookup(self: *ScopeStack, name: []const u8) RCError!?BindingRef {
        var index = self.frames.items.len;
        while (index > 0) {
            index -= 1;
            const frame = &self.frames.items[index];
            if (try findInFrame(frame, name)) |binding| {
                return .{
                    .value = try binding.getValuePtr(),
                    .is_mutable = binding.* == .mutable,
                    .rc = if (binding.* == .mutable) binding.mutable else null,
                };
            }
            if (frame.options.blocking) return null;
        }
        return null;
    }

    fn currentFrame(self: *ScopeStack) Error!*Frame {
        if (self.frames.items.len == 0) return Error.ScopeUnderflow;
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn findInFrame(frame: *Frame, name: []const u8) RCError!?*Binding {
        for (frame.bindings.items) |*binding| {
            if (std.mem.eql(u8, try binding.getName(), name)) return binding;
        }
        return null;
    }

    pub fn closure(self: *ScopeStack) Error!*ScopeStack {
        const scope = try self.allocator.create(ScopeStack);
        scope.* = .init(self.allocator);

        for (self.frames.items) |frame| {
            try scope.pushFrame(frame.options);

            for (frame.bindings.items) |binding| {
                try scope.declareClosureRef(binding);
            }
        }

        return scope;
    }
};
