const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;
const RC = @import("../mem/root.zig").RC;
const RCError = @import("../mem/root.zig").RCError;
const RCInitOptions = @import("../mem/root.zig").RCInitOptions;
const rainbow = @import("../rainbow.zig");

const prefix_color = rainbow.beginColor(.violet);
const end_color = rainbow.endColor();
const indentation = "  ";

/// ScopeStack manages lexical bindings for the interpreter. Each frame stores
/// immutable/mutable bindings, and lookups walk outward so inner frames can
/// shadow outer declarations.
pub const ScopeStack = struct {
    allocator: Allocator,
    frames: std.ArrayList(Frame) = .empty,
    scope_type: ScopeType,
    logging_enabled: bool = false,

    pub const Error = Allocator.Error || RCError || std.Io.Writer.Error || error{
        ScopeUnderflow,
        DuplicateBinding,
        FindInNonBindingFrame,
    };

    pub const ScopeType = enum {
        main,
        child,
    };

    const Frame = union(enum) {
        single: Single,
        scope_ref: ScopeRef,
        scope_ref_special: *ScopeStack,
        forward_context: ForwardContext,
        blocking,

        pub const Single = RC(std.ArrayList(Binding)).Ref;
        pub const ScopeRef = RC(ScopeStack).Ref;

        pub fn initSingleNew(allocator: Allocator) !Frame {
            return .{ .single = try .init(allocator, .empty, .{}) };
        }

        pub fn initSingleRef(single: Single) !Frame {
            const allocator = try single.allocator();
            const bindings = try single.getPtr();
            const single_ref = try single.clone(.{});
            const bindings_clone = try single_ref.getPtr();
            bindings_clone.* = try bindings.clone(allocator);
            for (bindings_clone.items) |*item| {
                item.* = try item.ref(allocator, .{});
            }
            return .{ .single = single_ref };
        }

        pub fn initScopeRef(scope_ref: ScopeRef) !Frame {
            const allocator = try scope_ref.allocator();
            const scope = try scope_ref.getPtr();
            const scope_cloned = try scope.closure();
            defer allocator.destroy(scope_cloned);
            const scope_ref_cloned: ScopeRef = try .init(allocator, scope_cloned.*, .{});
            return .{ .scope_ref = scope_ref_cloned };
        }

        pub fn initScopeRefSpecial(scopes: *ScopeStack) Frame {
            return .{ .scope_ref_special = scopes };
        }

        pub fn initForwardContext(forward_context: ForwardContext) Frame {
            return .{ .forward_context = forward_context };
        }

        pub fn initBlocking() Frame {
            return .blocking;
        }

        fn deinit(self: *Frame) void {
            switch (self.*) {
                .single => |*bindings_ref| bindings_ref.deinit(.{}),
                .scope_ref => |*scope_ref| scope_ref.deinit(.{}),
                .scope_ref_special => {},
                .forward_context => {},
                .blocking => {},
            }
            self.* = undefined;
        }

        fn deinitMain(self: *Frame, allocator: Allocator, scope: *ScopeStack, level: usize) void {
            scope.log(@src().fn_name ++ " frame {} : {s}", .{ level, @tagName(self.*) }) catch {};
            switch (self.*) {
                .single => |*bindings_ref| {
                    const bindings = bindings_ref.getPtr() catch {
                        @panic("shouldn't happen :)");
                    };

                    const bindings_items = bindings.toOwnedSlice(allocator) catch {
                        @panic("shouldn't happen :)");
                    };
                    defer allocator.free(bindings_items);

                    for (bindings_items) |*binding| {
                        const name = binding.getName() catch "<error>";
                        const value = binding.getValuePtr() catch null;
                        const value_type = if (value) |v| @tagName(v.*) else null;
                        scope.log(@src().fn_name ++ " binding \"{s}\" : {?s}", .{ name, value_type }) catch {};
                        binding.deinitMain(allocator);
                    }

                    bindings_ref.deinit(.{});
                },
                .scope_ref => |*scope_ref| {
                    scope_ref.deinit(.{
                        .deinit_child_fn = .withoutAllocator(ScopeStack.deinitMain),
                    });
                },
                .scope_ref_special => {},
                .forward_context => {},
                .blocking => {},
            }
            self.* = undefined;
        }

        pub fn logBindings(self: Frame, writer: *std.Io.Writer, level: usize, rel_level: usize) !void {
            try writeIndentation(writer, level);
            try writer.print("frame ({}) : {s}\n", .{ rel_level, @tagName(self) });

            switch (self) {
                .single => |bindings_ref| {
                    const bindings = try bindings_ref.get();
                    for (bindings.items) |binding| {
                        try writeIndentation(writer, level + 1);
                        const constantOrMut = if (binding == .constant) "constant" else "mutable";
                        const value = try binding.getValue();
                        try writer.print("{s} {s}: {f}\n", .{ constantOrMut, try binding.getName(), value });
                    }
                },
                .scope_ref => |scope_ref| {
                    try (try scope_ref.getPtr()).logBindings(@src().fn_name, level + 1, 0);
                },
                .scope_ref_special => |scope_ref_special| {
                    try scope_ref_special.logBindings(@src().fn_name, level + 1, 0);
                },
                .forward_context => {},
                .blocking => {},
            }
        }
    };

    pub const ForwardContext = struct {
        stdout: PipeInfo,
        stderr: PipeInfo,

        pub fn init(stdout: PipeInfo, stderr: PipeInfo) ForwardContext {
            return .{
                .stdout = stdout,
                .stderr = stderr,
            };
        }
    };

    pub const MaterializedForwardContext = struct {
        stdout: PipeMaterialized,
        stderr: PipeMaterialized,

        pub fn init(stdout: PipeMaterialized, stderr: PipeMaterialized) MaterializedForwardContext {
            return .{
                .stdout = stdout,
                .stderr = stderr,
            };
        }
    };

    const PipeInfo = union(enum) {
        inherit,
        materialized: PipeMaterialized,

        pub fn init(
            writer: *std.Io.Writer,
            streaming: PipeMaterialized.StreamingMode,
        ) PipeInfo {
            return .{
                .materialized = .{
                    .is_streaming = streaming,
                    .writer = writer,
                },
            };
        }
    };

    const PipeMaterialized = struct {
        is_streaming: StreamingMode,
        writer: *std.Io.Writer,

        pub const StreamingMode = enum {
            non_streaming,
            streaming,
        };
    };

    const Binding = union(enum) {
        constant: *BindingValue,
        mutable: RC(BindingValue).Ref,

        pub const BindingValue = struct {
            name: []const u8,
            value: Value,

            pub fn deinit(self: *BindingValue, allocator: Allocator) void {
                allocator.free(self.name);
                self.value.deinit();
                // TODO: this causes a double-free when we release the mutable RC while popping the scope
                allocator.destroy(self);
            }

            pub fn deinitMain(self: *BindingValue, allocator: Allocator) void {
                allocator.free(self.name);
                self.value.deinitMain();
                allocator.destroy(self);
            }

            pub fn clone(self: *BindingValue, allocator: Allocator) Error!*BindingValue {
                const ptr = try allocator.create(BindingValue);
                ptr.* = .{
                    .name = try allocator.dupe(u8, self.name),
                    .value = try self.value.clone(allocator),
                };

                return ptr;
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
                }, .{}) };
            } else {
                const constant = try allocator.create(BindingValue);
                constant.* = .{
                    .name = name,
                    .value = value,
                };
                return .{ .constant = constant };
            }
        }

        pub fn ref(self: Binding, allocator: Allocator, options: RCInitOptions) Error!Binding {
            return switch (self) {
                .constant => |c| .{ .constant = try c.clone(allocator) },
                .mutable => |m| .{ .mutable = try m.ref(options) },
            };
        }

        pub fn deinit(self: *Binding, allocator: Allocator) void {
            switch (self.*) {
                .constant => |c| c.deinit(allocator),
                .mutable => |*m| m.deinit(.{
                    .deinit_child_fn = .withAllocator(BindingValue.deinit),
                }),
            }
        }

        pub fn deinitMain(self: *Binding, allocator: Allocator) void {
            switch (self.*) {
                .constant => |c| c.deinitMain(allocator),
                .mutable => |*m| m.deinit(.{
                    .deinit_child_fn = .withAllocator(BindingValue.deinitMain),
                }),
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
                .constant => |c| &c.value,
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

    pub fn init(allocator: Allocator, scope_type: ScopeType) ScopeStack {
        const logging_enabled_s = std.process.getEnvVarOwned(allocator, "RUNIC_LOG_EXECUTOR") catch null;
        defer if (logging_enabled_s) |le| allocator.free(le);

        return .{
            .allocator = allocator,
            .scope_type = scope_type,
            .logging_enabled = if (logging_enabled_s) |le| std.mem.eql(u8, le, "1") else false,
        };
    }

    pub fn deinit(self: *ScopeStack) void {
        self.logBindings(@src().fn_name, 0, 0) catch {};
        switch (self.scope_type) {
            .main => self.deinitMain(),
            .child => self.deinitChild(),
        }
    }

    pub fn deinitNoDestroySelf(self: *ScopeStack) void {
        self.logBindings(@src().fn_name, 0, 0) catch {};
        switch (self.scope_type) {
            .main => self.deinitMainNoDestroySelf(),
            .child => self.deinitChildNoDestroySelf(),
        }
    }

    pub fn deinitMain(self: *ScopeStack) void {
        self.deinitMainNoDestroySelf();
        self.allocator.destroy(self);
        self.* = undefined;
    }

    pub fn deinitMainNoDestroySelf(self: *ScopeStack) void {
        self.logBindings(@src().fn_name, 0, 0) catch {};
        for (self.frames.items, 0..) |*frame, i| {
            frame.deinitMain(self.allocator, self, i);
        }
        self.frames.deinit(self.allocator);
    }

    pub fn deinitChild(self: *ScopeStack) void {
        self.deinitChildNoDestroySelf();
        self.allocator.destroy(self);
        self.* = undefined;
    }

    pub fn deinitChildNoDestroySelf(self: *ScopeStack) void {
        self.logBindings(@src().fn_name, 0, 0) catch {};
        for (self.frames.items) |*frame| frame.deinit();
        self.frames.deinit(self.allocator);
    }

    pub fn detach(self: *ScopeStack, levels: usize) Error!Value.ScopeRef {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        std.debug.assert(levels <= self.frames.items.len);

        const detached_ref: Value.ScopeRef = try .init(
            self.allocator,
            .init(self.allocator, .child),
            .{},
        );
        const detached = try detached_ref.getPtr();
        try detached.log(@src().fn_name ++ " from {*}", .{self});

        var popped_frames = try std.ArrayList(Frame).initCapacity(self.allocator, levels);
        defer popped_frames.deinit(self.allocator);

        for (0..levels) |_| {
            popped_frames.appendAssumeCapacity(try self.popFrameNoDeinit());
        }

        var reverseIt = std.mem.reverseIterator(popped_frames.items);
        while (reverseIt.next()) |frame| {
            try detached.pushFrame(@src().fn_name, frame);
        }

        return detached_ref;
    }

    pub fn log(self: *ScopeStack, comptime fmt: []const u8, args: anytype) !void {
        if (!self.logging_enabled) return;

        var stderr_ = std.fs.File.stderr().writer(&.{});
        const writer = &stderr_.interface;

        try writer.print("[{s}{*}{s}] ", .{ prefix_color, self, end_color });
        try writer.print(fmt ++ "\n", args);
    }

    pub fn pushFrame(self: *ScopeStack, label: []const u8, frame: Frame) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log("{s}:{s}({}) : {s}", .{ label, @src().fn_name, self.frames.items.len, @tagName(frame) });
        try self.frames.append(self.allocator, frame);
    }

    pub fn pushFrameForwarding(
        self: *ScopeStack,
        label: []const u8,
        stdout: *std.Io.Writer,
        stderr: *std.Io.Writer,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        const tag: std.meta.Tag(Frame) = .forward_context;
        try self.log("{s}:{s}({}) : {s}", .{ label, @src().fn_name, self.frames.items.len, @tagName(tag) });
        try self.frames.append(self.allocator, .initForwardContext(.init(
            .init(stdout, self.getStdoutPipe().is_streaming),
            .init(stderr, self.getStderrPipe().is_streaming),
        )));
    }

    pub fn popFrame(self: *ScopeStack, label: []const u8) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        const level = self.frames.items.len - 1;
        try self.log("{s}:{s}({}):", .{ label, @src().fn_name, level });
        try self.logBindings(@src().fn_name, level, level);
        var frame = try self.popFrameNoDeinit();
        frame.deinit();
    }

    pub fn popFrameN(self: *ScopeStack, label: []const u8, len: usize) Error!void {
        for (0..len) |_| try self.popFrame(label);
    }

    fn popFrameNoDeinit(self: *ScopeStack) Error!Frame {
        return self.frames.pop() orelse {
            try self.log("Couldn't pop frame", .{});
            return Error.ScopeUnderflow;
        };
    }

    pub fn declare(
        self: *ScopeStack,
        name: []const u8,
        value: *Value,
        is_mutable: bool,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};

        try self.log("{s}(\"{s}\" : {s} {s}):", .{
            @src().fn_name,
            name,
            if (is_mutable) "var" else "const",
            @tagName(value.*),
        });

        var frame = try self.currentFrameSingle();
        if (try findInFrame(frame, name) != null) return error.DuplicateBinding;

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);

        const moved_value = value.move();
        const bindings = try frame.single.getPtr();
        try bindings.append(self.allocator, try .init(
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
        var frame = try self.currentFrameSingle();
        const name = try binding.getName();
        if (try findInFrame(frame, name) != null) return Error.DuplicateBinding;

        switch (binding) {
            .constant => |c| {
                const name_owned = try self.allocator.dupe(u8, name);
                errdefer self.allocator.free(name_owned);
                var value_owned = try c.value.clone(self.allocator);
                errdefer value_owned.deinit(self.allocator);

                try frame.single.append(self.allocator, try .init(
                    self.allocator,
                    name_owned,
                    value_owned,
                    false,
                ));
            },
            .mutable => {
                try frame.single.append(
                    self.allocator,
                    try binding.ref(.{ .label = @src().fn_name }),
                );
            },
        }
    }

    pub fn lookup(self: *ScopeStack, name: []const u8) Error!?BindingRef {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name ++ "(\"{s}\")", .{name});
        var index = self.frames.items.len;
        while (index > 0) {
            index -= 1;
            const frame = &self.frames.items[index];
            switch (frame.*) {
                .single, .scope_ref, .scope_ref_special => {
                    if (try findInFrame(frame, name)) |binding| {
                        const value = try binding.getValuePtr();
                        var format_buf: [512]u8 = undefined;
                        try self.log(@src().fn_name ++ ": found {s}{s}", .{
                            @tagName(value.*),
                            formatValueValue(&format_buf, value) catch unreachable,
                        });
                        return .{
                            .value = value,
                            .is_mutable = binding.* == .mutable,
                            .rc = if (binding.* == .mutable) binding.mutable else null,
                        };
                    }
                },
                .blocking => {
                    try self.log(@src().fn_name ++ ": blocking, returning null", .{});
                    return null;
                },
                .forward_context => continue,
            }
        }
        try self.log(@src().fn_name ++ ": not found, returning null", .{});
        return null;
    }

    fn formatValueValue(buffer: []u8, value: *Value) ![]const u8 {
        var writer = std.Io.Writer.fixed(buffer);

        try writer.writeAll(" = ");

        switch (value.*) {
            .integer => |integer| try writer.print("{}", .{integer}),
            .float => |float| try writer.print("{}", .{float}),
            .string => |string_ref| {
                const string = try string_ref.get();
                try writer.print("\"{s}{s}\"", .{
                    string[0..@min(16, string.len)],
                    if (string.len >= 16) "..." else "",
                });
            },
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .void => try writer.print("{{}}", .{}),
            .range => |range| {
                if (range.end) |end| {
                    try writer.print("{}..{}", .{ range.start, end });
                } else {
                    try writer.print("{}..", .{range.start});
                }
            },
            .array => |array| try writer.print("<array:{}>", .{
                (try array.get()).items.len,
            }),
            .function => |fn_ref| try writer.print("<function:{s}>", .{
                (try fn_ref.getPtr()).fn_decl.name.name,
            }),
            .process_handle => try writer.print("<process_handle>", .{}),
            .scope => |scope_ref| try writer.print("<scope:{*}>", .{
                try scope_ref.getPtr(),
            }),
        }

        if (writer.buffered().len == 3) return "";

        return writer.buffered();
    }

    fn currentFrameSingle(self: *ScopeStack) Error!*Frame {
        if (self.frames.items.len == 0) return Error.ScopeUnderflow;
        for (0..self.frames.items.len) |i| {
            const j = self.frames.items.len - i - 1;
            const frame = &self.frames.items[j];
            switch (frame.*) {
                .single => return frame,
                else => continue,
            }
        }

        @panic("shouldn't happen :)");
    }

    fn findInFrame(frame: *Frame, name: []const u8) Error!?*Binding {
        switch (frame.*) {
            .single => |bindings_ref| {
                const bindings = try bindings_ref.getPtr();
                for (bindings.items) |*binding| {
                    if (std.mem.eql(u8, try binding.getName(), name)) return binding;
                }
            },
            .scope_ref => |scope_ref| {
                const scope = try scope_ref.getPtr();
                const current = try scope.currentFrameSingle();
                return findInFrame(current, name);
            },
            .scope_ref_special => |scope_ref_special| {
                const current = try scope_ref_special.currentFrameSingle();
                return findInFrame(current, name);
            },
            .forward_context => return Error.FindInNonBindingFrame,
            .blocking => return Error.FindInNonBindingFrame,
        }
        return null;
    }

    pub fn closure(self: *ScopeStack) Error!*ScopeStack {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log("{s}({})", .{ @src().fn_name, self.frames.items.len });
        const scope = try self.allocator.create(ScopeStack);
        scope.* = .init(self.allocator, .child);
        try scope.log(@src().fn_name ++ " from {*}", .{self});

        for (self.frames.items) |frame| {
            switch (frame) {
                .single => |bindings| {
                    try scope.pushFrame(@src().fn_name, try .initSingleRef(bindings));
                },
                .scope_ref => |scope_ref| {
                    try scope.pushFrame(@src().fn_name, try .initScopeRef(scope_ref));
                },
                .scope_ref_special => |scope_ref_special| {
                    try scope.pushFrame(@src().fn_name, .initScopeRefSpecial(scope_ref_special));
                },
                .forward_context => |forward_context| {
                    try scope.pushFrame(@src().fn_name, .initForwardContext(forward_context));
                },
                .blocking => {
                    try scope.pushFrame(@src().fn_name, .initBlocking());
                },
            }
        }

        return scope;
    }

    pub fn logBindings(self: *ScopeStack, label: []const u8, level: usize, frame_level: usize) Error!void {
        if (!self.logging_enabled) return;

        var file_writer = std.fs.File.stderr().writer(&.{});
        const writer = &file_writer.interface;

        if (level == 0) {
            try self.log("logBindings(\"{s}\", {}):\n", .{ label, level });
        }

        try writer.print("scope stack : {}\n", .{self.scope_type});

        for (frame_level..self.frames.items.len) |i| {
            try self.frames.items[i].logBindings(writer, level + i, i);
        }

        try writer.flush();
    }

    pub fn getForwardContext(
        self: *ScopeStack,
    ) MaterializedForwardContext {
        var index = self.frames.items.len - 1;
        var stdout: ?PipeMaterialized = null;
        var stderr: ?PipeMaterialized = null;

        while (true) {
            const idx, const forward_context = self.getForwardContextFromIndex(index);
            index = idx -| 1;

            if (stdout == null) {
                switch (forward_context.stdout) {
                    .materialized => |materialized| stdout = materialized,
                    .inherit => {},
                }
            }

            if (stderr == null) {
                switch (forward_context.stderr) {
                    .materialized => |materialized| stderr = materialized,
                    .inherit => {},
                }
            }

            if (stdout != null and stderr != null) {
                return .init(stdout.?, stderr.?);
            }

            if (index == 0) @panic("shouldn't happen :)");
        }
    }

    pub fn getForwardContextFromIndex(self: *ScopeStack, index: usize) struct { usize, ForwardContext } {
        var idx = index;
        while (idx >= 0) : (idx -= 1) {
            const frame = &self.frames.items[idx];
            switch (frame.*) {
                .forward_context => |forward_context| {
                    return .{ idx, forward_context };
                },
                else => continue,
            }
        }

        @panic("shouldn't happen :)");
    }

    pub fn isForwardContextStreaming(self: *ScopeStack) bool {
        return self.isForwardContextStreamingAux(self.frames.items.len - 1);
    }

    fn isForwardContextStreamingAux(self: *ScopeStack, index: usize) bool {
        const idx, const context = self.getForwardContextFromIndex(index);
        return switch (context.is_streaming) {
            .streaming => true,
            .non_streaming => false,
            .inherit => self.isForwardContextStreamingAux(idx - 1),
        };
    }

    pub fn getStdoutPipe(self: *ScopeStack) PipeMaterialized {
        const forward_context = self.getForwardContext();
        return forward_context.stdout;
    }

    pub fn getStderrPipe(self: *ScopeStack) PipeMaterialized {
        const forward_context = self.getForwardContext();
        return forward_context.stderr;
    }
};

fn writeIndentation(writer: *std.Io.Writer, level: usize) std.Io.Writer.Error!void {
    for (0..level) |_| try writer.writeAll(indentation);
}
