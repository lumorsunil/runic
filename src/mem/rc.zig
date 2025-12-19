const std = @import("std");
const Allocator = std.mem.Allocator;
const rainbow = @import("../rainbow.zig");

pub const RCError =
    Allocator.Error ||
    std.Io.Writer.Error ||
    error{
        NotAllocated,
        InvalidRef,
    };

const log_enabled = true;

// const prefix_color = "";
// const prefix_ref_color = "";
// const init_color = "";
// const release_color = "";
// const ref_color = "";
// const error_color = "";
// const warn_color = "";
// const end_color = "";

const prefix_color = rainbow.beginColor(.yellow);
const prefix_ref_color = rainbow.beginColor(.orange);
const init_color = rainbow.beginColor(.blue);
const release_color = rainbow.beginColor(.green);
const ref_color = rainbow.beginColor(.violet);
const error_color = rainbow.beginColor(.red);
const warn_color = rainbow.beginColor(.red);
const end_color = rainbow.endColor();

pub const RCInitOptions = struct {
    label: ?[]const u8 = null,
};

fn isLoggingEnabled() bool {
    return std.process.hasEnvVar(std.heap.page_allocator, "RUNIC_LOG_RC") catch false;
}

pub fn RC(comptime T: type) type {
    return struct {
        allocator: Allocator,
        ptr: ?*T = null,
        refs: usize = 0,

        pub const Ref = struct {
            id: usize,
            rc: ?*RC(T),
            options: RCInitOptions,

            pub const RCType = RC(T);
            pub const Payload = T;

            pub fn init(allocator_: Allocator, value: T, options: RCInitOptions) RCError!Ref {
                const rc = try RC(T).init(allocator_, value);
                const ref_ = try rc.ref(options);
                try ref_.log("{s}init{s}", .{ init_color, end_color });
                return ref_;
            }

            pub fn deinit(self: *Ref, options: DeinitOptions) void {
                if (self.rc) |rc| {
                    self.log("{s}release{s} (refs left before release: {})", .{ release_color, end_color, rc.refs }) catch {};
                    if (rc.refs == 1) {
                        self.rc = null;
                    }
                    if (rc.release(options)) {
                        return;
                    }
                } else {
                    self.log("{s}release called on already released RC.Ref{s}", .{ warn_color, end_color }) catch {};
                    return;
                }
                if (self.rc.?.refs == 0 or !options.refresh_ref) {
                    self.rc = null;
                }
            }

            pub fn log(self: Ref, comptime fmt: []const u8, args: anytype) !void {
                if (!isLoggingEnabled()) return;

                var stderr = std.fs.File.stderr().writer(&.{});
                const writer = &stderr.interface;

                if (self.options.label) |label| {
                    try writer.print("{s}{?*}{s} (\"{s}\"):\n", .{ prefix_ref_color, self.rc, end_color, label });
                    try writer.print(fmt, args);
                } else {
                    try writer.print("{s}{?*}{s} (id: {}):\n", .{ prefix_ref_color, self.rc, end_color, self.id });
                    try writer.print(fmt, args);
                }
                try writer.writeByte('\n');
                try writer.flush();
            }

            pub fn dupe(allocator_: Allocator, value: ConstSlice, options: RCInitOptions) RCError!Ref {
                const rc = try RC(T).dupe(allocator_, value);
                const ref_ = try rc.ref(options);
                try ref_.log("{s}dupe{s}", .{ init_color, end_color });
                return ref_;
            }

            pub fn print(
                allocator_: Allocator,
                comptime fmt: []const u8,
                args: anytype,
                options: RCInitOptions,
            ) RCError!Ref {
                const rc = try RC(T).print(allocator_, fmt, args);
                const ref_ = try rc.ref(options);
                try ref_.log("{s}print{s}", .{ init_color, end_color });
                return ref_;
            }

            pub fn get(self: Ref) RCError!T {
                const rc = self.rc orelse {
                    self.log(@src().fn_name ++ " {s}InvalidRef{s}", .{ error_color, end_color }) catch {};
                    return RCError.InvalidRef;
                };
                return rc.get();
            }

            pub fn getPtr(self: Ref) RCError!*T {
                const rc = self.rc orelse {
                    self.log(@src().fn_name ++ " {s}InvalidRef{s}", .{ error_color, end_color }) catch {};
                    return RCError.InvalidRef;
                };
                return rc.getPtr();
            }

            pub fn ref(self: Ref, options: RCInitOptions) RCError!Ref {
                const rc = self.rc orelse {
                    self.log(@src().fn_name ++ " {s}InvalidRef{s}", .{ error_color, end_color }) catch {};
                    return RCError.InvalidRef;
                };

                const ref_ = try rc.ref(options);
                try self.log("{s}new ref{s} (source) (refs: {})", .{ ref_color, end_color, rc.refs });
                try ref_.log("{s}new ref{s} (new)", .{ ref_color, end_color });
                return ref_;
            }

            pub fn clone(self: Ref, options: RCInitOptions) RCError!Ref {
                const rc = self.rc orelse {
                    self.log(@src().fn_name ++ " {s}InvalidRef{s}", .{ error_color, end_color }) catch {};
                    return RCError.InvalidRef;
                };

                try self.log("{s}clone rc{s} (source)", .{ ref_color, end_color });
                const rc_clone = try rc.clone();
                return try rc_clone.ref(options);
            }

            pub fn allocator(self: Ref) RCError!Allocator {
                const rc = self.rc orelse {
                    self.log(@src().fn_name ++ " {s}InvalidRef{s}", .{ error_color, end_color }) catch {};
                    return RCError.InvalidRef;
                };
                return rc.allocator;
            }

            pub fn refs(self: Ref) usize {
                const rc = self.rc orelse return 0;
                return rc.refs;
            }

            pub fn format(self: Ref, writer: *std.Io.Writer) !void {
                const value = self.get() catch {
                    try writer.writeAll("<ref error>");
                    return;
                };
                if (T == []u8 or T == []const u8) {
                    try writer.print("{s}", .{value});
                } else {
                    try writer.print("{f}", .{value});
                }
            }
        };

        pub fn init(allocator: Allocator, value: T) RCError!*RC(T) {
            const rcPtr = try allocator.create(RC(T));
            try rcPtr.initInner(allocator, value);
            return rcPtr;
        }

        pub fn dupe(allocator: Allocator, value: ConstSlice) RCError!*RC(T) {
            if (!isSlice) @compileError("Cannot dupe non-slice types");
            const duped = try allocator.dupe(@typeInfo(T).pointer.child, value);
            return try .init(allocator, duped);
        }

        pub fn print(
            allocator: Allocator,
            comptime fmt: []const u8,
            args: anytype,
        ) RCError!*RC(T) {
            if (!isSlice) @compileError("Cannot print non-slice types");
            const Child = @typeInfo(T).pointer.child;
            if (Child != u8) @compileError("Cannot print non-byte slices");
            const buffer = try std.fmt.allocPrint(allocator, fmt, args);
            return try .init(allocator, buffer);
        }

        /// Will not clone the child.
        pub fn clone(
            self: *RC(T),
        ) RCError!*RC(T) {
            if (self.isPtrAllocated()) |ptr| return .init(self.allocator, ptr.*);
            return RCError.NotAllocated;
        }

        fn initInner(self: *RC(T), allocator: Allocator, value: T) RCError!void {
            const ptr = try allocator.create(T);
            ptr.* = value;

            self.* = .{
                .allocator = allocator,
                .ptr = ptr,
            };

            try self.log(@src().fn_name ++ ": created {*}", .{ptr});
        }

        pub fn log(self: *RC(T), comptime fmt: []const u8, args: anytype) !void {
            if (!isLoggingEnabled()) return;

            var stderr = std.fs.File.stderr().writer(&.{});
            const writer = &stderr.interface;

            try writer.print("{s}{*}{s}:\n", .{ prefix_color, self, end_color });
            try writer.print("  " ++ fmt, args);
            try writer.writeByte('\n');
            try writer.flush();
        }

        const isArrayList: bool = switch (@typeInfo(T)) {
            .@"struct" => if (@hasField(T, "items")) brk: {
                const field = std.meta.fieldInfo(T, .items);
                const Elem_ = std.meta.Elem(field.type);
                break :brk T == std.ArrayList(Elem_);
            } else false,
            else => false,
        };

        const isSlice = brk: {
            switch (@typeInfo(T)) {
                .pointer => |p| if (p.size == .slice) break :brk true,
                else => {},
            }

            break :brk false;
        };

        const isPtrOne = brk: {
            switch (@typeInfo(T)) {
                .pointer => |p| if (p.size == .one) break :brk true,
                else => {},
            }

            break :brk false;
        };

        const Elem: ?type = brk: {
            if (isSlice) {
                break :brk std.meta.Elem(T);
            } else if (isArrayList) {
                break :brk std.meta.Elem(std.meta.fieldInfo(T, .items).type);
            }

            break :brk null;
        };

        const isElemDeinitable = if (Elem) |E| switch (@typeInfo(E)) {
            .pointer, .@"struct", .@"union", .@"enum" => true,
            else => false,
        } else false;

        const ConstSlice = if (Elem) |E| []const E else void;

        pub const DeinitOptions = struct {
            deinit_child: bool = true,
            /// Custom deinit function for child value.
            /// Needs to destroy `*T` as well.
            deinit_child_fn: ?DeinitChildFn(T) = null,
            /// Custom deinit function for array elements.
            deinit_array_child_fn: if (Elem) |E| ?DeinitChildFn(E) else ?void = null,
            /// Don't set rc = null if we still have other refs.
            refresh_ref: bool = false,
        };

        pub fn DeinitChildFn(comptime Child: type) type {
            return union(enum) {
                with_allocator: *const fn (*Child, Allocator) void,
                without_allocator: *const fn (*Child) void,

                pub fn withAllocator(deinit_fn: *const fn (*Child, Allocator) void) @This() {
                    return .{ .with_allocator = deinit_fn };
                }

                pub fn withoutAllocator(deinit_fn: *const fn (*Child) void) @This() {
                    return .{ .without_allocator = deinit_fn };
                }

                pub fn call(self: @This(), child: *Child, allocator: Allocator) void {
                    switch (self) {
                        .with_allocator => |deinit_fn| deinit_fn(child, allocator),
                        .without_allocator => |deinit_fn| deinit_fn(child),
                    }
                }
            };
        }

        pub fn deinit(self: *RC(T), options: DeinitOptions) void {
            self.log(@src().fn_name ++ " {{deinit_child: {}}}", .{options.deinit_child}) catch {};
            if (options.deinit_array_child_fn) |deinit_fn| {
                self.log("deinit_array_child_fn: {any}", .{deinit_fn}) catch {};
            }
            if (options.deinit_child) {
                if (options.deinit_child_fn) |deinit_fn| {
                    self.log("deinit_child_fn: {any}", .{deinit_fn}) catch {};
                    deinit_fn.call(self.ptr.?, self.allocator);
                } else {
                    self.deinitChild(options);
                }
            }
            self.ptr = null;
            self.refs = 0;
            self.allocator.destroy(self);
        }

        fn deinitChild(self: *RC(T), options: DeinitOptions) void {
            self.log(@src().fn_name, .{}) catch {};
            if (self.isAllocated()) |ptr| {
                if (isArrayList) {
                    self.log(@src().fn_name ++ " array list", .{}) catch {};
                    self.deinitArrayElements(ptr.items, options);
                    ptr.deinit(self.allocator);
                } else if (callDeinitIfExists(self.allocator, ptr)) |_| {
                    self.log(@src().fn_name ++ " struct with deinit", .{}) catch {};
                } else if (isSlice) {
                    self.log(@src().fn_name ++ " slice", .{}) catch {};
                    self.deinitArrayElements(ptr.*, options);
                    self.allocator.free(ptr.*);
                } else if (isPtrOne) {
                    self.log(@src().fn_name ++ " pointer one", .{}) catch {};
                    self.allocator.destroy(ptr.*);
                }
                ptr.* = undefined;
                self.allocator.destroy(ptr);
            }
        }

        fn deinitArrayElements(
            self: *RC(T),
            items: [](Elem orelse void),
            options: DeinitOptions,
        ) void {
            if (!isElemDeinitable) return;
            for (items) |*item| self.deinitArrayElement(item, options);
        }

        fn deinitArrayElement(
            self: *RC(T),
            item: *(Elem orelse void),
            options: DeinitOptions,
        ) void {
            if (options.deinit_array_child_fn) |deinit_fn| {
                self.log("deinit_array_child_fn: {any}", .{deinit_fn}) catch {};
                deinit_fn.call(item, self.allocator);
            } else {
                _ = callDeinitIfExists(self.allocator, item);
            }
        }

        fn get(self: RC(T)) RCError!T {
            if (self.isPtrAllocated()) |ptr| return ptr.*;
            return RCError.NotAllocated;
        }

        fn getPtr(self: RC(T)) RCError!*T {
            if (self.isPtrAllocated()) |ptr| return ptr;
            return RCError.NotAllocated;
        }

        /// Returns true if RC was invalidated
        fn release(self: *RC(T), options: DeinitOptions) bool {
            if (self.isAllocated() == null) {
                std.log.warn("RC: called release on non-allocated released RC ({*})", .{self});
                return true;
            }
            self.refs -|= 1;
            if (self.refs == 0) {
                self.deinit(options);
                return true;
            }

            return false;
        }

        fn isAllocated(self: *RC(T)) ?*T {
            return self.isPtrAllocated();
        }

        fn isPtrAllocated(self: RC(T)) ?*T {
            return self.ptr;
        }

        pub fn ref(self: *RC(T), options: RCInitOptions) RCError!Ref {
            if (self.isAllocated() == null) {
                if (log_enabled) self.log("releasing ref error: NotAllocated", .{}) catch {};
                return RCError.NotAllocated;
            }
            const id = self.refs;
            self.refs += 1;
            return .{
                .id = id,
                .rc = self,
                .options = options,
            };
        }
    };
}

fn callDeinitIfExists(allocator: Allocator, deinitee: anytype) ?void {
    const T: type = switch (@typeInfo(@TypeOf(deinitee))) {
        .pointer => |pointer| brk: {
            if (pointer.size == .one) break :brk pointer.child;
            return null;
        },
        else => @TypeOf(deinitee),
    };

    if (std.meta.hasFn(T, "deinit")) {
        const params = @typeInfo(@TypeOf(T.deinit)).@"fn".params;
        const is_self = if (params.len > 0) params[0].type == *T or params[0].type == T else false;
        if (!is_self) return null;

        const DeinitOptions = if (@hasDecl(T, "RCType")) T.RCType.DeinitOptions else void;

        if (params.len == 3 and params[1].type == Allocator and params[2].type == DeinitOptions) {
            return deinitee.deinit(allocator, .{});
        } else if (params.len == 2 and params[1].type == DeinitOptions) {
            return deinitee.deinit(.{});
        } else if (params.len == 2 and params[1].type == Allocator) {
            return deinitee.deinit(allocator);
        } else if (params.len == 1) {
            return deinitee.deinit();
        } else comptime {
            var errorMessage: []const u8 = "Unsupported deinit with more than 3 parameter. (self: " ++ @typeName(T) ++ ") Found ";

            for (params, 0..) |param, i| {
                errorMessage = errorMessage ++ @typeName(param.type orelse void);
                if (i < params.len - 1) errorMessage = errorMessage ++ ", ";
            }

            @compileError(errorMessage);
        }
    }

    return null;
}

test "RC ref get" {
    const testing = @import("std").testing;
    var ref: RC([]u8).Ref = try .dupe(testing.allocator, "the string", .{});
    defer ref.deinit(.{});
    try testing.expectEqual(1, ref.refs());
    var ref2 = try ref.ref(.{});
    defer ref2.deinit(.{});
    try testing.expectEqual(2, ref.refs());
    const refGet = try ref.getPtr();
    const ref2Get = try ref2.getPtr();
    try testing.expectEqual(2, ref.refs());
    try testing.expectEqual(ref.rc.?.ptr, refGet);
    try testing.expectEqual(refGet, ref2Get);
    try testing.expectEqualStrings("the string", refGet.*);
}

test "RC ref release" {
    const testing = @import("std").testing;
    const rc = try RC([]u8).dupe(testing.allocator, "the string");
    var ref = try rc.ref(.{});
    ref.deinit(.{});
}

test "RC ref release multiple" {
    const testing = @import("std").testing;
    const rc = try RC([]u8).dupe(testing.allocator, "the string");
    var ref = try rc.ref(.{});
    var ref2 = try rc.ref(.{});
    try testing.expectEqual(2, rc.refs);
    ref.deinit(.{});
    try testing.expectEqual(null, ref.rc);
    try testing.expectEqual(1, rc.refs);
    try testing.expect(rc.isAllocated() != null);
    ref2.deinit(.{});
    try testing.expectEqual(null, ref2.rc);
}

test "RC ref deinit children" {
    const testing = @import("std").testing;
    var rcs = std.ArrayList(RC([]u8).Ref).empty;
    defer rcs.deinit(testing.allocator);
    try rcs.append(testing.allocator, try .dupe(testing.allocator, "the first string", .{}));
    try rcs.append(testing.allocator, try .dupe(testing.allocator, "the second string", .{}));
    try rcs.append(testing.allocator, try .dupe(testing.allocator, "the third string", .{}));
    const rc = try RC([]RC([]u8).Ref).dupe(testing.allocator, rcs.items);
    var ref = try rc.ref(.{});
    var ref2 = try rc.ref(.{});
    try testing.expectEqual(2, rc.refs);
    ref.deinit(.{});
    try testing.expectEqual(null, ref.rc);
    try testing.expectEqual(1, rc.refs);
    try testing.expect(rc.isAllocated() != null);
    ref2.deinit(.{});
    try testing.expectEqual(null, ref2.rc);
}

test "RC deinit uses deinit_child_fn with allocator" {
    const testing = std.testing;

    const Payload = struct {
        freed: *bool,
    };

    const destroy = struct {
        fn call(payload: *Payload, allocator: Allocator) void {
            const freed_ptr = payload.*.freed;
            freed_ptr.* = true;
            allocator.destroy(payload);
        }
    }.call;

    var freed = false;
    const rc = try RC(Payload).init(testing.allocator, .{ .freed = &freed });
    var ref = try rc.ref(.{});
    ref.deinit(.{ .deinit_child_fn = .withAllocator(destroy) });

    try testing.expect(freed);
}

test "RC deinit uses deinit_child_fn without allocator" {
    const testing = std.testing;

    const Payload = struct {
        freed: *bool,
        allocator: Allocator,
    };

    const destroy = struct {
        fn call(payload: *Payload) void {
            const freed_ptr = payload.*.freed;
            freed_ptr.* = true;
            payload.*.allocator.destroy(payload);
        }
    }.call;

    var freed = false;
    const rc = try RC(Payload).init(testing.allocator, .{
        .freed = &freed,
        .allocator = testing.allocator,
    });
    var ref = try rc.ref(.{});
    ref.deinit(.{ .deinit_child_fn = .withoutAllocator(destroy) });

    try testing.expect(freed);
}

test "RC deinit skips child when disabled" {
    const testing = std.testing;

    const Payload = struct {
        freed: *bool,

        fn deinit(self: *@This(), allocator: Allocator) void {
            self.freed.* = true;
            allocator.destroy(self);
        }
    };

    var freed = false;
    const rc = try RC(Payload).init(testing.allocator, .{ .freed = &freed });
    var ref = try rc.ref(.{});
    const payload_ptr = try ref.getPtr();
    ref.deinit(.{ .deinit_child = false });

    try testing.expectEqual(false, freed);
    payload_ptr.deinit(testing.allocator);
}

test "RC deinit uses custom array child deinit" {
    const testing = std.testing;

    const Elem = struct {
        freed: *bool,
    };

    const markFreed = struct {
        fn call(elem: *Elem, allocator: Allocator) void {
            const freed_ptr = elem.*.freed;
            freed_ptr.* = true;
            _ = allocator;
        }
    }.call;

    var freed = [_]bool{ false, false, false };
    const rc = try RC([]Elem).dupe(testing.allocator, &[_]Elem{
        .{ .freed = &freed[0] },
        .{ .freed = &freed[1] },
        .{ .freed = &freed[2] },
    });

    var ref = try rc.ref(.{});
    ref.deinit(.{ .deinit_array_child_fn = .withAllocator(markFreed) });

    try testing.expectEqualSlices(bool, &[_]bool{ true, true, true }, freed[0..]);
}
