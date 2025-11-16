const std = @import("std");

/// TypeRef provides a stable pointer to an interned type inside `TypeStore`.
pub const TypeRef = *const Type;

/// Type captures the semantic type space exposed by Runic once AST nodes are
/// resolved. Complex types (arrays, maps, functions, etc.) keep pointers to
/// other `Type` instances managed by `TypeStore` so they can be shared.
pub const Type = union(enum) {
    primitive: Primitive,
    array: Collection.Array,
    map: Collection.Map,
    optional: OptionalType,
    promise: PromiseType,
    function: FunctionType,
    error_set: ErrorSet,
    error_union: ErrorUnion,
    process_handle: ProcessHandleType,

    pub const Primitive = enum {
        void,
        bool,
        int,
        float,
        string,
        bytes,
        pid,
        exit_status,
    };

    pub const Collection = struct {
        pub const Array = struct {
            element: TypeRef,
        };

        pub const Map = struct {
            key: TypeRef,
            value: TypeRef,
        };
    };

    pub const OptionalType = struct {
        child: TypeRef,
    };

    pub const PromiseType = struct {
        payload: TypeRef,
    };

    pub const FunctionType = struct {
        params: []const TypeRef,
        return_type: TypeRef,
        is_async: bool,
    };

    pub const ErrorSet = struct {
        behavior: Behavior,
        name: ?[]const u8,
        variants: []const Variant,

        pub const Behavior = enum {
            /// Equivalent to Zig's `anyerror`. Variants remain unknown until
            /// symbol resolution constrains the set.
            any,
            /// Closed sets list every variant via `variants`.
            closed,
        };

        pub const Variant = struct {
            name: []const u8,
            payload: ?TypeRef,
        };
    };

    pub const ErrorUnion = struct {
        set: TypeRef,
        payload: TypeRef,
    };

    pub const ProcessHandleType = struct {
        mode: Mode,
        stdout: StreamShape,
        stderr: StreamShape,
        status: TypeRef,

        pub const Mode = enum {
            /// A synchronously completed process whose outputs have already
            /// been captured.
            immediate,
            /// A background process that resolves through a promise.
            promise_backed,
        };

        pub const StreamShape = struct {
            payload: TypeRef,
            delivery: StreamDelivery,
        };

        pub const StreamDelivery = enum {
            inherit,
            buffered,
            streaming,
        };
    };
};

/// ErrorSetDesc mirrors the public surface for constructing concrete error
/// sets before interning them inside `TypeStore`.
pub const ErrorSetDesc = struct {
    name: ?[]const u8 = null,
    behavior: Type.ErrorSet.Behavior = .closed,
    variants: []const ErrorVariantDesc = &.{},
};

pub const ErrorVariantDesc = struct {
    name: []const u8,
    payload: ?TypeRef = null,
};

/// ProcessHandleDesc describes the process handle shape before it gets
/// interned as a `Type.process_handle` node.
pub const ProcessHandleDesc = struct {
    mode: Type.ProcessHandleType.Mode = .immediate,
    stdout: Type.ProcessHandleType.StreamShape,
    stderr: Type.ProcessHandleType.StreamShape,
    status: TypeRef,
};

/// TypeStore owns every instantiated `Type` node and guarantees the underlying
/// memory outlives all `TypeRef` instances handed to the rest of semantic
/// analysis.
pub const TypeStore = struct {
    arena: std.heap.ArenaAllocator,

    pub fn init(backing_allocator: std.mem.Allocator) TypeStore {
        return .{ .arena = std.heap.ArenaAllocator.init(backing_allocator) };
    }

    pub fn deinit(self: *TypeStore) void {
        self.arena.deinit();
    }

    fn allocator(self: *TypeStore) std.mem.Allocator {
        return self.arena.allocator();
    }

    fn create(self: *TypeStore, ty: Type) !TypeRef {
        const alloc = self.allocator();
        const slot = try alloc.create(Type);
        slot.* = ty;
        return slot;
    }

    fn dupeBytes(self: *TypeStore, bytes: []const u8) ![]const u8 {
        const alloc = self.allocator();
        const buf = try alloc.alloc(u8, bytes.len);
        std.mem.copy(u8, buf, bytes);
        return buf;
    }

    fn dupeMaybeBytes(self: *TypeStore, bytes: ?[]const u8) !?[]const u8 {
        if (bytes) |owned| {
            return try self.dupeBytes(owned);
        }
        return null;
    }

    fn copyTypeRefs(self: *TypeStore, refs: []const TypeRef) ![]const TypeRef {
        const alloc = self.allocator();
        const copy = try alloc.alloc(TypeRef, refs.len);
        std.mem.copy(TypeRef, copy, refs);
        return copy;
    }

    fn copyVariants(self: *TypeStore, variants: []const ErrorVariantDesc) ![]const Type.ErrorSet.Variant {
        const alloc = self.allocator();
        const copy = try alloc.alloc(Type.ErrorSet.Variant, variants.len);
        for (variants, 0..) |variant, idx| {
            copy[idx] = .{
                .name = try self.dupeBytes(variant.name),
                .payload = variant.payload,
            };
        }
        return copy;
    }

    pub fn primitive(self: *TypeStore, tag: Type.Primitive) !TypeRef {
        return self.create(.{ .primitive = tag });
    }

    pub fn array(self: *TypeStore, element: TypeRef) !TypeRef {
        return self.create(.{ .array = .{ .element = element } });
    }

    pub fn map(self: *TypeStore, key: TypeRef, value: TypeRef) !TypeRef {
        return self.create(.{ .map = .{ .key = key, .value = value } });
    }

    pub fn optional(self: *TypeStore, child: TypeRef) !TypeRef {
        return self.create(.{ .optional = .{ .child = child } });
    }

    pub fn promise(self: *TypeStore, payload: TypeRef) !TypeRef {
        return self.create(.{ .promise = .{ .payload = payload } });
    }

    pub fn function(self: *TypeStore, params: []const TypeRef, return_type: TypeRef, is_async: bool) !TypeRef {
        return self.create(.{
            .function = .{
                .params = try self.copyTypeRefs(params),
                .return_type = return_type,
                .is_async = is_async,
            },
        });
    }

    pub fn errorSet(self: *TypeStore, desc: ErrorSetDesc) !TypeRef {
        const variants: []const Type.ErrorSet.Variant = switch (desc.behavior) {
            .any => &.{},
            .closed => try self.copyVariants(desc.variants),
        };

        return self.create(.{
            .error_set = .{
                .behavior = desc.behavior,
                .name = try self.dupeMaybeBytes(desc.name),
                .variants = variants,
            },
        });
    }

    pub fn errorUnion(self: *TypeStore, set: TypeRef, payload: TypeRef) !TypeRef {
        std.debug.assert(switch (set.*) {
            .error_set => true,
            else => false,
        });

        return self.create(.{ .error_union = .{ .set = set, .payload = payload } });
    }

    pub fn processHandle(self: *TypeStore, desc: ProcessHandleDesc) !TypeRef {
        return self.create(.{ .process_handle = .{
            .mode = desc.mode,
            .stdout = desc.stdout,
            .stderr = desc.stderr,
            .status = desc.status,
        } });
    }
};

const TestError = error{UnexpectedType};

fn expectOptional(subject: TypeRef, child: TypeRef) !void {
    switch (subject.*) {
        .optional => |wrapped| try std.testing.expectEqual(child, wrapped.child),
        else => return TestError.UnexpectedType,
    }
}

fn expectPromise(subject: TypeRef, payload: TypeRef) !void {
    switch (subject.*) {
        .promise => |wrapped| try std.testing.expectEqual(payload, wrapped.payload),
        else => return TestError.UnexpectedType,
    }
}

fn expectArrayElement(subject: TypeRef, element: TypeRef) !void {
    switch (subject.*) {
        .array => |arr| try std.testing.expectEqual(element, arr.element),
        else => return TestError.UnexpectedType,
    }
}

test "TypeStore builds wrapper types" {
    var store = TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const arr = try store.array(str);
    try expectArrayElement(arr, str);

    const opt = try store.optional(arr);
    try expectOptional(opt, arr);

    const promise = try store.promise(opt);
    try expectPromise(promise, opt);
}

test "error sets and functions track structure" {
    var store = TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const err_set = try store.errorSet(.{
        .name = "FileError",
        .variants = &.{
            .{ .name = "NotFound", .payload = str },
            .{ .name = "PermissionDenied" },
        },
    });

    switch (err_set.*) {
        .error_set => |set| {
            try std.testing.expectEqual(Type.ErrorSet.Behavior.closed, set.behavior);
            try std.testing.expectEqual(@as(usize, 2), set.variants.len);
            try std.testing.expectEqualStrings("FileError", set.name.?);
            try std.testing.expectEqualStrings("NotFound", set.variants[0].name);
            try std.testing.expectEqualStrings("PermissionDenied", set.variants[1].name);
            try std.testing.expectEqual(str, set.variants[0].payload.?);
            try std.testing.expectEqual(@as(?TypeRef, null), set.variants[1].payload);
        },
        else => return TestError.UnexpectedType,
    }

    const err_union = try store.errorUnion(err_set, str);
    switch (err_union.*) {
        .error_union => |err| {
            try std.testing.expectEqual(err_set, err.set);
            try std.testing.expectEqual(str, err.payload);
        },
        else => return TestError.UnexpectedType,
    }

    const fn_ty = try store.function(&.{str}, err_union, false);
    switch (fn_ty.*) {
        .function => |func| {
            try std.testing.expectEqual(@as(usize, 1), func.params.len);
            try std.testing.expectEqual(str, func.params[0]);
            try std.testing.expectEqual(err_union, func.return_type);
            try std.testing.expect(!func.is_async);
        },
        else => return TestError.UnexpectedType,
    }
}

test "process handles track stream configuration" {
    var store = TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const bytes = try store.primitive(.bytes);
    const status = try store.primitive(.exit_status);
    const stream_array = try store.array(bytes);
    const handle = try store.processHandle(.{
        .mode = .promise_backed,
        .stdout = .{ .payload = stream_array, .delivery = .buffered },
        .stderr = .{ .payload = bytes, .delivery = .streaming },
        .status = status,
    });

    switch (handle.*) {
        .process_handle => |proc| {
            try std.testing.expectEqual(Type.ProcessHandleType.Mode.promise_backed, proc.mode);
            try std.testing.expectEqual(Type.ProcessHandleType.StreamDelivery.buffered, proc.stdout.delivery);
            try std.testing.expectEqual(stream_array, proc.stdout.payload);
            try std.testing.expectEqual(Type.ProcessHandleType.StreamDelivery.streaming, proc.stderr.delivery);
            try std.testing.expectEqual(bytes, proc.stderr.payload);
            try std.testing.expectEqual(status, proc.status);
        },
        else => return TestError.UnexpectedType,
    }
}
