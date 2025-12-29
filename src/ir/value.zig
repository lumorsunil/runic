const std = @import("std");
const Allocator = std.mem.Allocator;
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;
const endian = @import("constants.zig").endian;
const TypeAddr = @import("type-addr.zig").TypeAddr;
const Location = @import("location.zig").Location;

pub const Value = union(enum) {
    void,
    uinteger: usize,
    slice: Slice,
    strct: Struct,
    exit_code: ExitCode,
    addr: usize,
    stream: []const Value,

    pub fn deinit(self: *@This(), allocator: Allocator) void {
        switch (self.*) {
            .void, .location, .uinteger, .slice, .exit_code, .addr => {},
            .strct => |strct| strct.deinit(allocator),
            .stream => |stream| allocator.free(stream),
        }
    }

    pub const Slice = struct {
        addr: usize,
        element_size: usize,
        len: usize,

        pub fn empty(element_size: usize) @This() {
            return Slice{
                .addr = 0,
                .element_size = element_size,
                .len = 0,
            };
        }

        pub fn size() usize {
            return @sizeOf(usize) + @sizeOf(usize);
        }

        pub fn serialize(self: @This(), w: *std.Io.Writer) !void {
            try w.writeInt(usize, self.addr, endian);
            try w.writeInt(usize, self.len, endian);
        }

        pub fn deserialize(
            element_size: usize,
            r: *std.Io.Reader,
        ) std.Io.Reader.Error!@This() {
            const addr = try r.takeInt(usize, endian);
            const len = try r.takeInt(usize, endian);

            return .{
                .addr = addr,
                .element_size = element_size,
                .len = len,
            };
        }

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("[{x}..+{}]u8", .{
                self.addr,
                self.len,
            });
        }
    };

    pub const Struct = struct {
        type: usize,
        fields: []const Value,

        pub fn deinit(self: @This(), allocator: Allocator) void {
            for (self.fields) |*field| {
                field.deinit(allocator);
            }
            allocator.free(self.fields);
        }

        pub fn size(self: Struct) usize {
            return self.type.size();
        }

        pub fn serialize(self: Struct, w: *std.Io.Writer) std.Io.Writer.Error!void {
            for (self.fields) |field| {
                try field.serialize(w);
            }
        }

        pub fn deserialize(
            context: DeserializeStructContext,
            struct_type: usize,
            r: *std.Io.Reader,
        ) !@This() {
            const type_ = context.get(struct_type);
            const fields = try context.allocFields(struct_type);

            for (fields, type_.fields.values()) |*field, field_type_addr| {
                field.* = try switch (field_type_addr) {
                    .struct_type => |staddr| Struct.deserialize(context, staddr, r),
                    .value => |t| Value.deserialize(t, r),
                };
            }

            return .{
                .type = type_,
                .fields = fields,
            };
        }

        pub const DeserializeStructContext = struct {
            allocator: Allocator,
            struct_types: []const Type,

            pub fn get(self: @This(), struct_type: usize) Type {
                return self.struct_types[struct_type];
            }

            pub fn allocFields(self: @This(), struct_type: usize) ![]Value {
                const type_ = self.get(struct_type);
                return self.allocator.alloc(Value, type_.fields.count());
            }
        };

        pub const Type = struct {
            name: []const u8,
            decls: std.StringArrayHashMapUnmanaged(Decl) = .empty,
            fields: std.StringArrayHashMapUnmanaged(TypeAddr) = .empty,

            pub fn size(self: @This()) usize {
                return @reduce(.Add, self.fields.values());
            }

            pub const Decl = union(enum) {
                function: Function,

                pub const Function = struct {
                    signature: Signature,
                    addr: usize,

                    pub const Signature = struct {
                        params: []const Param,

                        pub const Param = struct { name: []const u8 };
                    };
                };
            };
        };
    };

    pub const refSize: usize = @sizeOf(Location);

    pub fn fromAddr(addr: usize) @This() {
        return .{ .addr = addr };
    }

    pub fn toBytes(self: @This()) []const u8 {
        return std.mem.toBytes(self);
    }

    pub fn fromBytes(bytes: []const u8) @This() {
        return std.mem.bytesToValue(@This(), bytes);
    }

    pub const ToStreamError = Allocator.Error || error{UnsupportedStreamCast};

    pub fn toStream(self: @This(), allocator: Allocator) ToStreamError!@This() {
        return switch (self) {
            .stream => self,
            .slice => .{ .stream = try allocator.dupe(Value, &.{self}) },
            else => ToStreamError.UnsupportedStreamCast,
        };
    }

    pub const DeserializeError = std.Io.Reader.Error || error{UnsupportedDeserialize};

    pub fn deserialize(
        tag: std.meta.Tag(@This()),
        r: *std.Io.Reader,
    ) DeserializeError!@This() {
        return switch (tag) {
            .void => .void,
            .uinteger => .{ .uinteger = try r.takeInt(usize, endian) },
            .addr => .{ .addr = try r.takeInt(usize, endian) },
            .stream => .{ .stream = std.mem.bytesAsValue(
                []Value,
                try r.takeArray(@sizeOf([]Value)),
            ).* },
            .slice, .strct => DeserializeError.UnsupportedDeserialize,
            .exit_code => .{ .exit_code = try ExitCode.deserialize(r) },
        };
    }

    pub fn serialize(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .void => {},
            .stream => |stream| try w.writeAll(&std.mem.toBytes(stream)),
            inline .uinteger => |t| try w.writeInt(@TypeOf(t), t, endian),
            inline else => |t| {
                if (std.meta.hasMethod(@TypeOf(t), "serialize")) {
                    return t.serialize(w);
                } else {
                    return w.writeAll(&std.mem.toBytes(self));
                }
            },
        }
    }

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .void => try w.writeAll("void"),
            .addr => |addr| try w.print("{x}", .{addr}),
            .stream => try w.writeAll("<stream>"),
            inline .slice, .exit_code => |s| try w.print("{f}", .{s}),
            inline else => |t| try w.print("{}", .{t}),
        }
    }
};
