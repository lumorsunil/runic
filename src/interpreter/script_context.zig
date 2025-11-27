const std = @import("std");
const runic = @import("runic");

const ProcessSnapshot = @import("../runtime/command_runner.zig").ProcessSnapshot;

/// ScriptContext tracks bindings introduced by simple declarations so future
/// interpreter stages can reference them. It currently handles string literals,
/// process snapshots, and module function aliases, but intentionally hides the
/// backing storage so richer value types can slide in later without touching
/// the CLI harness.
pub const ScriptContext = struct {
    allocator: std.mem.Allocator,
    bindings: std.ArrayList(Binding),

    const Binding = struct {
        name: []const u8,
        value: Value,
        is_mutable: bool,
    };

    pub const FunctionBinding = struct {
        module_alias: []const u8,
        function_name: []const u8,
    };

    const Value = union(enum) {
        string: []const u8,
        process: ProcessSnapshot,
        function: FunctionBinding,
        string_array: [][]const u8,
        object: ObjectValue,
        type_expr: []const u8,
    };

    pub const ObjectValue = struct {
        entries: []ObjectEntry,

        pub const ObjectEntry = struct {
            key: []const u8,
            value: []const u8,
        };

        pub fn deinit(self: *ObjectValue, allocator: std.mem.Allocator) void {
            for (self.entries) |entry| {
                allocator.free(entry.key);
                allocator.free(entry.value);
            }
            allocator.free(self.entries);
            self.* = undefined;
        }

        pub fn getField(self: *const ObjectValue, name: []const u8) ?[]const u8 {
            for (self.entries) |entry| {
                if (std.mem.eql(u8, entry.key, name)) return entry.value;
            }
            return null;
        }
    };

    pub const BindingError = error{
        DuplicateBinding,
        UnknownBinding,
        ImmutableBinding,
        TypeMismatch,
    } || std.mem.Allocator.Error;

    pub fn init(allocator: std.mem.Allocator) ScriptContext {
        return .{
            .allocator = allocator,
            .bindings = .empty,
        };
    }

    pub fn deinit(self: *ScriptContext) void {
        for (self.bindings.items) |*binding| {
            self.allocator.free(binding.name);
            self.deinitValue(&binding.value);
        }
        self.bindings.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn declareStringBinding(
        self: *ScriptContext,
        name: []const u8,
        value: []const u8,
        is_mutable: bool,
    ) BindingError!void {
        if (self.findBindingIndex(name)) |_| {
            return error.DuplicateBinding;
        }

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);

        const value_owned = try self.allocator.dupe(u8, value);
        errdefer self.allocator.free(value_owned);

        try self.bindings.append(self.allocator, .{
            .name = name_owned,
            .value = .{ .string = value_owned },
            .is_mutable = is_mutable,
        });
    }

    pub fn declareTypeBinding(
        self: *ScriptContext,
        name: []const u8,
        type_expr: []const u8,
        is_mutable: bool,
    ) BindingError!void {
        if (self.findBindingIndex(name)) |_| {
            return error.DuplicateBinding;
        }

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);

        const type_owned = try self.allocator.dupe(u8, type_expr);
        errdefer self.allocator.free(type_owned);

        try self.bindings.append(self.allocator, .{
            .name = name_owned,
            .value = .{ .type_expr = type_owned },
            .is_mutable = is_mutable,
        });
    }

    pub fn declareStringArrayBinding(
        self: *ScriptContext,
        name: []const u8,
        values: []const []const u8,
        is_mutable: bool,
    ) BindingError!void {
        if (self.findBindingIndex(name)) |_| {
            return error.DuplicateBinding;
        }

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);

        var owned_values = try self.allocator.alloc([]const u8, values.len);
        var filled: usize = 0;
        errdefer {
            for (owned_values[0..filled]) |entry| self.allocator.free(entry);
            self.allocator.free(owned_values);
        }

        for (values, 0..) |value, idx| {
            owned_values[idx] = try self.allocator.dupe(u8, value);
            filled += 1;
        }

        try self.bindings.append(self.allocator, .{
            .name = name_owned,
            .value = .{ .string_array = owned_values },
            .is_mutable = is_mutable,
        });
    }

    pub fn declareObjectBinding(
        self: *ScriptContext,
        name: []const u8,
        object: *ObjectValue,
        is_mutable: bool,
    ) BindingError!void {
        if (self.findBindingIndex(name)) |_| {
            object.deinit(self.allocator);
            return error.DuplicateBinding;
        }

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);

        try self.bindings.append(self.allocator, .{
            .name = name_owned,
            .value = .{ .object = object.* },
            .is_mutable = is_mutable,
        });
    }

    pub fn assignStringBinding(self: *ScriptContext, name: []const u8, value: []const u8) BindingError!void {
        const idx = self.findBindingIndex(name) orelse return error.UnknownBinding;
        const value_owned = try self.allocator.dupe(u8, value);
        errdefer self.allocator.free(value_owned);

        const binding = &self.bindings.items[idx];
        if (!binding.is_mutable) return error.ImmutableBinding;
        if (binding.value != .string) {
            self.allocator.free(value_owned);
            return error.TypeMismatch;
        }

        self.allocator.free(binding.value.string);
        binding.value.string = value_owned;
    }

    pub fn declareFunctionBinding(
        self: *ScriptContext,
        name: []const u8,
        module_alias: []const u8,
        function_name: []const u8,
        is_mutable: bool,
    ) BindingError!void {
        if (self.findBindingIndex(name)) |_| return error.DuplicateBinding;

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);

        const alias_owned = try self.allocator.dupe(u8, module_alias);
        errdefer self.allocator.free(alias_owned);

        const function_owned = try self.allocator.dupe(u8, function_name);
        errdefer self.allocator.free(function_owned);

        try self.bindings.append(self.allocator, .{
            .name = name_owned,
            .value = .{ .function = .{
                .module_alias = alias_owned,
                .function_name = function_owned,
            } },
            .is_mutable = is_mutable,
        });
    }

    pub fn declareProcessBinding(
        self: *ScriptContext,
        name: []const u8,
        snapshot: ProcessSnapshot,
        is_mutable: bool,
    ) BindingError!void {
        if (self.findBindingIndex(name)) |_| {
            return error.DuplicateBinding;
        }

        const name_owned = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(name_owned);

        try self.bindings.append(self.allocator, .{
            .name = name_owned,
            .value = .{ .process = snapshot },
            .is_mutable = is_mutable,
        });
    }

    pub fn getStringBinding(self: *const ScriptContext, name: []const u8) ?[]const u8 {
        if (self.findBindingIndex(name)) |idx| {
            const binding = self.bindings.items[idx];
            if (binding.value == .string) return binding.value.string;
        }
        return null;
    }

    pub fn getTypeBinding(self: *const ScriptContext, name: []const u8) ?[]const u8 {
        if (self.findBindingIndex(name)) |idx| {
            const binding = self.bindings.items[idx];
            if (binding.value == .type_expr) return binding.value.type_expr;
        }
        return null;
    }

    pub fn getStringArrayBinding(self: *const ScriptContext, name: []const u8) ?[]const []const u8 {
        if (self.findBindingIndex(name)) |idx| {
            const binding = self.bindings.items[idx];
            if (binding.value == .string_array) return binding.value.string_array;
        }
        return null;
    }

    pub fn getProcessBinding(self: *const ScriptContext, name: []const u8) ?*const ProcessSnapshot {
        if (self.findBindingIndex(name)) |idx| {
            const binding = &self.bindings.items[idx];
            if (binding.value == .process) return &binding.value.process;
        }
        return null;
    }

    pub fn getFunctionBinding(self: *const ScriptContext, name: []const u8) ?FunctionBinding {
        if (self.findBindingIndex(name)) |idx| {
            const binding = self.bindings.items[idx];
            if (binding.value == .function) return binding.value.function;
        }
        return null;
    }

    pub fn getObjectBinding(self: *const ScriptContext, name: []const u8) ?*const ObjectValue {
        if (self.findBindingIndex(name)) |idx| {
            const binding = &self.bindings.items[idx];
            if (binding.value == .object) return &binding.value.object;
        }
        return null;
    }

    fn findBindingIndex(self: *const ScriptContext, name: []const u8) ?usize {
        for (self.bindings.items, 0..) |binding, idx| {
            if (std.mem.eql(u8, binding.name, name)) return idx;
        }
        return null;
    }

    fn deinitValue(self: *ScriptContext, value: *Value) void {
        switch (value.*) {
            .string => |bytes| self.allocator.free(bytes),
            .process => |*snapshot| snapshot.deinit(),
            .function => |fn_binding| {
                self.allocator.free(fn_binding.module_alias);
                self.allocator.free(fn_binding.function_name);
            },
            .string_array => |entries| {
                for (entries) |entry| self.allocator.free(entry);
                self.allocator.free(entries);
            },
            .object => |*object_value| object_value.deinit(self.allocator),
            .type_expr => |ty| self.allocator.free(ty),
        }
    }
};

test "immutable bindings reject reassignment" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    try context.declareStringBinding("greeting", "hello", false);
    try std.testing.expectError(error.ImmutableBinding, context.assignStringBinding("greeting", "updated"));
}

test "mutable bindings accept reassignment" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    try context.declareStringBinding("greeting", "hello", true);
    try context.assignStringBinding("greeting", "updated");
    try std.testing.expectEqualStrings("updated", context.getStringBinding("greeting").?);
}

test "array bindings store independent copies" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    const values = [_][]const u8{ "api", "jobs", "web" };
    try context.declareStringArrayBinding("services", &values, false);

    const stored_opt = context.getStringArrayBinding("services");
    try std.testing.expect(stored_opt != null);
    const stored = stored_opt.?;
    try std.testing.expectEqual(@as(usize, 3), stored.len);
    try std.testing.expectEqualStrings("api", stored[0]);
    try std.testing.expectEqualStrings("web", stored[2]);
}

test "object bindings store copies and expose fields" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    const entries = try gpa.allocator().alloc(ScriptContext.ObjectValue.ObjectEntry, 2);
    entries[0] = .{
        .key = try gpa.allocator().dupe(u8, "api"),
        .value = try gpa.allocator().dupe(u8, "200"),
    };
    entries[1] = .{
        .key = try gpa.allocator().dupe(u8, "jobs"),
        .value = try gpa.allocator().dupe(u8, "350"),
    };
    var object = ScriptContext.ObjectValue{ .entries = entries };

    try context.declareObjectBinding("thresholds", &object, false);
    const binding = context.getObjectBinding("thresholds");
    try std.testing.expect(binding != null);
    try std.testing.expectEqualStrings("200", binding.?.getField("api").?);
}

test "process bindings are retrievable" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    const snapshot = ProcessSnapshot{
        .allocator = gpa.allocator(),
        .stdout = null,
        .stderr = null,
        .status = .{ .ok = true, .exit_code = null, .signal = null, .failed_stage = null },
        .pid = 0,
        .started_at_ns = 0,
        .finished_at_ns = 0,
    };

    try context.declareProcessBinding("handle", snapshot, false);
    const stored = context.getProcessBinding("handle");
    try std.testing.expect(stored != null);
    try std.testing.expect(stored.?.status.ok);
    try std.testing.expect(context.getProcessBinding("missing") == null);
}

test "function bindings are retrievable" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    try context.declareFunctionBinding("ensure", "testing", "ensure", false);
    const binding = context.getFunctionBinding("ensure");
    try std.testing.expect(binding != null);
    try std.testing.expectEqualStrings("testing", binding.?.module_alias);
    try std.testing.expectEqualStrings("ensure", binding.?.function_name);
    try std.testing.expect(context.getFunctionBinding("missing") == null);
}

test "type bindings store expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    try context.declareTypeBinding("Position", "struct { x: Float }", false);
    const stored = context.getTypeBinding("Position");
    try std.testing.expect(stored != null);
    try std.testing.expectEqualStrings("struct { x: Float }", stored.?);
}
