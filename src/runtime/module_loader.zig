const std = @import("std");
const types = @import("../semantic/types.zig");

/// ModuleLoader resolves `import http from "net/http"` style specs into source
/// files relative to the importing script's directory (or additional search
/// paths) and surfaces the typed export signatures declared in each manifest.
pub const ModuleLoader = struct {
    allocator: std.mem.Allocator,
    store: *types.TypeStore,
    config: Config,
    modules: std.StringHashMapUnmanaged(*Module) = .{},

    pub const Error = std.mem.Allocator.Error ||
        std.fs.File.OpenError ||
        std.fs.File.ReadError ||
        std.json.ParseError ||
        error{
            InvalidSpec,
            ModuleNotFound,
            ManifestNotFound,
            ManifestTooLarge,
            ManifestInvalid,
            UnknownType,
        };

    pub const Config = struct {
        source_extension: []const u8 = ".rn",
        manifest_suffix: []const u8 = ".module.json",
        manifest_max_bytes: usize = 1 << 20,
    };

    pub fn init(allocator: std.mem.Allocator, store: *types.TypeStore, config: Config) !ModuleLoader {
        return .{
            .allocator = allocator,
            .store = store,
            .config = .{
                .source_extension = try dupe(allocator, config.source_extension),
                .manifest_suffix = try dupe(allocator, config.manifest_suffix),
                .manifest_max_bytes = config.manifest_max_bytes,
            },
            .modules = .{},
        };
    }

    pub fn deinit(self: *ModuleLoader) void {
        var it = self.modules.iterator();
        while (it.next()) |entry| {
            const module = entry.value_ptr.*;
            module.deinit(self.allocator);
            self.allocator.destroy(module);
        }
        self.modules.deinit(self.allocator);
        self.allocator.free(self.config.source_extension);
        self.allocator.free(self.config.manifest_suffix);
        self.* = undefined;
    }

    pub fn load(self: *ModuleLoader, importer_dir: []const u8, spec_literal: []const u8) Error!*Module {
        const spec = try self.normalizeSpec(spec_literal);
        var spec_cleanup = true;
        defer if (spec_cleanup) self.allocator.free(spec);

        const source_path = try self.buildPath(importer_dir, spec, self.config.source_extension);
        var source_cleanup = true;
        defer if (source_cleanup) self.allocator.free(source_path);

        if (self.modules.get(source_path)) |module| {
            return module;
        }

        try self.ensureFileExists(source_path);

        const manifest_path = try self.appendSuffix(source_path, self.config.manifest_suffix);
        var manifest_cleanup = true;
        defer if (manifest_cleanup) self.allocator.free(manifest_path);

        const manifest_bytes = self.readManifest(manifest_path) catch |err| switch (err) {
            error.FileNotFound => return Error.ManifestNotFound,
            error.FileTooBig => return Error.ManifestTooLarge,
            else => return err,
        };
        defer self.allocator.free(manifest_bytes);

        const parsed = try std.json.parseFromSlice(std.json.Value, self.allocator, manifest_bytes, .{});
        defer parsed.deinit();

        var module = try self.allocator.create(Module);
        errdefer self.allocator.destroy(module);
        module.* = .{
            .spec = spec,
            .source_path = source_path,
            .manifest_path = manifest_path,
            .exports = &[_]Module.Export{},
        };
        spec_cleanup = false;
        source_cleanup = false;
        manifest_cleanup = false;
        errdefer module.deinit(self.allocator);

        try self.populateExports(module, parsed.value);

        try self.modules.put(self.allocator, module.source_path, module);
        return module;
    }

    fn populateExports(self: *ModuleLoader, module: *Module, root_value: std.json.Value) Error!void {
        const root = try expectObject(root_value, "manifest root");
        const exports_value = try expectField(root, "exports");
        const exports = try expectArray(exports_value, "exports");
        if (exports.items.len == 0) {
            module.exports = &[_]Module.Export{};
            return;
        }

        var buffer = try self.allocator.alloc(Module.Export, exports.items.len);
        var buffer_cleanup = true;
        defer if (buffer_cleanup) self.allocator.free(buffer);

        var initialized: usize = 0;
        module.exports = buffer[0..0];

        for (exports.items) |entry| {
            buffer[initialized] = try self.parseExport(entry);
            initialized += 1;
            module.exports = buffer[0..initialized];
            buffer_cleanup = false;
        }
    }

    fn parseExport(self: *ModuleLoader, entry: std.json.Value) Error!Module.Export {
        const obj = try expectObject(entry, "export");
        const kind = try expectStringField(obj, "kind");
        if (std.mem.eql(u8, kind, "function")) {
            return Module.Export{ .function = try self.parseFunctionExport(obj) };
        } else if (std.mem.eql(u8, kind, "value")) {
            return Module.Export{ .value = try self.parseValueExport(obj) };
        }
        return Error.ManifestInvalid;
    }

    fn parseFunctionExport(self: *ModuleLoader, obj: std.json.Object) Error!Module.FunctionExport {
        var fn_export = Module.FunctionExport{
            .name = &[_]u8{},
            .params = &[_]Module.FunctionParam{},
            .return_type = undefined,
            .is_async = false,
        };
        errdefer self.destroyFunctionExport(&fn_export);

        const name = try expectStringField(obj, "name");
        fn_export.name = try dupe(self.allocator, name);

        if (obj.get("is_async")) |value| {
            fn_export.is_async = switch (value) {
                .bool => |flag| flag,
                else => return Error.ManifestInvalid,
            };
        }

        const params_value = try expectField(obj, "params");
        const params_array = try expectArray(params_value, "params");
        if (params_array.items.len == 0) {
            fn_export.params = &[_]Module.FunctionParam{};
        } else {
            var params_buf = try self.allocator.alloc(Module.FunctionParam, params_array.items.len);
            errdefer self.allocator.free(params_buf);

            var params_filled: usize = 0;
            fn_export.params = params_buf[0..0];

            for (params_array.items) |param_entry| {
                const param_obj = try expectObject(param_entry, "function parameter");
                const param_name = try expectStringField(param_obj, "name");
                const type_node = try expectField(param_obj, "type");
                const param_type = try self.parseType(type_node);

                params_buf[params_filled] = .{
                    .name = try dupe(self.allocator, param_name),
                    .ty = param_type,
                };
                params_filled += 1;
                fn_export.params = params_buf[0..params_filled];
            }
        }

        const return_value = try expectField(obj, "return_type");
        fn_export.return_type = try self.parseType(return_value);

        return fn_export;
    }

    fn parseValueExport(self: *ModuleLoader, obj: std.json.Object) Error!Module.ValueExport {
        var value_export = Module.ValueExport{
            .name = &[_]u8{},
            .ty = undefined,
        };
        errdefer self.destroyValueExport(&value_export);

        const name = try expectStringField(obj, "name");
        value_export.name = try dupe(self.allocator, name);

        const type_node = try expectField(obj, "type");
        value_export.ty = try self.parseType(type_node);
        return value_export;
    }

    fn parseType(self: *ModuleLoader, node: std.json.Value) Error!types.TypeRef {
        const obj = try expectObject(node, "type descriptor");
        const kind = try expectStringField(obj, "kind");
        if (std.mem.eql(u8, kind, "primitive")) {
            const tag_name = try expectStringField(obj, "name");
            const primitive = std.meta.stringToEnum(types.Type.Primitive, tag_name) orelse
                return Error.UnknownType;
            return self.store.primitive(primitive);
        } else if (std.mem.eql(u8, kind, "array")) {
            const element_value = try expectField(obj, "element");
            const element_type = try self.parseType(element_value);
            return self.store.array(element_type);
        } else if (std.mem.eql(u8, kind, "map")) {
            const key_value = try expectField(obj, "key");
            const value_value = try expectField(obj, "value");
            const key_type = try self.parseType(key_value);
            const value_type = try self.parseType(value_value);
            return self.store.map(key_type, value_type);
        } else if (std.mem.eql(u8, kind, "optional")) {
            const child_value = try expectField(obj, "child");
            const child_type = try self.parseType(child_value);
            return self.store.optional(child_type);
        } else if (std.mem.eql(u8, kind, "promise")) {
            const payload_value = try expectField(obj, "payload");
            const payload = try self.parseType(payload_value);
            return self.store.promise(payload);
        } else if (std.mem.eql(u8, kind, "struct")) {
            const fields_value = try expectField(obj, "fields");
            const fields_array = try expectArray(fields_value, "fields");
            if (fields_array.items.len == 0) {
                return self.store.structure(.{});
            }

            var field_descs = try self.allocator.alloc(types.StructFieldDesc, fields_array.items.len);
            defer self.allocator.free(field_descs);

            for (fields_array.items, 0..) |field_entry, idx| {
                const field_obj = try expectObject(field_entry, "struct field");
                const field_name = try expectStringField(field_obj, "name");
                const field_type_value = try expectField(field_obj, "type");
                const field_type = try self.parseType(field_type_value);
                field_descs[idx] = .{
                    .name = field_name,
                    .ty = field_type,
                };
            }

            return self.store.structure(.{ .fields = field_descs[0..fields_array.items.len] });
        }

        return Error.ManifestInvalid;
    }

    fn ensureFileExists(self: *ModuleLoader, path: []const u8) Error!void {
        var file = std.fs.cwd().openFile(path, .{}) catch |err| switch (err) {
            error.FileNotFound => return Error.ModuleNotFound,
            else => return err,
        };
        defer file.close();
        _ = self;
    }

    fn appendSuffix(self: *ModuleLoader, base: []const u8, suffix: []const u8) ![]u8 {
        var builder = std.ArrayList(u8).init(self.allocator);
        errdefer builder.deinit();
        try builder.appendSlice(base);
        try builder.appendSlice(suffix);
        return builder.toOwnedSlice();
    }

    fn buildPath(
        self: *ModuleLoader,
        importer_dir: []const u8,
        spec: []const u8,
        extension: []const u8,
    ) ![]u8 {
        var builder = std.ArrayList(u8).init(self.allocator);
        errdefer builder.deinit();

        if (importer_dir.len > 0) {
            try builder.appendSlice(importer_dir);
        }

        if (spec.len > 0) {
            if (builder.items.len > 0 and builder.items[builder.items.len - 1] != std.fs.path.sep) {
                try builder.append(std.fs.path.sep);
            }
            for (spec) |ch| {
                if (ch == '/') {
                    try builder.append(std.fs.path.sep);
                } else {
                    try builder.append(ch);
                }
            }
        }

        try builder.appendSlice(extension);
        return builder.toOwnedSlice();
    }

    fn readManifest(self: *ModuleLoader, path: []const u8) ![]u8 {
        return std.fs.cwd().readFileAlloc(self.allocator, path, self.config.manifest_max_bytes);
    }

    fn normalizeSpec(self: *ModuleLoader, literal: []const u8) Error![]u8 {
        const trimmed = std.mem.trim(u8, literal, " \r\n\t");
        if (trimmed.len == 0) return Error.InvalidSpec;

        var builder = std.ArrayList(u8).init(self.allocator);
        errdefer builder.deinit();

        var index: usize = 0;
        while (index < trimmed.len) {
            const next = std.mem.indexOfScalarPos(u8, trimmed, index, '/') orelse trimmed.len;
            const segment = trimmed[index..next];
            if (segment.len == 0) return Error.InvalidSpec;
            if (std.mem.eql(u8, segment, ".") or std.mem.eql(u8, segment, "..")) {
                return Error.InvalidSpec;
            }
            if (!segmentAllowed(segment)) return Error.InvalidSpec;
            if (builder.items.len > 0) try builder.append('/');
            try builder.appendSlice(segment);
            if (next == trimmed.len) break;
            index = next + 1;
            if (index == trimmed.len) return Error.InvalidSpec;
        }

        return builder.toOwnedSlice();
    }
};

pub const Module = struct {
    spec: []const u8,
    source_path: []const u8,
    manifest_path: []const u8,
    exports: []Module.Export,

    pub const Export = union(enum) {
        function: FunctionExport,
        value: ValueExport,
    };

    pub const FunctionExport = struct {
        name: []const u8,
        params: []const FunctionParam,
        return_type: types.TypeRef,
        is_async: bool,
    };

    pub const FunctionParam = struct {
        name: []const u8,
        ty: types.TypeRef,
    };

    pub const ValueExport = struct {
        name: []const u8,
        ty: types.TypeRef,
    };

    pub fn deinit(self: *Module, allocator: std.mem.Allocator) void {
        allocator.free(self.spec);
        allocator.free(self.source_path);
        allocator.free(self.manifest_path);
        for (self.exports) |*export_ptr| {
            destroyExport(allocator, export_ptr);
        }
        if (self.exports.len > 0) {
            allocator.free(self.exports);
        }
        self.* = undefined;
    }
};

fn destroyExport(allocator: std.mem.Allocator, export_ptr: *Module.Export) void {
    switch (export_ptr.*) {
        .function => |*fn_export| destroyFunctionExport(allocator, fn_export),
        .value => |*value_export| destroyValueExport(allocator, value_export),
    }
}

fn destroyFunctionExport(allocator: std.mem.Allocator, fn_export: *Module.FunctionExport) void {
    allocator.free(fn_export.name);
    for (fn_export.params) |param| {
        allocator.free(param.name);
    }
    if (fn_export.params.len > 0) {
        allocator.free(fn_export.params);
    }
}

fn destroyValueExport(allocator: std.mem.Allocator, value_export: *Module.ValueExport) void {
    allocator.free(value_export.name);
}

fn expectObject(value: std.json.Value, context: []const u8) ModuleLoader.Error!std.json.ObjectMap {
    _ = context;
    return switch (value) {
        .object => |obj| obj,
        else => ModuleLoader.Error.ManifestInvalid,
    };
}

fn expectArray(value: std.json.Value, context: []const u8) ModuleLoader.Error!std.json.Array {
    _ = context;
    return switch (value) {
        .array => |arr| arr,
        else => ModuleLoader.Error.ManifestInvalid,
    };
}

fn expectStringField(obj: std.json.ObjectMap, field: []const u8) ModuleLoader.Error![]const u8 {
    const value = try expectField(obj, field);
    return switch (value) {
        .string => |slice| slice,
        else => ModuleLoader.Error.ManifestInvalid,
    };
}

fn expectField(obj: std.json.ObjectMap, field: []const u8) ModuleLoader.Error!std.json.Value {
    return obj.get(field) orelse ModuleLoader.Error.ManifestInvalid;
}

fn segmentAllowed(segment: []const u8) bool {
    for (segment) |ch| {
        if (ch == '/' or ch == '\\') return false;
        if (std.ascii.isAlphanumeric(ch)) continue;
        switch (ch) {
            '_', '-', '.' => continue,
            else => return false,
        }
    }
    return true;
}

fn dupe(allocator: std.mem.Allocator, slice: []const u8) ![]u8 {
    const buf = try allocator.alloc(u8, slice.len);
    std.mem.copy(u8, buf, slice);
    return buf;
}

test "module loader resolves typed APIs from manifest" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.makePath("src/net");
    {
        var file = try tmp.dir.createFile("src/net/http.rn", .{});
        defer file.close();
        try file.writeAll("// stub\n");
    }
    {
        var manifest = try tmp.dir.createFile("src/net/http.rn.module.json", .{});
        defer manifest.close();
        try manifest.writeAll(
            \\{
            \\  "exports": [
            \\    {
            \\      "kind": "function",
            \\      "name": "get",
            \\      "is_async": true,
            \\      "params": [
            \\        { "name": "url", "type": { "kind": "primitive", "name": "string" } },
            \\        {
            \\          "name": "headers",
            \\          "type": {
            \\            "kind": "map",
            \\            "key": { "kind": "primitive", "name": "string" },
            \\            "value": { "kind": "primitive", "name": "string" }
            \\          }
            \\        }
            \\      ],
            \\      "return_type": {
            \\        "kind": "optional",
            \\        "child": { "kind": "primitive", "name": "bytes" }
            \\      }
            \\    },
            \\    {
            \\      "kind": "value",
            \\      "name": "default_headers",
            \\      "type": {
            \\        "kind": "array",
            \\        "element": { "kind": "primitive", "name": "string" }
            \\      }
            \\    },
            \\    {
            \\      "kind": "value",
            \\      "name": "result_type",
            \\      "type": {
            \\        "kind": "struct",
            \\        "fields": [
            \\          { "name": "value", "type": { "kind": "primitive", "name": "float" } },
            \\          { "name": "ok", "type": { "kind": "primitive", "name": "bool" } }
            \\        ]
            \\      }
            \\    }
            \\  ]
            \\}
        );
    }

    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const root_path = try tmp.dir.realpathAlloc(std.testing.allocator, "src");
    defer std.testing.allocator.free(root_path);

    var loader = try ModuleLoader.init(std.testing.allocator, &store, .{});
    defer loader.deinit();

    const module = try loader.load(root_path, "net/http");
    try std.testing.expectEqualStrings("net/http", module.spec);

    const expected_source = try std.fmt.allocPrint(
        std.testing.allocator,
        "{s}{c}net{c}http.rn",
        .{ root_path, std.fs.path.sep, std.fs.path.sep },
    );
    defer std.testing.allocator.free(expected_source);
    try std.testing.expectEqualStrings(expected_source, module.source_path);

    const expected_manifest = try std.fmt.allocPrint(
        std.testing.allocator,
        "{s}.module.json",
        .{expected_source},
    );
    defer std.testing.allocator.free(expected_manifest);
    try std.testing.expectEqualStrings(expected_manifest, module.manifest_path);

    try std.testing.expectEqual(@as(usize, 3), module.exports.len);

    const export_get = module.exports[0];
    try std.testing.expect(std.meta.activeTag(export_get) == .function);
    const fn_get = export_get.function;
    try std.testing.expectEqualStrings("get", fn_get.name);
    try std.testing.expect(fn_get.is_async);
    try std.testing.expectEqual(@as(usize, 2), fn_get.params.len);

    const url_param = fn_get.params[0];
    try std.testing.expectEqualStrings("url", url_param.name);
    try std.testing.expect(std.meta.activeTag(url_param.ty.*) == .primitive);
    try std.testing.expect(url_param.ty.*.primitive == types.Type.Primitive.string);

    const headers_param = fn_get.params[1];
    try std.testing.expect(std.meta.activeTag(headers_param.ty.*) == .map);
    try std.testing.expect(std.meta.activeTag(headers_param.ty.*.map.key.*) == .primitive);
    try std.testing.expect(headers_param.ty.*.map.key.*.primitive == types.Type.Primitive.string);
    try std.testing.expect(std.meta.activeTag(headers_param.ty.*.map.value.*) == .primitive);

    try std.testing.expect(std.meta.activeTag(fn_get.return_type.*) == .optional);
    const optional_child = fn_get.return_type.*.optional.child;
    try std.testing.expect(optional_child.*.primitive == types.Type.Primitive.bytes);

    const export_value = module.exports[1];
    try std.testing.expect(std.meta.activeTag(export_value) == .value);
    const default_headers = export_value.value;
    try std.testing.expectEqualStrings("default_headers", default_headers.name);
    try std.testing.expect(std.meta.activeTag(default_headers.ty.*) == .array);
    try std.testing.expect(std.meta.activeTag(default_headers.ty.*.array.element.*) == .primitive);
    try std.testing.expect(default_headers.ty.*.array.element.*.primitive == types.Type.Primitive.string);

    const export_struct = module.exports[2];
    try std.testing.expect(std.meta.activeTag(export_struct) == .value);
    const result_value = export_struct.value;
    try std.testing.expectEqualStrings("result_type", result_value.name);
    try std.testing.expect(std.meta.activeTag(result_value.ty.*) == .structure);
    const struct_info = result_value.ty.*.structure;
    try std.testing.expectEqual(@as(usize, 2), struct_info.fields.len);
    try std.testing.expectEqualStrings("value", struct_info.fields[0].name);
    try std.testing.expect(std.meta.activeTag(struct_info.fields[0].ty.*) == .primitive);
    try std.testing.expect(struct_info.fields[0].ty.*.primitive == types.Type.Primitive.float);
    try std.testing.expectEqualStrings("ok", struct_info.fields[1].name);
    try std.testing.expect(std.meta.activeTag(struct_info.fields[1].ty.*) == .primitive);
    try std.testing.expect(struct_info.fields[1].ty.*.primitive == types.Type.Primitive.bool);

    const module_again = try loader.load(root_path, "net/http");
    try std.testing.expectEqual(module, module_again);
}

test "module loader rejects invalid specs and missing manifests" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const root = try std.fs.cwd().realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(root);

    var loader = try ModuleLoader.init(std.testing.allocator, &store, .{});
    defer loader.deinit();

    try std.testing.expectError(ModuleLoader.Error.InvalidSpec, loader.load(root, ""));
    try std.testing.expectError(ModuleLoader.Error.InvalidSpec, loader.load(root, "../net/http"));
    try std.testing.expectError(ModuleLoader.Error.InvalidSpec, loader.load(root, "net//http"));

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.makePath("src/fs");
    {
        var file = try tmp.dir.createFile("src/fs/temp.rn", .{});
        defer file.close();
        try file.writeAll("// stub\n");
    }
    const root_path = try tmp.dir.realpathAlloc(std.testing.allocator, "src");
    defer std.testing.allocator.free(root_path);

    var isolated_loader = try ModuleLoader.init(std.testing.allocator, &store, .{});
    defer isolated_loader.deinit();
    try std.testing.expectError(ModuleLoader.Error.ManifestNotFound, isolated_loader.load(root_path, "fs/temp"));
}

test "module loader resolves relative to importer directory" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.makePath("alpha/util");
    try tmp.dir.makePath("beta/util");

    {
        var file = try tmp.dir.createFile("alpha/util/math.rn", .{});
        defer file.close();
        try file.writeAll("// alpha\n");
    }
    {
        var manifest = try tmp.dir.createFile("alpha/util/math.rn.module.json", .{});
        defer manifest.close();
        try manifest.writeAll(
            \\{ "exports": [{ "kind": "value", "name": "alpha", "type": { "kind": "primitive", "name": "int" } }] }
        );
    }
    {
        var file = try tmp.dir.createFile("beta/util/math.rn", .{});
        defer file.close();
        try file.writeAll("// beta\n");
    }
    {
        var manifest = try tmp.dir.createFile("beta/util/math.rn.module.json", .{});
        defer manifest.close();
        try manifest.writeAll(
            \\{ "exports": [{ "kind": "value", "name": "beta", "type": { "kind": "primitive", "name": "int" } }] }
        );
    }

    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    var loader = try ModuleLoader.init(std.testing.allocator, &store, .{});
    defer loader.deinit();

    const alpha_dir = try tmp.dir.realpathAlloc(std.testing.allocator, "alpha");
    defer std.testing.allocator.free(alpha_dir);
    const beta_dir = try tmp.dir.realpathAlloc(std.testing.allocator, "beta");
    defer std.testing.allocator.free(beta_dir);

    const alpha_module = try loader.load(alpha_dir, "util/math");
    const beta_module = try loader.load(beta_dir, "util/math");

    try std.testing.expect(alpha_module != beta_module);
    try std.testing.expect(!std.mem.eql(u8, alpha_module.source_path, beta_module.source_path));
    try std.testing.expect(std.mem.indexOf(u8, alpha_module.source_path, "alpha") != null);
    try std.testing.expect(std.mem.indexOf(u8, beta_module.source_path, "beta") != null);
}
