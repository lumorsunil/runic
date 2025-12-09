const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ScriptExecutor = @import("../interpreter/script_executor.zig").ScriptExecutor;
const command_runner = @import("../runtime/command_runner.zig");
const DocumentStore = @import("../document_store.zig").DocumentStore;

const MAX_DOCUMENT_LEN = 4 * 1024 * 1024;

pub const Document = struct {
    path: []const u8,
    source: []const u8,
    ast: ?ast.Script = null,
    parser: parser.Parser,
    script_executor: ?ScriptExecutor = null,
    exitCode: ?command_runner.ExitCode = null,
};

/// Made to be used by an arena allocator
pub const FrontendDocumentStore = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    map: std.StringArrayHashMapUnmanaged(*Document) = .empty,

    pub fn init(allocator: Allocator) FrontendDocumentStore {
        return .{
            .allocator = allocator,
            .arena = .init(allocator),
        };
    }

    pub fn deinit(self: *FrontendDocumentStore) void {
        for (self.map.values()) |document| {
            if (document.script_executor) |*se| se.deinit();
            document.parser.deinit();
        }
        self.arena.deinit();
    }

    pub fn documentStore(self: *FrontendDocumentStore) DocumentStore {
        return .{
            .ptr = self,
            .vtable = vtable,
        };
    }

    const vtable = &DocumentStore.VTable{
        .getSource = FrontendDocumentStore.getSource,
        .getAst = FrontendDocumentStore.getAst,
        .putAst = FrontendDocumentStore.putAst,
        .getParser = FrontendDocumentStore.getParser,
    };

    pub fn getDocument(ptr: *anyopaque, path: []const u8) DocumentStore.Error!*Document {
        const ctx: *FrontendDocumentStore = @ptrCast(@alignCast(ptr));
        return ctx.requestDocument(path) catch |err| return switch (err) {
            error.FileNotFound => DocumentStore.Error.DocumentNotFound,
            else => DocumentStore.Error.GetFailed,
        };
    }

    pub fn getSource(ptr: *anyopaque, path: []const u8) DocumentStore.Error![]const u8 {
        const document = try getDocument(ptr, path);
        return document.source;
    }

    pub fn getAst(ptr: *anyopaque, path: []const u8) DocumentStore.Error!?ast.Script {
        const document = try getDocument(ptr, path);
        return document.ast;
    }

    pub fn putAst(ptr: *anyopaque, path: []const u8, script: ast.Script) DocumentStore.Error!void {
        const document = try getDocument(ptr, path);
        document.ast = script;
    }

    pub fn getParser(ptr: *anyopaque, path: []const u8) DocumentStore.Error!*parser.Parser {
        const document = try getDocument(ptr, path);
        return &document.parser;
    }

    pub fn requestDocument(
        self: *FrontendDocumentStore,
        path: []const u8,
    ) !*Document {
        const resolvedPath = try self.resolvePath(path);
        const entry = try self.map.getOrPut(self.arena.allocator(), resolvedPath);

        if (!entry.found_existing) {
            entry.value_ptr.* = try self.loadDocument(resolvedPath);
        }

        return entry.value_ptr.*;
    }

    pub fn invalidate(self: *FrontendDocumentStore, path: []const u8) !void {
        const resolvedPath = try self.resolvePath(path);
        _ = self.map.swapRemove(resolvedPath);
    }

    fn loadDocument(self: *FrontendDocumentStore, path: []const u8) !*Document {
        const file = try std.fs.openFileAbsolute(path, .{});
        defer file.close();
        var buffer: [512]u8 = undefined;
        var reader = file.reader(&buffer);
        var contentBuffer = try self.arena.allocator().alloc(u8, MAX_DOCUMENT_LEN);
        const bytesRead = try reader.interface.readSliceShort(contentBuffer);
        const document = try self.arena.allocator().create(Document);
        document.* = .{
            .path = path,
            .source = contentBuffer[0..bytesRead],
            .parser = .init(self.allocator, self.documentStore()),
        };

        return document;
    }

    pub fn resolvePath(self: *FrontendDocumentStore, path: []const u8) ![]const u8 {
        if (std.fs.path.isAbsolute(path)) return self.arena.allocator().dupe(u8, path);
        return std.fs.cwd().realpathAlloc(self.arena.allocator(), path) catch |err| {
            switch (err) {
                error.FileNotFound => {
                    std.log.debug("File not found: {s}", .{path});
                },
                else => {},
            }
            return err;
        };
    }
};

pub fn resolveModulePath(
    allocator: Allocator,
    importer: []const u8,
    moduleName: []const u8,
) ![]const u8 {
    const dirname = std.fs.path.dirname(importer) orelse "./";
    const relative = try std.fs.path.join(allocator, &.{ dirname, moduleName });
    defer allocator.free(relative);
    return std.fs.cwd().realpathAlloc(allocator, relative) catch |err| {
        switch (err) {
            error.FileNotFound => {
                std.log.debug("File not found: {s}", .{relative});
            },
            else => {},
        }
        return err;
    };
}
