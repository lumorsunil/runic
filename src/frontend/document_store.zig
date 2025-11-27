const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ScriptExecutor = @import("../interpreter/script_executor.zig").ScriptExecutor;

const MAX_DOCUMENT_LEN = 4 * 1024 * 1024;

pub const Document = struct {
    path: []const u8,
    source: []const u8,
    ast: ?ast.Script = null,
    parser: Parser,
    script_executor: ?ScriptExecutor = null,
    exitCode: ?u8 = null,
};

/// Made to be used by an arena allocator
pub const DocumentStore = struct {
    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    map: std.StringArrayHashMapUnmanaged(*Document) = .empty,

    pub fn init(allocator: Allocator) DocumentStore {
        return .{
            .allocator = allocator,
            .arena = .init(allocator),
        };
    }

    pub fn deinit(self: *DocumentStore) void {
        for (self.map.values()) |document| {
            if (document.script_executor) |*se| se.deinit();
            document.parser.deinit();
        }
        self.arena.deinit();
    }

    pub fn requestDocument(
        self: *DocumentStore,
        path: []const u8,
    ) !*Document {
        const resolvedPath = try self.resolvePath(path);
        const entry = try self.map.getOrPut(self.arena.allocator(), resolvedPath);

        if (!entry.found_existing) {
            entry.value_ptr.* = try self.loadDocument(resolvedPath);
        }

        return entry.value_ptr.*;
    }

    pub fn invalidate(self: *DocumentStore, path: []const u8) !void {
        const resolvedPath = try self.resolvePath(path);
        _ = self.map.swapRemove(resolvedPath);
    }

    fn loadDocument(self: *DocumentStore, path: []const u8) !*Document {
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
            .parser = .init(self.allocator, self),
        };

        return document;
    }

    fn resolvePath(self: *DocumentStore, path: []const u8) ![]const u8 {
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

pub const Parser = parser.Parser(DocumentStore, getCachedAst, putCachedAst, getSource);

fn getCachedAst(self: *DocumentStore, path: []const u8) !?ast.Script {
    const document = try self.requestDocument(path);
    return document.ast;
}

fn putCachedAst(self: *DocumentStore, path: []const u8, script: ast.Script) !void {
    const document = try self.requestDocument(path);
    document.ast = script;
}

fn getSource(self: *DocumentStore, path: []const u8) ![]const u8 {
    const document = try self.requestDocument(path);
    return document.source;
}

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
