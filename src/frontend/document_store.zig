const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ExitCode = @import("../runtime/exit_code.zig").ExitCode;
const DocumentStore = @import("../document_store.zig").DocumentStore;

const MAX_DOCUMENT_LEN = 4 * 1024 * 1024;

pub const Document = struct {
    path: []const u8,
    source: []const u8,
    ast: ?ast.Script = null,
    parser: parser.Parser,
    exitCode: ?ExitCode = null,
};

/// Made to be used by an arena allocator
pub const FrontendDocumentStore = struct {
    io: std.Io,
    allocator: Allocator,
    env_map: *std.process.Environ.Map,
    arena: std.heap.ArenaAllocator,
    document_store: DocumentStore = .{ .vtable = vtable },
    map: std.StringArrayHashMapUnmanaged(*Document) = .empty,

    pub fn init(
        io: std.Io,
        allocator: Allocator,
        env_map: *std.process.Environ.Map,
    ) FrontendDocumentStore {
        return .{
            .io = io,
            .allocator = allocator,
            .env_map = env_map,
            .arena = .init(allocator),
        };
    }

    pub fn deinit(self: *FrontendDocumentStore) void {
        for (self.map.values()) |document| {
            document.parser.deinit();
        }
        self.arena.deinit();
    }

    const vtable = &DocumentStore.VTable{
        .getSource = FrontendDocumentStore.getSource,
        .getAst = FrontendDocumentStore.getAst,
        .putAst = FrontendDocumentStore.putAst,
        .getParser = FrontendDocumentStore.getParser,
    };

    pub fn getDocument(doc_store: *DocumentStore, path: []const u8) DocumentStore.Error!*Document {
        const self: *FrontendDocumentStore = @fieldParentPtr("document_store", doc_store);
        return self.requestDocument(path) catch |err| return switch (err) {
            error.FileNotFound => DocumentStore.Error.DocumentNotFound,
            else => DocumentStore.Error.GetFailed,
        };
    }

    pub fn getSource(doc_store: *DocumentStore, path: []const u8) DocumentStore.Error![]const u8 {
        const document = try getDocument(doc_store, path);
        return document.source;
    }

    pub fn getAst(doc_store: *DocumentStore, path: []const u8) DocumentStore.Error!?ast.Script {
        const document = try getDocument(doc_store, path);
        return document.ast;
    }

    pub fn putAst(
        doc_store: *DocumentStore,
        path: []const u8,
        script: ast.Script,
    ) DocumentStore.Error!void {
        const document = try getDocument(doc_store, path);
        document.ast = script;
    }

    pub fn getParser(
        doc_store: *DocumentStore,
        path: []const u8,
    ) DocumentStore.Error!*parser.Parser {
        const document = try getDocument(doc_store, path);
        return &document.parser;
    }

    pub fn requestDocument(
        self: *FrontendDocumentStore,
        path: []const u8,
    ) !*Document {
        if (self.map.get(path)) |document| return document;
        const resolvedPath = try self.resolvePath(path);
        const entry = try self.map.getOrPut(self.arena.allocator(), resolvedPath);

        if (!entry.found_existing) {
            entry.value_ptr.* = try self.loadDocument(resolvedPath);
        }

        return entry.value_ptr.*;
    }

    pub fn putDocument(
        self: *FrontendDocumentStore,
        path: []const u8,
        source: []const u8,
    ) !*Document {
        const owned_path = try self.arena.allocator().dupe(u8, path);
        const entry = try self.map.getOrPut(self.arena.allocator(), owned_path);

        if (!entry.found_existing) {
            const document = try self.arena.allocator().create(Document);
            document.* = .{
                .path = owned_path,
                .source = try self.arena.allocator().dupe(u8, source),
                .parser = .init(self.io, self.allocator, self.env_map, &self.document_store),
            };
            entry.value_ptr.* = document;
        }

        return entry.value_ptr.*;
    }

    pub fn invalidate(self: *FrontendDocumentStore, path: []const u8) !void {
        const resolvedPath = try self.resolvePath(path);
        _ = self.map.swapRemove(resolvedPath);
    }

    fn loadDocument(self: *FrontendDocumentStore, path: []const u8) !*Document {
        const file = try std.Io.Dir.openFileAbsolute(self.io, path, .{});
        defer file.close(self.io);
        var buffer: [512]u8 = undefined;
        var reader = file.reader(self.io, &buffer);
        var contentBuffer = try self.arena.allocator().alloc(u8, MAX_DOCUMENT_LEN);
        const bytesRead = try reader.interface.readSliceShort(contentBuffer);
        const document = try self.arena.allocator().create(Document);
        document.* = .{
            .path = path,
            .source = contentBuffer[0..bytesRead],
            .parser = .init(self.io, self.allocator, self.env_map, &self.document_store),
        };

        return document;
    }

    pub fn resolvePath(self: *FrontendDocumentStore, path: []const u8) ![]const u8 {
        if (path.len > 0 and path[0] == ':') return self.arena.allocator().dupe(u8, path);
        if (std.fs.path.isAbsolute(path)) return self.arena.allocator().dupe(u8, path);
        return std.Io.Dir.cwd().realPathFileAlloc(self.io, path, self.arena.allocator()) catch |err| {
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
    io: std.Io,
    allocator: Allocator,
    importer: []const u8,
    moduleName: []const u8,
) ![]const u8 {
    const dirname = std.fs.path.dirname(importer) orelse "./";
    const relative = try std.fs.path.join(allocator, &.{ dirname, moduleName });
    defer allocator.free(relative);
    // `realPathFileAlloc` returns a sentinel-terminated `[:0]u8`; re-dupe it into a
    // plain slice so callers can free it with a size matching the allocation.
    const real = std.Io.Dir.cwd().realPathFileAlloc(io, relative, allocator) catch |err| {
        switch (err) {
            error.FileNotFound => {
                std.log.debug("File not found: {s}", .{relative});
            },
            else => {},
        }
        return err;
    };
    defer allocator.free(real);
    return try allocator.dupe(u8, real);
}
