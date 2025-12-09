const ast = @import("frontend/ast.zig");
const parser = @import("frontend/parser.zig");

pub const DocumentStore = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const Error = error{ DocumentNotFound, GetFailed };

    pub const VTable = struct {
        getSource: *const fn (ptr: *anyopaque, path: []const u8) Error![]const u8,
        getAst: *const fn (ptr: *anyopaque, path: []const u8) Error!?ast.Script,
        putAst: *const fn (ptr: *anyopaque, path: []const u8, script: ast.Script) Error!void,
        getParser: *const fn (ptr: *anyopaque, path: []const u8) Error!*parser.Parser,
    };

    pub fn getSource(self: DocumentStore, path: []const u8) Error![]const u8 {
        return self.vtable.getSource(self.ptr, path);
    }

    pub fn getAst(self: DocumentStore, path: []const u8) Error!?ast.Script {
        return self.vtable.getAst(self.ptr, path);
    }

    pub fn putAst(self: DocumentStore, path: []const u8, script: ast.Script) Error!void {
        return self.vtable.putAst(self.ptr, path, script);
    }

    pub fn getParser(self: DocumentStore, path: []const u8) Error!*parser.Parser {
        return self.vtable.getParser(self.ptr, path);
    }
};
