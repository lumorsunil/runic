const ast = @import("frontend/ast.zig");
const parser = @import("frontend/parser.zig");

pub const DocumentStore = struct {
    vtable: *const VTable,

    pub const Error = error{ DocumentNotFound, GetFailed };

    pub const VTable = struct {
        getSource: *const fn (self: *DocumentStore, path: []const u8) Error![]const u8,
        getAst: *const fn (self: *DocumentStore, path: []const u8) Error!?ast.Script,
        putAst: *const fn (self: *DocumentStore, path: []const u8, script: ast.Script) Error!void,
        getParser: *const fn (self: *DocumentStore, path: []const u8) Error!*parser.Parser,
    };

    pub fn getSource(self: *DocumentStore, path: []const u8) Error![]const u8 {
        return self.vtable.getSource(self, path);
    }

    pub fn getAst(self: *DocumentStore, path: []const u8) Error!?ast.Script {
        return self.vtable.getAst(self, path);
    }

    pub fn putAst(self: *DocumentStore, path: []const u8, script: ast.Script) Error!void {
        return self.vtable.putAst(self, path, script);
    }

    pub fn getParser(self: *DocumentStore, path: []const u8) Error!*parser.Parser {
        return self.vtable.getParser(self, path);
    }
};
