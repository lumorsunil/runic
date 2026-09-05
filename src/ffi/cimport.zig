//! Runtime state for an open `cimport` library: the loaded `std.DynLib` handle
//! and each declared extern's resolved address + C signature. Held as a
//! `Closeable` so the library is `dlclose`d at script exit, like any other
//! process resource (see `future/c-ffi.md`).
const std = @import("std");
const CType = @import("ctype.zig").CType;
const ExitCode = @import("../runtime/exit_code.zig").ExitCode;
const Closeable = @import("../closeable.zig").Closeable;

/// One resolved extern function: its symbol name, runtime address, and the C
/// signature used to marshal a call. `symbol`/`params` borrow the IR
/// instruction's memory (which outlives execution); `addr` comes from `dlsym`.
pub const ResolvedExtern = struct {
    symbol: []const u8,
    addr: *anyopaque,
    params: []const CType,
    ret: CType,
};

pub const CImportCloseable = struct {
    allocator: std.mem.Allocator,
    lib: std.DynLib,
    externs: []ResolvedExtern,
    label: []const u8,
    result: ?ExitCode = null,
    closeable: Closeable(ExitCode) = .{ .vtable = &vtable },

    const vtable = Closeable(ExitCode).VTable{
        .close = close,
        .getResult = getResult,
        .getLabel = getLabel,
    };

    /// Looks up a declared extern by symbol name (linear — a cimport block has
    /// few functions). Null if the name was not declared.
    pub fn find(self: *@This(), symbol: []const u8) ?*ResolvedExtern {
        for (self.externs) |*ext| {
            if (std.mem.eql(u8, ext.symbol, symbol)) return ext;
        }
        return null;
    }

    fn close(c: *Closeable(ExitCode)) ExitCode {
        const self: *@This() = @fieldParentPtr("closeable", c);
        if (self.result == null) {
            self.lib.close();
            self.allocator.free(self.externs);
            self.result = .success;
        }
        return self.result.?;
    }

    fn getResult(c: *Closeable(ExitCode)) ?ExitCode {
        const self: *@This() = @fieldParentPtr("closeable", c);
        return self.result;
    }

    fn getLabel(c: *Closeable(ExitCode)) []const u8 {
        const self: *@This() = @fieldParentPtr("closeable", c);
        return self.label;
    }
};
