const std = @import("std");

var signals = Signals{};

const sig_action = std.posix.Sigaction{
    .handler = .{ .handler = handler },
    .mask = std.posix.sigemptyset(),
    .flags = 0,
};

const Signals = struct {
    trapped: u256 = 0,
    tripped: u256 = 0,
    original_handlers: std.AutoArrayHashMapUnmanaged(u8, std.posix.Sigaction) = .empty,
    allocator: std.mem.Allocator = undefined,

    pub fn trap(self: *Signals, sig: u8) void {
        self.trapped |= @as(u256, 1) << sig;
    }

    pub fn untrap(self: *Signals, sig: u8) void {
        self.trapped &= ~(@as(u256, 1) << sig);
    }

    pub fn trip(self: *Signals, sig: u8) void {
        self.tripped |= @as(u256, 1) << sig;
    }

    pub fn untrip(self: *Signals, sig: u8) bool {
        std.debug.assert(self.trapped & @as(u256, 1) << sig > 0);
        defer self.tripped &= ~(@as(u256, 1) << sig);
        return self.tripped & @as(u256, 1) << sig > 0;
    }

    pub fn trop(self: *Signals, sig: u8, original_handler: std.posix.Sigaction) void {
        self.original_handlers.put(self.allocator, sig, original_handler) catch {};
    }

    pub fn untrop(self: *Signals, sig: u8) std.posix.Sigaction {
        return self.original_handlers.fetchSwapRemove(sig).?.value;
    }
};

pub fn init(allocator: std.mem.Allocator) void {
    signals.allocator = allocator;
}

pub fn consume(sig: u8) bool {
    return signals.untrip(sig);
}

fn handler(sig: c_int) callconv(.c) void {
    signals.trip(@intCast(sig));
}

pub fn trap(sig: u8) void {
    var original_handler: std.posix.Sigaction = undefined;
    std.posix.sigaction(sig, &sig_action, &original_handler);
    signals.trap(sig);
    signals.trop(sig, original_handler);
}

pub fn untrap(sig: u8) void {
    signals.untrap(sig);
    const original_handler = signals.untrop(sig);
    std.posix.sigaction(sig, &original_handler, null);
}
