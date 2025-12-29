const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;
const Location = @import("location.zig").Location;
const Labels = @import("labels.zig").Labels;
const Instruction = @import("instruction.zig").Instruction;
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;

pub const page_size = 1024 * 4;
pub const stack_start: usize = std.math.maxInt(usize) - 1024 * 1024 * 10;

pub const ThreadId = usize;

pub const IRProgramContext = struct {
    allocator: Allocator,
    shared: IRSharedContext,
    threads: std.ArrayList(IRThreadContext) = .empty,
    pipe_threads: std.ArrayList(IRThreadContext) = .empty,
    thread_counter: usize = 0,
    threads_to_remove: std.AutoArrayHashMapUnmanaged(ThreadId, void) = .empty,
    pipe_threads_to_remove: std.AutoArrayHashMapUnmanaged(ThreadId, void) = .empty,
    thread_exit_codes: std.AutoArrayHashMapUnmanaged(ThreadId, ExitCode) = .empty,
    thread_id_counter: ThreadId = 0,

    pub fn init(allocator: Allocator, shared: IRSharedContext) @This() {
        return .{ .allocator = allocator, .shared = shared };
    }

    pub fn addMainThread(self: *@This()) Allocator.Error!void {
        _ = try self.spawnThread();
    }

    pub fn dataSize(self: @This()) usize {
        return self.shared.dataSize();
    }

    pub fn readonlyData(self: @This()) []const []const u8 {
        return self.shared.data;
    }

    pub fn structTypes(self: @This()) []const Value.Struct.Type {
        return self.shared.struct_types;
    }

    pub fn instructions(self: @This()) []const Instruction {
        return self.shared.instructions;
    }

    pub fn labels(self: @This()) Labels {
        return self.shared.labels;
    }

    pub fn mapAddr(self: @This(), addr: usize) Location {
        return self.shared.mapAddr(addr);
    }

    fn newThreadId(self: *@This()) ThreadId {
        defer self.thread_id_counter += 1;
        return self.thread_id_counter;
    }

    pub fn getCurrentThread(self: @This()) IRThreadContext {
        return self.threads.items[self.thread_counter];
    }

    pub fn spawnThread(self: *@This()) Allocator.Error!ThreadId {
        const id = self.newThreadId();
        const private = try self.allocator.create(IRPrivateContext);
        private.* = .init();
        try self.threads.append(self.allocator, .init(id, &self.shared, private));
        return id;
    }

    pub fn getThreadContext(self: @This(), id: ThreadId) ?IRThreadContext {
        for (self.threads.items) |tc| {
            if (tc.id == id) return tc;
        }

        return null;
    }

    pub fn closeThread(
        self: *@This(),
        id: ThreadId,
        exit_code: ?ExitCode,
    ) !void {
        const thread = self.getThreadContext(id).?;
        const wait_exit_code = try thread.wait();
        const exit_code_ = exit_code orelse wait_exit_code;
        try self.thread_exit_codes.put(self.allocator, id, exit_code_);
        try self.threads_to_remove.put(self.allocator, id, {});
    }

    pub fn getMainThreadExitCode(self: @This()) ExitCode {
        return self.thread_exit_codes.get(0).?;
    }

    pub const AdvanceEvent = enum { cont, quit };

    pub fn advanceThreadCounter(self: *@This()) AdvanceEvent {
        while (true) {
            self.thread_counter += 1;
            if (self.thread_counter >= self.threads.items.len) break;
            if (self.isThreadActive(self.getCurrentThread().id)) break;
        }
        if (self.thread_counter < self.threads.items.len) return .cont;

        self.removeThreads();
        self.thread_counter = 0;

        if (self.threads.items.len > 0) return .cont;

        return .quit;
    }

    fn isThreadActive(self: @This(), id: ThreadId) bool {
        const thread = self.getThreadContext(id) orelse return false;
        if (self.threads_to_remove.contains(thread.id)) return false;
        self.processWaitingFor(thread);
        if (thread.private.waiting_for != null) return false;
        return true;
    }

    fn processWaitingFor(self: @This(), thread: IRThreadContext) void {
        if (thread.private.waiting_for) |waiting_for| {
            if (self.threads_to_remove.contains(waiting_for)) {
                thread.private.waiting_for = null;
                return;
            }

            for (self.threads.items) |t| if (t.id == waiting_for) {
                return;
            };

            thread.private.waiting_for = null;
        }
    }

    fn removeThreads(self: *@This()) void {
        defer self.threads_to_remove.clearRetainingCapacity();
        for (self.threads_to_remove.keys()) |id| {
            for (self.threads.items, 0..) |item, i| {
                if (item.id == id) {
                    _ = self.threads.swapRemove(i);
                    break;
                }
            }
        }

        defer self.pipe_threads_to_remove.clearRetainingCapacity();
        for (self.pipe_threads_to_remove.keys()) |id| {
            for (self.pipe_threads.items, 0..) |item, i| {
                if (item.id == id) {
                    _ = self.threads.orderedRemove(i);
                    break;
                }
            }
        }
    }
};

pub const IRThreadContext = struct {
    id: ThreadId,
    shared: *IRSharedContext,
    private: *IRPrivateContext,

    pub fn init(
        id: ThreadId,
        shared: *IRSharedContext,
        private: *IRPrivateContext,
    ) @This() {
        return .{
            .id = id,
            .shared = shared,
            .private = private,
        };
    }

    pub fn currentInstruction(self: @This()) ?Instruction {
        if (self.shared.instructions.len <= self.private.instruction_counter) {
            return null;
        }
        return self.shared.instructions[self.private.instruction_counter];
    }

    pub fn waitFor(self: @This(), id: ThreadId) void {
        self.private.waiting_for = id;
    }

    pub fn wait(self: @This()) std.process.Child.WaitError!ExitCode {
        defer self.private.process = null;
        if (self.private.process) |*p| return .fromTerm(try p.wait());
        return .success;
    }
};

const PipeEnd = struct {};

pub const IRPipeThreadContext = struct {
    source: PipeEnd,
    destination: PipeEnd,
};

pub const IRSharedContext = struct {
    data: []const []const u8,
    instructions: []const Instruction,
    labels: Labels,
    struct_types: []const Value.Struct.Type,

    pub fn dataSize(self: @This()) usize {
        if (self.data.len == 0) return 0;
        return (self.data.len - 1) * page_size + self.data[self.data.len - 1].len;
    }

    pub fn mapAddr(self: @This(), addr: usize) Location {
        const data_end = self.dataSize();

        if (addr < data_end) {
            return .{ .data = .fromAddr(addr) };
        } else if (addr >= stack_start) {
            return .{ .stack = stack_start - addr };
        } else {
            return .{ .ref = .{ .addr = addr } };
        }
    }
};

pub const IRPrivateContext = struct {
    instruction_counter: usize = 0,
    stack: std.ArrayList(Value) = .empty,
    refs: std.AutoArrayHashMapUnmanaged(usize, Value) = .empty,
    process: ?std.process.Child = null,
    waiting_for: ?ThreadId = null,

    pub fn init() @This() {
        return .{};
    }
};
