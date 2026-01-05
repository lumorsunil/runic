const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;
const Location = @import("location.zig").Location;
const Labels = @import("labels.zig").Labels;
const Instruction = @import("instruction.zig").Instruction;
const ResolvedInstructionAddr = @import("instruction-addr.zig").ResolvedInstructionAddr;
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

    pub fn instructions(self: @This()) []const []const Instruction {
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
        self.thread_counter += 1;
        if (self.getNextActiveThread()) |thread_counter| {
            self.thread_counter = thread_counter;
            return .cont;
        }

        self.removeThreadsSlatedToBeRemoved();

        self.thread_counter = 0;
        if (self.getNextActiveThread()) |thread_counter| {
            self.thread_counter = thread_counter;
            return .cont;
        }

        if (self.threads.items.len > 0) return .cont;

        return .quit;
    }

    fn getNextActiveThread(self: @This()) ?usize {
        var i = self.thread_counter;

        while (true) : (i += 1) {
            if (i >= self.threads.items.len) return null;
            if (self.isThreadActive(self.threads.items[i].id)) return i;
        }
    }

    pub fn isThreadActive(self: @This(), id: ThreadId) bool {
        const thread = self.getThreadContext(id) orelse return false;
        if (self.threads_to_remove.contains(thread.id)) return false;
        self.processWaitingFor(thread);
        if (thread.private.waiting_for != null) return false;
        return true;
    }

    pub fn isThreadClosed(self: @This(), id: ThreadId) bool {
        return self.getThreadContext(id) == null or self.threads_to_remove.contains(id);
    }

    fn processWaitingFor(self: @This(), thread: IRThreadContext) void {
        if (thread.private.waiting_for) |waiting_for| {
            if (self.isThreadClosed(waiting_for)) {
                thread.private.waiting_for = null;
            }
        }
    }

    fn removeThreadsSlatedToBeRemoved(self: *@This()) void {
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

    pub fn getCurrentInstructionAddr(self: @This()) ResolvedInstructionAddr {
        return self.private.instruction_counter;
    }

    fn getInstructionSet(self: @This(), addr: ResolvedInstructionAddr) []const Instruction {
        return self.shared.instructions[addr.instr_set];
    }

    fn getCurrentInstructionSet(self: @This()) []const Instruction {
        return self.getInstructionSet(self.getCurrentInstructionAddr());
    }

    pub fn getInstruction(self: @This(), addr: ResolvedInstructionAddr) Instruction {
        return self.getInstructionSet(addr)[addr.local_addr];
    }

    pub fn currentInstruction(self: @This()) ?Instruction {
        if (self.getCurrentInstructionSet().len <= self.getCurrentInstructionAddr().local_addr) {
            return null;
        }
        return self.getInstruction(self.getCurrentInstructionAddr());
    }

    pub fn incInstructionCounter(self: @This()) void {
        self.private.instruction_counter.inc();
    }

    pub fn setInstructionCounter(self: @This(), instr_counter: ResolvedInstructionAddr) void {
        self.private.instruction_counter = instr_counter;
    }

    pub fn waitFor(self: @This(), id: ThreadId) void {
        self.private.waiting_for = id;
    }

    pub fn wait(self: @This()) std.process.Child.WaitError!ExitCode {
        defer self.private.process = null;
        // TODO: translate wait error to exit code
        if (self.private.process) |p| return .fromTerm(try p.wait());
        return .success;
    }

    pub fn refs(self: @This()) *std.AutoArrayHashMapUnmanaged(usize, Value) {
        return &self.shared.refs;
    }

    pub fn getRefValue(self: @This(), addr: usize) Value {
        return self.shared.refs.get(addr).?;
    }
};

const PipeEnd = struct {};

pub const IRPipeThreadContext = struct {
    source: PipeEnd,
    destination: PipeEnd,
};

pub const IRSharedContext = struct {
    data: []const []const u8,
    instructions: []const []const Instruction,
    labels: Labels,
    struct_types: []const Value.Struct.Type,
    refs: std.AutoArrayHashMapUnmanaged(usize, Value) = .empty,

    pub fn dataSize(self: @This()) usize {
        if (self.data.len == 0) return 0;
        return (self.data.len - 1) * page_size + self.data[self.data.len - 1].len;
    }

    pub fn mapAddr(self: @This(), addr: usize) Location {
        const data_end = self.dataSize();

        if (addr < data_end) {
            return .{ .data = .fromAddr(addr) };
        } else if (addr >= stack_start) {
            return .{ .stack = addr - stack_start };
        } else {
            return .{ .ref = .{ .addr = addr } };
        }
    }
};

pub const IRPrivateContext = struct {
    instruction_counter: ResolvedInstructionAddr = .init(0, 0),
    stack: std.ArrayList(Value) = .empty,
    process: ?*std.process.Child = null,
    waiting_for: ?ThreadId = null,

    pub fn init() @This() {
        return .{};
    }
};
