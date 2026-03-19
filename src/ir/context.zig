const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;
const Location = @import("location.zig").Location;
const LocationMod = @import("location.zig").LocationMod;
const Labels = @import("labels.zig").Labels;
const Instruction = @import("instruction.zig").Instruction;
const ResolvedInstructionAddr = @import("instruction-addr.zig").ResolvedInstructionAddr;
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;
const Stream = @import("../stream.zig").Stream;
const ReaderWriterStream = @import("../stream.zig").ReaderWriterStream;
const Closeable = @import("../closeable.zig").Closeable;
const Ref = @import("ref.zig").Ref;
const FileSink = @import("../process.zig").FileSink;

pub const page_size = 1024 * 4;
pub const stack_start: usize = std.math.maxInt(usize) - 1024 * 1024 * 10;

pub const ThreadId = usize;
pub const PipeHandle = usize;
pub const CloseableHandle = usize;
pub const SubshellContextHandle = usize;

pub const SubshellContext = struct {
    /// null = inherit the OS process's working directory (no cd has been issued)
    cwd: ?[]const u8 = null,
};

pub const Error = error{
    MissingThreadContext,
    MissingMainThreadExitCode,
    MissingPipeHandle,
    MissingCloseableHandle,
};

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

    pipes: std.AutoArrayHashMapUnmanaged(PipeHandle, *ReaderWriterStream) = .empty,
    pipe_handle_counter: usize = 0,
    closeables: std.AutoArrayHashMapUnmanaged(CloseableHandle, *Closeable(ExitCode)) = .empty,
    closeable_handle_counter: usize = 0,
    file_sinks: std.ArrayList(*FileSink) = .empty,

    subshell_contexts: std.AutoArrayHashMapUnmanaged(SubshellContextHandle, SubshellContext) = .empty,
    subshell_context_handle_counter: SubshellContextHandle = 0,

    pub fn init(allocator: Allocator, shared: IRSharedContext) @This() {
        return .{ .allocator = allocator, .shared = shared };
    }

    pub fn deinit(self: *@This()) void {
        for (self.threads.items) |thread| {
            thread.private.stack.deinit(self.allocator);
            thread.private.subshell_context_stack.deinit(self.allocator);
            self.allocator.destroy(thread.private);
        }
        self.threads.deinit(self.allocator);

        for (self.pipe_threads.items) |thread| {
            thread.private.stack.deinit(self.allocator);
            thread.private.subshell_context_stack.deinit(self.allocator);
            self.allocator.destroy(thread.private);
        }
        self.pipe_threads.deinit(self.allocator);

        for (self.pipes.keys(), self.pipes.values()) |handle, pipe| {
            if (handle < 3) {
                pipe.disconnectSourcesAll();
                pipe.disconnectDestination();
                continue;
            }
            pipe.deinitParent();
        }
        self.pipes.deinit(self.allocator);

        for (self.closeables.values()) |closeable| {
            if (!closeable.isClosed()) _ = closeable.close();
        }
        self.closeables.deinit(self.allocator);

        for (self.file_sinks.items) |file_sink| {
            if (!file_sink.closeable.isClosed()) _ = file_sink.closeable.close();
            file_sink.deinit(self.allocator);
        }
        self.file_sinks.deinit(self.allocator);

        self.threads_to_remove.deinit(self.allocator);
        self.pipe_threads_to_remove.deinit(self.allocator);
        self.thread_exit_codes.deinit(self.allocator);
        self.shared.heap.deinit(self.allocator);
        self.subshell_contexts.deinit(self.allocator);
    }

    pub fn addMainThread(self: *@This()) Allocator.Error!void {
        const initial_ctx = try self.addSubshellContext(.{});
        _ = try self.spawnThread(initial_ctx);
    }

    pub fn addSubshellContext(self: *@This(), ctx: SubshellContext) Allocator.Error!SubshellContextHandle {
        const handle = self.subshell_context_handle_counter;
        self.subshell_context_handle_counter += 1;
        try self.subshell_contexts.put(self.allocator, handle, ctx);
        return handle;
    }

    pub fn getSubshellContextPtr(self: *@This(), handle: SubshellContextHandle) *SubshellContext {
        return self.subshell_contexts.getPtr(handle).?;
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

    pub fn getCurrentThread(self: @This()) ?IRThreadContext {
        if (self.threads.items.len <= self.thread_counter) return null;
        return self.threads.items[self.thread_counter];
    }

    pub fn spawnThread(self: *@This(), subshell_context: SubshellContextHandle) Allocator.Error!ThreadId {
        const id = self.newThreadId();
        const private = try self.allocator.create(IRPrivateContext);
        private.* = .init(subshell_context);
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
    ) (Allocator.Error || Error || std.process.Child.WaitError)!void {
        const thread = self.getThreadContext(id) orelse return Error.MissingThreadContext;
        const wait_exit_code = try thread.wait();
        const exit_code_ = exit_code orelse wait_exit_code;
        try self.thread_exit_codes.put(self.allocator, id, exit_code_);
        try self.threads_to_remove.put(self.allocator, id, {});
    }

    pub fn getMainThreadExitCode(self: @This()) Error!ExitCode {
        return self.thread_exit_codes.get(0) orelse Error.MissingMainThreadExitCode;
    }

    pub const AdvanceEvent = enum { cont, quit };

    pub fn advanceThreadCounter(self: *@This()) AdvanceEvent {
        self.thread_counter += 1;
        if (self.getNextActiveThread(null)) |thread_counter| {
            self.thread_counter = thread_counter;
            return .cont;
        }

        self.removeThreadsSlatedToBeRemoved();

        self.thread_counter = 0;
        if (self.getNextActiveThread(null)) |thread_counter| {
            self.thread_counter = thread_counter;
            return .cont;
        }

        if (self.threads.items.len > 0) return .cont;

        return .quit;
    }

    pub fn getNextActiveThread(self: @This(), i: ?usize) ?usize {
        var i_ = i orelse self.thread_counter;

        while (true) : (i_ += 1) {
            if (i_ >= self.threads.items.len) return null;
            if (self.isThreadActive(self.threads.items[i_].id)) return i_;
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

    pub fn addPipe(
        self: *@This(),
        pipe: *ReaderWriterStream,
    ) !PipeHandle {
        const handle = self.pipe_handle_counter;
        self.pipe_handle_counter += 1;
        try self.pipes.put(self.allocator, handle, pipe);
        return handle;
    }

    pub fn getPipe(self: *@This(), handle: PipeHandle) Error!*ReaderWriterStream {
        return self.pipes.get(handle) orelse Error.MissingPipeHandle;
    }

    pub fn addCloseable(
        self: *@This(),
        closeable: *Closeable(ExitCode),
    ) !CloseableHandle {
        const handle = self.closeable_handle_counter;
        self.closeable_handle_counter += 1;
        try self.closeables.put(self.allocator, handle, closeable);
        return handle;
    }

    pub fn getCloseable(self: *@This(), handle: CloseableHandle) Error!*Closeable(ExitCode) {
        return self.closeables.get(handle) orelse Error.MissingCloseableHandle;
    }

    pub fn addFileSink(self: *@This(), file_sink: *FileSink) !void {
        try self.file_sinks.append(self.allocator, file_sink);
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

    pub fn getRefPtr(
        self: @This(),
        ref: Ref,
        mod: ?LocationMod,
    ) *Value {
        const mod_: LocationMod = mod orelse .empty;
        return &self.private.stack.items[self.private.stack_frame + mod_.apply(ref.rel_stack_addr)];
    }

    pub fn setRef(self: @This(), ref: Ref, mod: ?LocationMod, value: Value) void {
        self.getRefPtr(ref, mod).* = value;
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
    heap: std.AutoArrayHashMapUnmanaged(usize, Value) = .empty,
    current_heap_addr: usize,

    pub fn dataSize(self: @This()) usize {
        if (self.data.len == 0) return 0;
        return (self.data.len - 1) * page_size + self.data[self.data.len - 1].len;
    }

    pub fn mapAddr(self: @This(), addr: usize) Location {
        const data_end = self.dataSize();

        if (addr < data_end) {
            return .initAbs(.{ .data = .fromAddr(addr) }, .{});
        } else if (addr >= stack_start) {
            return .initAbs(.{ .stack = addr - stack_start }, .{});
        } else {
            return .initAbs(.{ .heap = addr }, .{});
        }
    }

    pub fn alloc(self: *@This(), allocator: Allocator, size: usize) !Value {
        defer self.current_heap_addr += size;
        for (self.current_heap_addr..self.current_heap_addr + size) |addr| {
            try self.heap.put(allocator, addr, .void);
        }
        return .fromAddr(self.current_heap_addr);
    }
};

pub const IRPrivateContext = struct {
    instruction_counter: ResolvedInstructionAddr = .init(0, 0),
    stack: std.ArrayList(Value) = .empty,
    stack_frame: usize = 0,
    result_register: Value = .void,
    result_register_2: Value = .void,
    process: ?*std.process.Child = null,
    waiting_for: ?ThreadId = null,
    subshell_context: SubshellContextHandle = 0,
    /// Saved handles for nested subshell enter/exit
    subshell_context_stack: std.ArrayListUnmanaged(SubshellContextHandle) = .empty,

    pub fn init(subshell_context: SubshellContextHandle) @This() {
        return .{ .subshell_context = subshell_context };
    }
};

test "context reports missing handles and exit code explicitly" {
    const allocator = std.testing.allocator;
    var context = IRProgramContext.init(allocator, .{
        .data = &.{},
        .instructions = &.{},
        .labels = .init(),
        .struct_types = &.{},
        .current_heap_addr = 0,
    });
    defer context.deinit();

    try std.testing.expectError(Error.MissingPipeHandle, context.getPipe(0));
    try std.testing.expectError(Error.MissingCloseableHandle, context.getCloseable(0));
    try std.testing.expectError(Error.MissingMainThreadExitCode, context.getMainThreadExitCode());
    try std.testing.expectError(Error.MissingThreadContext, context.closeThread(999, null));
}
