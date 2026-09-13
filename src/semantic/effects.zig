//! Synchrony (effect) analysis.
//!
//! Classifies each named function as **sync** or **threaded**. A function is
//! `sync` when its body can be run to completion in the caller's own thread,
//! producing at most one value, without needing a separate green thread for
//! correctness. Such a call can later be lowered without the fork + closure +
//! pipe + wait + dequeue machinery the general call path emits today (see
//! `future/execution-optimization.md`).
//!
//! A body is `threaded` if it contains any concurrency-requiring construct — a
//! pipeline, a command / external executable, a backgrounded or redirected
//! call, an `&0`/fd stream, a subshell, an `import`, **more than one `yield`, or
//! a `yield` inside a loop** (a generator) — or if it calls another `threaded`
//! or unknown function. The analysis is conservative: anything it can't prove
//! sync is `threaded`.
//!
//! v1 scope: only bare-identifier calls to named functions are resolved. Member
//! calls (`x.pow y`, module functions, C-FFI externs) and indirect calls are
//! treated as `threaded` for now; whitelisting the sync ones (string/float/int
//! builtins, `cimport_call`, sync module functions) is a follow-up done when
//! this is wired into the IR compiler, which has the name resolution for it.

const std = @import("std");
const ast = @import("../frontend/ast.zig");

pub const Effect = enum { sync, threaded };

pub const Analysis = struct {
    allocator: std.mem.Allocator,
    fns: std.StringHashMapUnmanaged(Effect) = .empty,

    pub fn deinit(self: *Analysis) void {
        self.fns.deinit(self.allocator);
    }

    /// The effect of a named function. An unknown name (an external command, an
    /// indirect callee, or a name that isn't a function) is conservatively
    /// `threaded`.
    pub fn effectOf(self: *const Analysis, name: []const u8) Effect {
        return self.fns.get(name) orelse .threaded;
    }

    pub fn isSync(self: *const Analysis, name: []const u8) bool {
        return self.effectOf(name) == .sync;
    }
};

/// Runs the analysis over a whole script. The returned `Analysis` borrows the
/// AST's string slices for its keys, so it must not outlive the script.
pub fn analyze(allocator: std.mem.Allocator, script: *const ast.Script) !Analysis {
    var analysis: Analysis = .{ .allocator = allocator };
    errdefer analysis.deinit();

    // 1. Collect every named function declaration. A name declared more than
    //    once is ambiguous to resolve, so force it `threaded`.
    var decls: std.StringHashMapUnmanaged(*const ast.FunctionDecl) = .empty;
    defer decls.deinit(allocator);
    var duplicated: std.StringHashMapUnmanaged(void) = .empty;
    defer duplicated.deinit(allocator);

    var collector: Collector = .{ .allocator = allocator, .decls = &decls, .duplicated = &duplicated };
    for (script.statements) |stmt| try collector.stmt(stmt);

    // 2. Seed: duplicated names threaded, every other candidate optimistically
    //    sync (so mutual recursion converges instead of poisoning itself).
    var seed = decls.iterator();
    while (seed.next()) |entry| {
        const effect: Effect = if (duplicated.contains(entry.key_ptr.*)) .threaded else .sync;
        try analysis.fns.put(allocator, entry.key_ptr.*, effect);
    }

    // 3. Fixpoint. The lattice is monotone (sync → threaded only), so this
    //    terminates: each pass can only demote, and there are finitely many
    //    functions.
    var changed = true;
    while (changed) {
        changed = false;
        var it = decls.iterator();
        while (it.next()) |entry| {
            if (analysis.effectOf(entry.key_ptr.*) == .threaded) continue;
            if (bodyEffect(&analysis, entry.value_ptr.*) == .threaded) {
                try analysis.fns.put(allocator, entry.key_ptr.*, .threaded);
                changed = true;
            }
        }
    }

    return analysis;
}

/// Collects named function declarations anywhere in the AST, including nested
/// ones (a function declared inside another function's body).
const Collector = struct {
    allocator: std.mem.Allocator,
    decls: *std.StringHashMapUnmanaged(*const ast.FunctionDecl),
    duplicated: *std.StringHashMapUnmanaged(void),

    fn record(self: *Collector, decl: *const ast.FunctionDecl) !void {
        const name = (decl.name orelse return).name;
        const gop = try self.decls.getOrPut(self.allocator, name);
        if (gop.found_existing) {
            try self.duplicated.put(self.allocator, name, {});
        } else {
            gop.value_ptr.* = decl;
        }
    }

    fn stmt(self: *Collector, s: *const ast.Statement) !void {
        switch (s.*) {
            .binding_decl => |d| try self.expr(d.initializer),
            .expression => |e| try self.expr(e.expression),
            .exit_stmt => |e| if (e.value) |v| try self.expr(v),
            .yield_stmt => |y| try self.expr(y.value),
            .while_stmt => |w| {
                try self.expr(w.condition);
                for (w.body.statements) |bs| try self.stmt(bs);
            },
            .type_binding_decl, .bash_block => {},
        }
    }

    fn expr(self: *Collector, e: *const ast.Expression) (std.mem.Allocator.Error)!void {
        switch (e.*) {
            .fn_decl => |*d| {
                try self.record(d);
                try self.expr(d.body);
            },
            .array => |a| for (a.elements) |el| try self.expr(el),
            .map => |m| for (m.entries) |entry| {
                try self.expr(entry.key);
                try self.expr(entry.value);
            },
            .range => |r| {
                try self.expr(r.start);
                if (r.end) |end| try self.expr(end);
            },
            .struct_literal => |sl| {
                if (sl.object) |o| try self.expr(o);
                for (sl.fields) |f| try self.expr(f.value);
            },
            .pipeline => |p| for (p.stages) |st| try self.expr(st),
            .pipeline_deprecated => |p| for (p.stages) |st| switch (st.payload) {
                .expression => |x| try self.expr(x),
                .command => {},
            },
            .call => |c| {
                try self.expr(c.callee);
                for (c.arguments) |arg| try self.expr(arg);
            },
            .member => |m| try self.expr(m.object),
            .index => |i| {
                try self.expr(i.target);
                try self.expr(i.index);
            },
            .unary => |u| try self.expr(u.operand),
            .binary => |b| {
                try self.expr(b.left);
                try self.expr(b.right);
            },
            .block => |b| for (b.statements) |bs| try self.stmt(bs),
            .if_expr => |i| {
                try self.expr(i.condition);
                try self.expr(i.then_expr);
                switch (i.else_branch orelse return) {
                    .expr => |x| try self.expr(x),
                    .if_expr => |nested| try self.expr(@ptrCast(nested)),
                    .condition => {},
                }
            },
            .for_expr => |f| {
                for (f.sources) |src| try self.expr(src);
                try self.expr(f.body);
            },
            .comptime_expr => |c| try self.expr(c.operand),
            .match_expr => |m| {
                try self.expr(m.subject);
                for (m.cases) |case| for (case.body.statements) |bs| try self.stmt(bs);
            },
            .try_expr => |t| try self.expr(t.subject),
            .catch_expr => |c| {
                try self.expr(c.subject);
                try self.expr(c.handler);
            },
            .is_expr => |i| try self.expr(i.subject),
            .assignment => |a| try self.expr(a.expr),
            .subshell => |s| try self.expr(s.child),
            // Leaves and constructs that cannot contain a function declaration.
            .identifier, .env_var, .path, .literal, .fd, .executable, .builtin, .import_expr, .cimport_expr => {},
        }
    }
};

/// Accumulators threaded through the body walk.
const Walk = struct {
    analysis: *const Analysis,
    allocator: std.mem.Allocator,
    /// Value bindings in scope (parameters, locals, captures). A bare identifier
    /// parses as a zero-arg call, so this is what distinguishes a variable read
    /// (sync) from a nullary external command (threaded). Grown in source order
    /// (declare-before-use makes that correct); never shrunk — an over-broad
    /// scope only ever treats more reads as variables, which is safe.
    value_names: *std.StringHashMapUnmanaged(void),
    threaded: bool = false,
    /// A `yield` reached inside a loop body — the function produces a stream of
    /// values (a generator), which needs its own thread.
    yield_in_loop: bool = false,
};

fn bodyEffect(analysis: *const Analysis, decl: *const ast.FunctionDecl) Effect {
    var value_names: std.StringHashMapUnmanaged(void) = .empty;
    defer value_names.deinit(analysis.allocator);
    var w: Walk = .{ .analysis = analysis, .allocator = analysis.allocator, .value_names = &value_names };
    addParamNames(&w, decl.params);
    walkExpr(&w, decl.body, false);
    // More than one value produced on a single path is also a generator. Branches
    // are alternatives (max), sequential statements accumulate (sum), so
    // `if (c) { yield a } else { yield b }` is one value, not two.
    if (w.threaded or w.yield_in_loop or pathYieldsExpr(decl.body) > 1) return .threaded;
    return .sync;
}

/// The maximum number of `yield`s along any single execution path through an
/// expression. Loops contribute 0 here — a yield inside a loop is caught by
/// `yield_in_loop` and makes the function threaded regardless.
fn pathYieldsExpr(e: *const ast.Expression) usize {
    return switch (e.*) {
        .block => |b| blk: {
            var total: usize = 0;
            for (b.statements) |s| total += pathYieldsStmt(s);
            break :blk total;
        },
        .if_expr => |i| blk: {
            const then_y = pathYieldsExpr(i.then_expr);
            const else_y: usize = if (i.else_branch) |eb| switch (eb) {
                .expr => |x| pathYieldsExpr(x),
                .if_expr => |nested| pathYieldsExpr(@ptrCast(nested)),
                .condition => 0,
            } else 0;
            break :blk @max(then_y, else_y);
        },
        .match_expr => |m| blk: {
            var most: usize = 0;
            for (m.cases) |case| {
                var total: usize = 0;
                for (case.body.statements) |s| total += pathYieldsStmt(s);
                most = @max(most, total);
            }
            break :blk most;
        },
        .catch_expr => |c| pathYieldsExpr(c.handler),
        .comptime_expr => |c| pathYieldsExpr(c.operand),
        else => 0,
    };
}

fn pathYieldsStmt(s: *const ast.Statement) usize {
    return switch (s.*) {
        .yield_stmt => 1,
        .expression => |e| pathYieldsExpr(e.expression),
        .binding_decl => |d| pathYieldsExpr(d.initializer),
        else => 0,
    };
}

/// Adds a value-binding name to the in-scope set (best-effort: an allocation
/// failure just leaves the name out, which is conservative).
fn addName(w: *Walk, name: []const u8) void {
    w.value_names.put(w.allocator, name, {}) catch {};
}

fn addPatternNames(w: *Walk, pattern: *const ast.BindingPattern) void {
    switch (pattern.*) {
        .identifier => |id| addName(w, id.name),
        .tuple => |t| for (t.elements) |el| addPatternNames(w, el),
        // Record destructuring is left out for now — a missed name only makes a
        // later read look like a command (threaded), which is safe.
        .record, .discard => {},
    }
}

fn addCaptureNames(w: *Walk, capture: ?ast.CaptureClause) void {
    const c = capture orelse return;
    for (c.bindings) |b| addPatternNames(w, b);
}

fn addParamNames(w: *Walk, params: ast.FunctionDecl.Parameters) void {
    switch (params) {
        ._non_variadic => |ps| for (ps) |p| addPatternNames(w, p.pattern),
        ._variadic => |p| addPatternNames(w, p.pattern),
    }
}

fn callThreaded(w: *const Walk, call: ast.CallExpr) bool {
    if (call.background or call.redirects.len > 0) return true;
    return switch (call.callee.*) {
        .identifier => |id| {
            // A known function inherits its effect.
            if (w.analysis.fns.get(id.name)) |effect| return effect == .threaded;
            // A bare identifier parses as a zero-arg call. If it names a value
            // binding in scope it's a variable read (sync) when called with no
            // args; with args it's an indirect call through that value, which
            // needs a thread. An unknown name is an external command.
            if (w.value_names.contains(id.name)) return call.arguments.len > 0;
            return true;
        },
        // A UFCS method call `recv.method` dispatches by the *name* `method` to a
        // free function (there is no per-type method table), so — exactly like the
        // IR compiler's `tryCompileSyncCall` — a call to a known *sync* user
        // function is itself sync. Anything else (a field access `p.x`, a builtin
        // method like `.len`/`.map`, a module fn, C-FFI) is left conservatively
        // threaded: only demote when the name resolves to a known threaded fn, or
        // stay sync only when it resolves to a known sync fn.
        .binary => |b| blk: {
            if (b.op == .member and b.right.* == .identifier) {
                if (w.analysis.fns.get(b.right.identifier.name)) |effect| break :blk effect == .threaded;
            }
            break :blk true;
        },
        // Other indirect callees are conservatively threaded for now.
        else => true,
    };
}

fn walkExpr(w: *Walk, e: *const ast.Expression, in_loop: bool) void {
    switch (e.*) {
        // Inherently threaded constructs.
        .pipeline, .pipeline_deprecated, .fd, .executable, .builtin, .subshell, .import_expr, .cimport_expr => w.threaded = true,

        .call => |c| {
            if (callThreaded(w, c)) w.threaded = true;
            // Walk the receiver/callee and arguments regardless: a sync callee
            // can still be handed a threaded argument (`f (someCommand)`).
            walkExpr(w, c.callee, in_loop);
            for (c.arguments) |arg| walkExpr(w, arg, in_loop);
        },

        // A nested function *definition* does not make the enclosing body
        // threaded; its own effect is analyzed separately. Do not descend.
        .fn_decl => {},

        .array => |a| for (a.elements) |el| walkExpr(w, el, in_loop),
        .map => |m| for (m.entries) |entry| {
            walkExpr(w, entry.key, in_loop);
            walkExpr(w, entry.value, in_loop);
        },
        .range => |r| {
            walkExpr(w, r.start, in_loop);
            if (r.end) |end| walkExpr(w, end, in_loop);
        },
        .struct_literal => |sl| {
            if (sl.object) |o| walkExpr(w, o, in_loop);
            for (sl.fields) |f| walkExpr(w, f.value, in_loop);
        },
        .member => |m| walkExpr(w, m.object, in_loop),
        .index => |i| {
            walkExpr(w, i.target, in_loop);
            walkExpr(w, i.index, in_loop);
        },
        .unary => |u| walkExpr(w, u.operand, in_loop),
        .binary => |b| {
            walkExpr(w, b.left, in_loop);
            walkExpr(w, b.right, in_loop);
        },
        .block => |b| {
            if (b.background) w.threaded = true;
            for (b.statements) |s| walkStmt(w, s, in_loop);
        },
        .if_expr => |i| {
            walkExpr(w, i.condition, in_loop);
            addCaptureNames(w, i.capture);
            walkExpr(w, i.then_expr, in_loop);
            if (i.else_branch) |eb| switch (eb) {
                .expr => |x| walkExpr(w, x, in_loop),
                .if_expr => |nested| walkExpr(w, @ptrCast(nested), in_loop),
                .condition => {},
            };
        },
        .for_expr => |f| {
            for (f.sources) |src| walkExpr(w, src, in_loop);
            addCaptureNames(w, f.capture);
            walkExpr(w, f.body, true);
        },
        .comptime_expr => |c| walkExpr(w, c.operand, in_loop),
        .match_expr => |m| {
            walkExpr(w, m.subject, in_loop);
            for (m.cases) |case| {
                addCaptureNames(w, case.capture);
                for (case.body.statements) |s| walkStmt(w, s, in_loop);
            }
        },
        .try_expr => |t| walkExpr(w, t.subject, in_loop),
        .catch_expr => |c| {
            walkExpr(w, c.subject, in_loop);
            addCaptureNames(w, c.capture);
            walkExpr(w, c.handler, in_loop);
        },
        .is_expr => |i| walkExpr(w, i.subject, in_loop),
        .assignment => |a| walkExpr(w, a.expr, in_loop),

        // Sync leaves.
        .identifier, .env_var, .path, .literal => {},
    }
}

fn walkStmt(w: *Walk, s: *const ast.Statement, in_loop: bool) void {
    switch (s.*) {
        .binding_decl => |d| {
            walkExpr(w, d.initializer, in_loop);
            addPatternNames(w, d.pattern);
        },
        .expression => |e| walkExpr(w, e.expression, in_loop),
        .exit_stmt => |e| if (e.value) |v| walkExpr(w, v, in_loop),
        .yield_stmt => |y| {
            if (in_loop) w.yield_in_loop = true;
            walkExpr(w, y.value, in_loop);
        },
        .while_stmt => |wh| {
            walkExpr(w, wh.condition, in_loop);
            addCaptureNames(w, wh.capture);
            for (wh.body.statements) |bs| walkStmt(w, bs, true);
        },
        .bash_block => w.threaded = true,
        .type_binding_decl => {},
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

const parser_mod = @import("../frontend/parser.zig");
const FrontendDocumentStore = @import("../frontend/document_store.zig").FrontendDocumentStore;

const TestHarness = struct {
    env: std.process.Environ.Map,
    fds: FrontendDocumentStore,
    parser: parser_mod.Parser,

    fn init(self: *TestHarness, allocator: std.mem.Allocator) void {
        self.env = std.process.Environ.Map.init(allocator);
        self.fds = .init(std.testing.io, allocator, &self.env);
        self.parser = parser_mod.Parser.init(std.testing.io, allocator, &self.env, &self.fds.document_store);
    }

    fn deinit(self: *TestHarness) void {
        self.parser.deinit();
        self.fds.deinit();
        self.env.deinit();
    }
};

fn expectEffects(src: []const u8, expected: []const struct { []const u8, Effect }) !void {
    const allocator = std.testing.allocator;
    var h: TestHarness = undefined;
    h.init(allocator);
    defer h.deinit();

    const script = try h.parser.parseSource(src);
    var analysis = try analyze(allocator, &script);
    defer analysis.deinit();

    for (expected) |want| {
        try std.testing.expectEqual(want[1], analysis.effectOf(want[0]));
    }
}

test "a pure single-yield function is sync" {
    try expectEffects(
        \\fn Void inc(n: Int) Int { yield n + 1 }
    , &.{.{ "inc", .sync }});
}

test "a function that runs a command is threaded" {
    try expectEffects(
        \\fn Void greet() Void { echo "hi" }
    , &.{.{ "greet", .threaded }});
}

test "a function that yields inside a loop is threaded (a generator)" {
    try expectEffects(
        \\fn Void nums() Int { for (0..3) |i| { yield i } }
    , &.{.{ "nums", .threaded }});
}

test "a function that yields more than once is threaded" {
    try expectEffects(
        \\fn Void two() Int { yield 1; yield 2 }
    , &.{.{ "two", .threaded }});
}

test "a pipeline makes a function threaded" {
    try expectEffects(
        \\fn Void run() Int { yield (echo "3" | parseInt) }
    , &.{.{ "run", .threaded }});
}

test "calling a threaded function is threaded; calling a sync one stays sync" {
    try expectEffects(
        \\fn Void leaf(n: Int) Int { yield n + 1 }
        \\fn Void usesSync(n: Int) Int { yield (leaf n) + 1 }
        \\fn Void greet() Void { echo "hi" }
        \\fn Void usesAsync() Void { greet }
    , &.{
        .{ "leaf", .sync },
        .{ "usesSync", .sync },
        .{ "greet", .threaded },
        .{ "usesAsync", .threaded },
    });
}

test "effect propagates transitively through the call graph" {
    try expectEffects(
        \\fn Void a() Void { echo "x" }
        \\fn Void b() Int { a; yield 1 }
        \\fn Void c(n: Int) Int { yield (b) + n }
    , &.{
        .{ "a", .threaded },
        .{ "b", .threaded },
        .{ "c", .threaded },
    });
}

test "mutual recursion of pure functions stays sync" {
    try expectEffects(
        \\fn Void isEven(n: Int) Bool { if (n == 0) { yield true } else { yield (isOdd (n - 1)) } }
        \\fn Void isOdd(n: Int) Bool { if (n == 0) { yield false } else { yield (isEven (n - 1)) } }
    , &.{
        .{ "isEven", .sync },
        .{ "isOdd", .sync },
    });
}

test "a UFCS call (with args) to a sync function is sync; to a threaded one is threaded" {
    // A UFCS call *with arguments* parses as a call with a member callee, so it
    // reaches `callThreaded` and inherits the method's effect by name.
    try expectEffects(
        \\fn Void scale(self: Int, k: Int) Int { yield self * k }
        \\fn Void usesSync(p: Int) Int { yield p.scale 2 }
        \\fn Void shout(self: Int, k: Int) Void { echo "hi" }
        \\fn Void usesThreaded(p: Int) Void { p.shout 2 }
    , &.{
        .{ "scale", .sync },
        .{ "usesSync", .sync },
        .{ "shout", .threaded },
        .{ "usesThreaded", .threaded },
    });
}

test "an unknown function name is conservatively threaded" {
    const allocator = std.testing.allocator;
    var h: TestHarness = undefined;
    h.init(allocator);
    defer h.deinit();
    const script = try h.parser.parseSource("fn Void f() Int { yield 1 }");
    var analysis = try analyze(allocator, &script);
    defer analysis.deinit();
    try std.testing.expectEqual(Effect.threaded, analysis.effectOf("nonexistent"));
    try std.testing.expectEqual(Effect.sync, analysis.effectOf("f"));
}
