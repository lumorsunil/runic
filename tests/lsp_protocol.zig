const std = @import("std");
const lsp = @import("runic_lsp");

const Allocator = std.mem.Allocator;

const ProtocolResponse = struct {
    id: i64,
    body: []const u8,
};

const ServerRunResult = struct {
    stdout: []const u8,
    stderr: []const u8,
    /// Documents type-checked by the final recheck pass (open/client docs only).
    recheck_count: usize = 0,
    /// Total documents in the store after the run (open + transitively imported).
    doc_count: usize = 0,

    fn deinit(self: ServerRunResult, allocator: Allocator) void {
        allocator.free(self.stdout);
        allocator.free(self.stderr);
    }
};

test "lsp formatting preserves comments" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1 # keep
        \\if (foo) {
        \\echo foo
        \\}
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo = 1 # keep
            \\if (foo) {
            \\echo foo
            \\}
            \\
        ),
        try makeFormattingRequest(allocator, 1, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 1);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const edits = parsed.value.object.get("result").?.array.items;
    try std.testing.expect(edits.len > 0);
    const new_text = edits[0].object.get("newText").?.string;
    // The trailing comment survives verbatim.
    try std.testing.expect(std.mem.indexOf(u8, new_text, "# keep") != null);
    // The body of the `if` block is indented one level (four spaces).
    try std.testing.expect(std.mem.indexOf(u8, new_text, "\n    echo foo\n") != null);
    // The closing brace returns to column zero.
    try std.testing.expect(std.mem.indexOf(u8, new_text, "\n}\n") != null);
}

test "lsp formatting is indentation-only and string/comment safe" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // Braces inside a string and a comment must not shift indentation, and
    // command-argument spacing inside a line must be preserved verbatim.
    const source =
        \\fn Void run() Void {
        \\echo "a { brace } in a string"   "two   spaces"
        \\const arr = .{
        \\"x",
        \\}
        \\}
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeFormattingRequest(allocator, 1, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 1);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const edits = parsed.value.object.get("result").?.array.items;
    try std.testing.expect(edits.len > 0);
    const new_text = edits[0].object.get("newText").?.string;

    // The `echo` line sits at one level; the brace inside the string did not
    // push a further level (the array element `"x"` is still at two levels).
    try std.testing.expect(std.mem.indexOf(u8, new_text, "\n    echo \"a { brace } in a string\"   \"two   spaces\"\n") != null);
    // The `.{` array literal indents its element one level deeper.
    try std.testing.expect(std.mem.indexOf(u8, new_text, "\n        \"x\",\n") != null);
    // Interior command-argument spacing is preserved (not reflowed).
    try std.testing.expect(std.mem.indexOf(u8, new_text, "\"two   spaces\"") != null);
}

test "lsp document symbols include real ranges" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\const bar = foo
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\const bar = foo
            \\
        ),
        try makeDocumentSymbolRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const symbols_value = parsed.value.object.get("result").?;
    try std.testing.expect(symbols_value.array.items.len >= 2);
    const first = symbols_value.array.items[0].object;
    const range = first.get("range").?.object;
    const start = range.get("start").?.object;
    const end = range.get("end").?.object;

    try std.testing.expectEqual(@as(i64, 0), start.get("line").?.integer);
    try std.testing.expectEqual(@as(i64, 6), start.get("character").?.integer);
    try std.testing.expect(end.get("character").?.integer > start.get("character").?.integer);
}

test "lsp document symbols include top-level functions" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const budget = 50
        \\fn Void greet(name: String) Void {
        \\    echo "hi ${name}"
        \\}
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeDocumentSymbolRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const syms = parsed.value.object.get("result").?.array.items;
    var saw_budget = false;
    var greet_kind: ?i64 = null;
    for (syms) |s| {
        const name = s.object.get("name").?.string;
        if (std.mem.eql(u8, name, "budget")) saw_budget = true;
        if (std.mem.eql(u8, name, "greet")) greet_kind = s.object.get("kind").?.integer;
    }

    try std.testing.expect(saw_budget);
    // The function appears in the outline with LSP SymbolKind.Function (12).
    try std.testing.expectEqual(@as(?i64, 12), greet_kind);
}

test "lsp document symbols surface destructured binding names" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // Tuple, record (with a rebinding), a `_` discard, and a nested pattern.
    const source =
        \\const P = struct { x: Int, y: Int }
        \\const a, b = .{ 1, 2 }
        \\const { x, y: height } = P{ .x = 1, .y = 2 }
        \\const first, _, third = .{ 7, 8, 9 }
        \\const { x: (nx) }, rest = .{ P{ .x = 5, .y = 6 }, 9 }
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeDocumentSymbolRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const syms = parsed.value.object.get("result").?.array.items;
    const want = [_][]const u8{ "a", "b", "x", "height", "first", "third", "nx", "rest" };
    var seen = [_]bool{false} ** want.len;
    var saw_discard = false;
    for (syms) |s| {
        const name = s.object.get("name").?.string;
        if (std.mem.eql(u8, name, "_")) saw_discard = true;
        for (want, &seen) |w, *hit| {
            if (std.mem.eql(u8, name, w)) hit.* = true;
        }
    }
    for (seen, want) |hit, w| {
        if (!hit) {
            std.debug.print("missing destructured symbol: {s}\n", .{w});
            return error.MissingSymbol;
        }
    }
    // A `_` discard binds nothing, so it must not appear.
    try std.testing.expect(!saw_discard);
}

test "lsp signature help shows the callee signature and active parameter" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\fn Void greet(name: String, times: Int) Void { echo "${name}" }
        \\greet "hi" 3
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Cursor inside the first argument ("hi") on line 1.
        try makeSignatureHelpRequest(allocator, 2, uri, 1, 7),
        // Cursor on the second argument (3).
        try makeSignatureHelpRequest(allocator, 3, uri, 1, 11),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // First arg -> the label lists both params and the active one is 0.
    {
        const response = try findResponseById(allocator, output, 2);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        const result = parsed.value.object.get("result").?.object;
        const sig = result.get("signatures").?.array.items[0].object;
        const label = sig.get("label").?.string;
        try std.testing.expect(std.mem.indexOf(u8, label, "name: String") != null);
        try std.testing.expect(std.mem.indexOf(u8, label, "times: Int") != null);
        try std.testing.expectEqual(@as(i64, 0), result.get("activeParameter").?.integer);
    }
    // Second arg -> active parameter advances to 1.
    {
        const response = try findResponseById(allocator, output, 3);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        const result = parsed.value.object.get("result").?.object;
        try std.testing.expectEqual(@as(i64, 1), result.get("activeParameter").?.integer);
    }
}

test "lsp code action wraps an undeclared uppercase type as a type parameter" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // `T` is an undeclared uppercase type; the checker flags it and suggests
    // writing it as `|T|`. The client passes that diagnostic back in context.
    const source =
        \\fn Void map(xs: []T) []T { yield xs }
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    // The `T` in `[]T` is at line 0, characters 18-19.
    const message = "type 'T' is not declared (to introduce a generic type parameter, write it as |T|)";
    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeCodeActionWithDiagnostic(allocator, 2, uri, 0, 18, 19, message),
    };
    defer for (messages) |message_bytes| allocator.free(message_bytes);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    var wrap: ?std.json.Value = null;
    for (parsed.value.object.get("result").?.array.items) |a| {
        if (std.mem.eql(u8, a.object.get("title").?.string, "Introduce type parameter |T|")) wrap = a;
    }
    try std.testing.expect(wrap != null);
    // Two inserts of `|`: one at the start of `T`, one at its end.
    const edits = wrap.?.object.get("edit").?.object.get("documentChanges").?.array
        .items[0].object.get("edits").?.array.items;
    try std.testing.expectEqual(@as(usize, 2), edits.len);
    try std.testing.expectEqualStrings("|", edits[0].object.get("newText").?.string);
    try std.testing.expectEqualStrings("|", edits[1].object.get("newText").?.string);
    try std.testing.expectEqual(@as(i64, 18), edits[0].object.get("range").?.object.get("start").?.object.get("character").?.integer);
    try std.testing.expectEqual(@as(i64, 19), edits[1].object.get("range").?.object.get("start").?.object.get("character").?.integer);
}

test "lsp code action changes a lowercase type to the suggested capitalized one" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // `int` is a lowercase type name — a parse error; the checker suggests `Int`.
    // (This exercises the diagnostic-linked path even without a valid AST.)
    const source =
        \\fn Void f(x: int) Void { echo "hi" }
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    // `int` is at line 0, characters 13-16.
    const message = "expected type identifier (did you mean Int?)";
    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeCodeActionWithDiagnostic(allocator, 2, uri, 0, 13, 16, message),
    };
    defer for (messages) |message_bytes| allocator.free(message_bytes);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    var change: ?std.json.Value = null;
    for (parsed.value.object.get("result").?.array.items) |a| {
        if (std.mem.eql(u8, a.object.get("title").?.string, "Change to 'Int'")) change = a;
    }
    try std.testing.expect(change != null);
    const edit = change.?.object.get("edit").?.object.get("documentChanges").?.array
        .items[0].object.get("edits").?.array.items[0].object;
    try std.testing.expectEqualStrings("Int", edit.get("newText").?.string);
    // Replaces the exact `int` range (chars 13-16).
    try std.testing.expectEqual(@as(i64, 13), edit.get("range").?.object.get("start").?.object.get("character").?.integer);
    try std.testing.expectEqual(@as(i64, 16), edit.get("range").?.object.get("end").?.object.get("character").?.integer);
}

test "lsp call hierarchy resolves callers and callees" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\fn Void helper(x: Int) Int { yield x }
        \\fn Void run() Void {
        \\  const a = helper 1
        \\  const b = helper 2
        \\  echo "${a}${b}"
        \\}
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Prepare on `helper`'s declaration name (line 0).
        try makePrepareCallHierarchyRequest(allocator, 2, uri, 0, 10),
        try makeCallHierarchyItemRequest(allocator, 3, "callHierarchy/incomingCalls", uri, "helper"),
        try makeCallHierarchyItemRequest(allocator, 4, "callHierarchy/outgoingCalls", uri, "run"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // prepare -> one item named `helper`.
    {
        const response = try findResponseById(allocator, output, 2);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        const items = parsed.value.object.get("result").?.array.items;
        try std.testing.expectEqual(@as(usize, 1), items.len);
        try std.testing.expectEqualStrings("helper", items[0].object.get("name").?.string);
    }
    // incoming(helper) -> `run` calls it twice.
    {
        const response = try findResponseById(allocator, output, 3);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        const calls = parsed.value.object.get("result").?.array.items;
        try std.testing.expectEqual(@as(usize, 1), calls.len);
        try std.testing.expectEqualStrings("run", calls[0].object.get("from").?.object.get("name").?.string);
        try std.testing.expectEqual(@as(usize, 2), calls[0].object.get("fromRanges").?.array.items.len);
    }
    // outgoing(run) -> calls helper twice.
    {
        const response = try findResponseById(allocator, output, 4);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        const calls = parsed.value.object.get("result").?.array.items;
        try std.testing.expectEqual(@as(usize, 1), calls.len);
        try std.testing.expectEqualStrings("helper", calls[0].object.get("to").?.object.get("name").?.string);
        try std.testing.expectEqual(@as(usize, 2), calls[0].object.get("fromRanges").?.array.items.len);
    }
}

test "lsp call hierarchy finds cross-file incoming calls" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const lib_uri = try fixture.writeDocument("lib.rn",
        \\pub fn Int greet(x: Int) Int { yield x }
        \\
    );
    defer allocator.free(lib_uri);
    // An importer that calls `m.greet` — only indexed, never opened.
    const main_uri = try fixture.writeDocument("main.rn",
        \\const m = import "./lib.rn"
        \\fn Void run() Void {
        \\  const a = m.greet 1
        \\  echo "${a}"
        \\}
        \\
    );
    defer allocator.free(main_uri);

    const root_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{fixture.root_path});
    defer allocator.free(root_uri);

    const messages = [_][]const u8{
        try makeInitializeWithRoot(allocator, 1, root_uri),
        try makeDidOpen(allocator, lib_uri,
            \\pub fn Int greet(x: Int) Int { yield x }
            \\
        ),
        // Prepare on `greet`'s declaration, then ask for its incoming calls.
        try makePrepareCallHierarchyRequest(allocator, 2, lib_uri, 0, 12),
        try makeCallHierarchyItemRequest(allocator, 3, "callHierarchy/incomingCalls", lib_uri, "greet"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 3);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    // `run` in the never-opened importer calls `m.greet` once.
    const calls = parsed.value.object.get("result").?.array.items;
    try std.testing.expectEqual(@as(usize, 1), calls.len);
    const from = calls[0].object.get("from").?.object;
    try std.testing.expectEqualStrings("run", from.get("name").?.string);
    try std.testing.expectEqualStrings(main_uri, from.get("uri").?.string);
    try std.testing.expectEqual(@as(usize, 1), calls[0].object.get("fromRanges").?.array.items.len);
}

test "lsp semantic tokens classify keywords, types, numbers and strings" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const x = 5
        \\fn Void greet(name: String) Void { echo "${name}" }
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeSemanticTokensRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const data = parsed.value.object.get("result").?.object.get("data").?.array.items;
    // Five ints per token; at least the tokens we assert below.
    try std.testing.expect(data.len % 5 == 0);
    try std.testing.expect(data.len >= 5 * 10);

    // Decode the delta-encoded stream into absolute (line, char, type) tuples.
    const Tok = struct { line: i64, char: i64, ttype: i64 };
    var toks = std.ArrayList(Tok).empty;
    defer toks.deinit(allocator);
    var line: i64 = 0;
    var char: i64 = 0;
    var i: usize = 0;
    while (i < data.len) : (i += 5) {
        const dline = data[i].integer;
        const dchar = data[i + 1].integer;
        const ttype = data[i + 3].integer;
        line += dline;
        if (dline != 0) char = 0;
        char += dchar;
        try toks.append(allocator, .{ .line = line, .char = char, .ttype = ttype });
    }

    // Token type codes from server.zig: keyword=0, string=1, number=2, type=4, variable=5.
    const keyword = 0;
    const string = 1;
    const number = 2;
    const typ = 4;
    const variable = 5;

    // Find a token at a given position and assert its type.
    const findType = struct {
        fn at(list: []const Tok, l: i64, c: i64) ?i64 {
            for (list) |t| if (t.line == l and t.char == c) return t.ttype;
            return null;
        }
    }.at;

    // `const` keyword at (0,0).
    try std.testing.expectEqual(@as(?i64, keyword), findType(toks.items, 0, 0));
    // `x` variable at (0,6).
    try std.testing.expectEqual(@as(?i64, variable), findType(toks.items, 0, 6));
    // `5` number at (0,10).
    try std.testing.expectEqual(@as(?i64, number), findType(toks.items, 0, 10));
    // `fn` keyword at (1,0).
    try std.testing.expectEqual(@as(?i64, keyword), findType(toks.items, 1, 0));
    // `Void` type at (1,3).
    try std.testing.expectEqual(@as(?i64, typ), findType(toks.items, 1, 3));
    // `String` type at (1,20).
    try std.testing.expectEqual(@as(?i64, typ), findType(toks.items, 1, 20));
    // The interpolated `name` inside `${name}` is code (variable), not string.
    try std.testing.expectEqual(@as(?i64, variable), findType(toks.items, 1, 43));
    // The opening quote of the string is a string token.
    try std.testing.expectEqual(@as(?i64, string), findType(toks.items, 1, 40));
}

test "lsp semantic tokens refine functions, parameters and declarations from the ast" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\fn Int greet(name: String) Int { yield name }
        \\const result = greet "x"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeSemanticTokensRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const data = parsed.value.object.get("result").?.object.get("data").?.array.items;

    // Decode to absolute (line, char) with type and modifier bitmask.
    const Tok = struct { line: i64, char: i64, ttype: i64, mods: i64 };
    var toks = std.ArrayList(Tok).empty;
    defer toks.deinit(allocator);
    var line: i64 = 0;
    var char: i64 = 0;
    var i: usize = 0;
    while (i < data.len) : (i += 5) {
        const dline = data[i].integer;
        line += dline;
        if (dline != 0) char = 0;
        char += data[i + 1].integer;
        try toks.append(allocator, .{ .line = line, .char = char, .ttype = data[i + 3].integer, .mods = data[i + 4].integer });
    }

    const find = struct {
        fn at(list: []const Tok, l: i64, c: i64) ?Tok {
            for (list) |t| if (t.line == l and t.char == c) return t;
            return null;
        }
    }.at;

    // Type codes: type=4, variable=5, function=6, parameter=7.
    // Modifier bits: declaration=1<<0, readonly=1<<1.
    const function = 6;
    const parameter = 7;
    const variable = 5;
    const decl_mod: i64 = 1;
    const readonly_mod: i64 = 2;

    // `greet` is a function declaration.
    const greet_decl = find(toks.items, 0, 7).?;
    try std.testing.expectEqual(@as(i64, function), greet_decl.ttype);
    try std.testing.expectEqual(decl_mod, greet_decl.mods & decl_mod);
    // `name` is a parameter declaration.
    const name_param = find(toks.items, 0, 13).?;
    try std.testing.expectEqual(@as(i64, parameter), name_param.ttype);
    try std.testing.expectEqual(decl_mod, name_param.mods & decl_mod);
    // `name` used in `yield name` is a plain reference (lexical variable), not a
    // parameter or a zero-arg call misread as a function.
    try std.testing.expectEqual(@as(i64, variable), find(toks.items, 0, 39).?.ttype);
    // `result` is a readonly (const) binding declaration.
    const result_bind = find(toks.items, 1, 6).?;
    try std.testing.expectEqual(@as(i64, variable), result_bind.ttype);
    try std.testing.expectEqual(decl_mod, result_bind.mods & decl_mod);
    try std.testing.expectEqual(readonly_mod, result_bind.mods & readonly_mod);
    // `greet "x"` is a call with an argument, so the callee is a function.
    try std.testing.expectEqual(@as(i64, function), find(toks.items, 1, 15).?.ttype);
}

test "lsp document symbols nest struct fields and function parameters" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const Point = struct { x: Int, y: Int }
        \\fn Void greet(name: String, times: Int) Void {
        \\    echo "hi"
        \\}
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeDocumentSymbolRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const syms = parsed.value.object.get("result").?.array.items;

    var checked_point = false;
    var checked_greet = false;
    for (syms) |s| {
        const name = s.object.get("name").?.string;
        const kind = s.object.get("kind").?.integer;
        const children = if (s.object.get("children")) |c| (if (c == .array) c.array.items else &[_]std.json.Value{}) else &[_]std.json.Value{};

        if (std.mem.eql(u8, name, "Point")) {
            checked_point = true;
            try std.testing.expectEqual(@as(i64, 23), kind); // SymbolKind.Struct
            try std.testing.expectEqual(@as(usize, 2), children.len);
            try std.testing.expectEqualStrings("x", children[0].object.get("name").?.string);
            try std.testing.expectEqualStrings("y", children[1].object.get("name").?.string);
            try std.testing.expectEqual(@as(i64, 8), children[0].object.get("kind").?.integer); // Field
        }
        if (std.mem.eql(u8, name, "greet")) {
            checked_greet = true;
            try std.testing.expectEqual(@as(i64, 12), kind); // Function
            try std.testing.expectEqual(@as(usize, 2), children.len);
            try std.testing.expectEqualStrings("name", children[0].object.get("name").?.string);
            try std.testing.expectEqualStrings("times", children[1].object.get("name").?.string);
        }
    }

    try std.testing.expect(checked_point);
    try std.testing.expect(checked_greet);
}

test "lsp document symbols nest a cimport's externs" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const c = import "std/ffi.rn"
        \\const m = cimport "libm.so.6" {
        \\    extern fn pow(base: c.Double, exp: c.Double) c.Double
        \\    extern fn cos(x: c.Double) c.Double
        \\}
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeDocumentSymbolRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const syms = parsed.value.object.get("result").?.array.items;
    var checked_m = false;
    for (syms) |s| {
        if (!std.mem.eql(u8, s.object.get("name").?.string, "m")) continue;
        checked_m = true;
        try std.testing.expectEqual(@as(i64, 2), s.object.get("kind").?.integer); // Module
        const children = s.object.get("children").?.array.items;
        try std.testing.expectEqual(@as(usize, 2), children.len);
        try std.testing.expectEqualStrings("pow", children[0].object.get("name").?.string);
        try std.testing.expectEqualStrings("cos", children[1].object.get("name").?.string);
        try std.testing.expectEqual(@as(i64, 12), children[0].object.get("kind").?.integer); // Function
    }
    try std.testing.expect(checked_m);
}

test "lsp rename returns concrete same-file edits" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\echo foo
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\echo foo
            \\
        ),
        try makeRenameRequest(allocator, 3, uri, 0, 6, "bar"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 3);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const changes = parsed.value.object.get("result").?.object.get("documentChanges").?.array.items;
    try std.testing.expectEqual(@as(usize, 1), changes.len);
    const edits = changes[0].object.get("edits").?.array.items;
    try std.testing.expectEqual(@as(usize, 2), edits.len);

    for (edits) |edit| {
        try std.testing.expectEqualStrings("bar", edit.object.get("newText").?.string);
        const range = edit.object.get("range").?.object;
        const start = range.get("start").?.object;
        const end = range.get("end").?.object;
        try std.testing.expect(end.get("character").?.integer > start.get("character").?.integer);
    }
}

test "lsp rename edits every indexed file that references the symbol" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const helper_uri = try fixture.writeDocument("helper.rn",
        \\const shared = 1
        \\
    );
    defer allocator.free(helper_uri);
    const main_uri = try fixture.writeDocument("main.rn",
        \\echo "${shared}"
        \\
    );
    defer allocator.free(main_uri);

    const root_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{fixture.root_path});
    defer allocator.free(root_uri);

    const messages = [_][]const u8{
        try makeInitializeWithRoot(allocator, 1, root_uri),
        try makeDidOpen(allocator, main_uri,
            \\echo "${shared}"
            \\
        ),
        // Rename `shared` from its use in main.rn.
        try makeRenameRequest(allocator, 2, main_uri, 0, 9, "renamed"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const changes = parsed.value.object.get("result").?.object.get("documentChanges").?.array.items;
    // Both the defining file and the referencing file are edited.
    try std.testing.expectEqual(@as(usize, 2), changes.len);
    var saw_helper = false;
    var saw_main = false;
    for (changes) |change| {
        const tde = change.object;
        const uri = tde.get("textDocument").?.object.get("uri").?.string;
        const edits = tde.get("edits").?.array.items;
        try std.testing.expect(edits.len >= 1);
        try std.testing.expectEqualStrings("renamed", edits[0].object.get("newText").?.string);
        if (std.mem.eql(u8, uri, helper_uri)) saw_helper = true;
        if (std.mem.eql(u8, uri, main_uri)) saw_main = true;
    }
    try std.testing.expect(saw_helper);
    try std.testing.expect(saw_main);
}

test "lsp prepare rename returns the identifier range, or null off an identifier" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const foo = 1
        \\echo foo
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makePrepareRenameRequest(allocator, 2, uri, 0, 6), // on `foo`
        try makePrepareRenameRequest(allocator, 3, uri, 0, 10), // on `=`
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // On the identifier: range covers `foo` and placeholder is its name.
    {
        const response = try findResponseById(allocator, output, 2);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        const result = parsed.value.object.get("result").?.object;
        try std.testing.expectEqualStrings("foo", result.get("placeholder").?.string);
        const range = result.get("range").?.object;
        const start = range.get("start").?.object;
        try std.testing.expectEqual(@as(i64, 0), start.get("line").?.integer);
        try std.testing.expectEqual(@as(i64, 6), start.get("character").?.integer);
        // The range must END where `foo` ends (col 9), not one short or one
        // long — a span-width off-by-one in the 1↔0-indexed conversion would
        // rename only `fo`/`foo `.
        const end = range.get("end").?.object;
        try std.testing.expectEqual(@as(i64, 0), end.get("line").?.integer);
        try std.testing.expectEqual(@as(i64, 9), end.get("character").?.integer);
    }

    // Off any identifier (on `=`): null, so the client blocks the rename.
    {
        const response = try findResponseById(allocator, output, 3);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        try std.testing.expect(parsed.value.object.get("result").? == .null);
    }
}

test "lsp rename of a module member edits the module declaration and the access" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const module_uri = try fixture.writeDocument("module.rn",
        \\pub const shared = 1
        \\
    );
    defer allocator.free(module_uri);
    const main_uri = try fixture.writeDocument("main.rn",
        \\const m = import "./module.rn"
        \\echo "${m.shared}"
        \\
    );
    defer allocator.free(main_uri);
    // An unrelated, identically-named binding in another file that must NOT be
    // renamed — the lexical behaviour would have wrongly rewritten it.
    const other_uri = try fixture.writeDocument("other.rn",
        \\const shared = 99
        \\echo "${shared}"
        \\
    );
    defer allocator.free(other_uri);
    // A second importer of the module that is only indexed, never opened. Its
    // `k.shared` access must still be renamed (resolved via on-demand checking).
    const importer2_uri = try fixture.writeDocument("importer2.rn",
        \\const k = import "./module.rn"
        \\echo "${k.shared}"
        \\
    );
    defer allocator.free(importer2_uri);

    const root_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{fixture.root_path});
    defer allocator.free(root_uri);

    const messages = [_][]const u8{
        try makeInitializeWithRoot(allocator, 1, root_uri),
        try makeDidOpen(allocator, main_uri,
            \\const m = import "./module.rn"
            \\echo "${m.shared}"
            \\
        ),
        // Rename `shared` from the `m.shared` member access (line 1).
        try makeRenameRequest(allocator, 2, main_uri, 1, 12, "renamed"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const changes = parsed.value.object.get("result").?.object.get("documentChanges").?.array.items;
    var saw_module = false;
    var saw_main = false;
    var saw_other = false;
    var saw_importer2 = false;
    for (changes) |change| {
        const tde = change.object;
        const uri = tde.get("textDocument").?.object.get("uri").?.string;
        const edits = tde.get("edits").?.array.items;
        try std.testing.expect(edits.len >= 1);
        try std.testing.expectEqualStrings("renamed", edits[0].object.get("newText").?.string);
        if (std.mem.eql(u8, uri, module_uri)) saw_module = true;
        if (std.mem.eql(u8, uri, main_uri)) saw_main = true;
        if (std.mem.eql(u8, uri, other_uri)) saw_other = true;
        if (std.mem.eql(u8, uri, importer2_uri)) saw_importer2 = true;
    }
    // The `pub const shared` declaration and the `m.shared` access are both edited,
    // as is the `k.shared` access in the never-opened second importer...
    try std.testing.expect(saw_module);
    try std.testing.expect(saw_main);
    try std.testing.expect(saw_importer2);
    // ...but the unrelated `shared` in other.rn is not.
    try std.testing.expect(!saw_other);
}

test "lsp rename is scoped to the binding, not every same-named identifier" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // Two functions each have a local `x`. Renaming one must not touch the other.
    const source =
        \\fn Void a() Int {
        \\    const x = 1
        \\    yield x
        \\}
        \\fn Void b() Int {
        \\    const x = 2
        \\    yield x
        \\}
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Cursor on `x` used in function a (line 2).
        try makeRenameRequest(allocator, 2, uri, 2, 10, "renamed"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const changes = parsed.value.object.get("result").?.object.get("documentChanges").?.array.items;
    try std.testing.expectEqual(@as(usize, 1), changes.len);
    const edits = changes[0].object.get("edits").?.array.items;

    // Only the two occurrences inside function a (lines 1 and 2) are renamed;
    // function b's `x` on lines 5 and 6 is a different binding and left alone.
    try std.testing.expectEqual(@as(usize, 2), edits.len);
    for (edits) |edit| {
        const line = edit.object.get("range").?.object.get("start").?.object.get("line").?.integer;
        try std.testing.expect(line == 1 or line == 2);
    }
}

test "lsp rename ignores strings and comments" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\echo "foo should stay in strings"
        \\# foo should stay in comments
        \\echo foo
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\echo "foo should stay in strings"
            \\# foo should stay in comments
            \\echo foo
            \\
        ),
        try makeRenameRequest(allocator, 6, uri, 0, 6, "bar"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 6);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const changes = parsed.value.object.get("result").?.object.get("documentChanges").?.array.items;
    const edits = changes[0].object.get("edits").?.array.items;
    try std.testing.expectEqual(@as(usize, 2), edits.len);
    for (edits) |edit| {
        try std.testing.expectEqualStrings("bar", edit.object.get("newText").?.string);
    }
}

test "lsp definition resolves imported module member to module file" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const module_uri = try fixture.writeDocument("module.rn",
        \\pub const version = "0.0.1"
        \\
    );
    defer allocator.free(module_uri);

    const main_uri = try fixture.writeDocument("main.rn",
        \\const m = import "./module.rn"
        \\echo "${m.version}"
        \\
    );
    defer allocator.free(main_uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, module_uri,
            \\pub const version = "0.0.1"
            \\
        ),
        try makeDidOpen(allocator, main_uri,
            \\const m = import "./module.rn"
            \\echo "${m.version}"
            \\
        ),
        try makeDefinitionRequest(allocator, 4, main_uri, 1, 10),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 4);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const result = parsed.value.object.get("result").?.object;
    try std.testing.expectEqualStrings(module_uri, result.get("uri").?.string);
    const range = result.get("range").?.object;
    const start = range.get("start").?.object;
    try std.testing.expectEqual(@as(i64, 0), start.get("line").?.integer);
    try std.testing.expectEqual(@as(i64, 10), start.get("character").?.integer);
}

const DefResult = struct { found: bool, line: i64, character: i64 };

fn singleFileDefinition(alloc: Allocator, source: []const u8, line: u32, char: u32) !DefResult {
    var fixture = try TestFixture.init(alloc);
    defer fixture.deinit();
    const uri = try fixture.writeDocument("main.rn", source);
    defer alloc.free(uri);
    const messages = [_][]const u8{
        try makeDidOpen(alloc, uri, source),
        try makeDefinitionRequest(alloc, 1, uri, line, char),
    };
    defer for (messages) |m| alloc.free(m);
    const output = try runServerWithMessages(alloc, &messages);
    defer alloc.free(output);
    const response = try findResponseById(alloc, output, 1);
    defer alloc.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, alloc, response.body, .{});
    defer parsed.deinit();
    const result = parsed.value.object.get("result").?;
    if (result == .null) return .{ .found = false, .line = -1, .character = -1 };
    const start = result.object.get("range").?.object.get("start").?.object;
    return .{ .found = true, .line = start.get("line").?.integer, .character = start.get("character").?.integer };
}

fn expectDefinition(source: []const u8, line: u32, char: u32, exp_line: i64, exp_char: i64) !void {
    const r = try singleFileDefinition(std.testing.allocator, source, line, char);
    try std.testing.expect(r.found);
    try std.testing.expectEqual(exp_line, r.line);
    try std.testing.expectEqual(exp_char, r.character);
}

test "lsp definition resolves a local binding usage" {
    try expectDefinition(
        \\const foo = 1
        \\echo foo
        \\
    , 1, 5, 0, 6);
}

test "lsp definition resolves a function parameter usage" {
    try expectDefinition(
        \\fn Void add(x: Int, y: Int) Int {
        \\    yield x + y
        \\}
        \\
    , 1, 10, 0, 12);
}

test "lsp definition resolves the nearest shadowing binding" {
    // The inner `const x` (line 2) shadows the outer one (line 0); the usage in
    // the function body must resolve to the inner declaration.
    try expectDefinition(
        \\const x = 1
        \\fn Void f() Int {
        \\    const x = 2
        \\    yield x
        \\}
        \\
    , 3, 10, 2, 10);
}

test "lsp definition resolves a function call to its declaration" {
    try expectDefinition(
        \\fn Void greet(name: String) Void {
        \\    echo "hi ${name}"
        \\}
        \\greet "x"
        \\
    , 3, 2, 0, 8);
}

test "lsp definition resolves a struct field member access to the field declaration" {
    // Cursor on `x` in `p.x` jumps to the `x` field in the struct definition,
    // not to any unrelated `x` binding that might be in scope.
    try expectDefinition(
        \\const Point = struct { x: Int, y: Int }
        \\const p = Point{ .x = 1, .y = 2 }
        \\echo "${p.x}"
        \\
    , 2, 10, 0, 23);
}

test "lsp definition resolves a nested struct field member access two levels deep" {
    // Cursor on `x` in `l.from.x` must descend Line -> Point and resolve to
    // Point's `x` field, not stop at the first level.
    try expectDefinition(
        \\const Point = struct { x: Int, y: Int }
        \\const Line = struct { from: Point, to: Point }
        \\fn Void main() Void {
        \\    const l = Line{ .from = Point{ .x = 1, .y = 2 }, .to = Point{ .x = 3, .y = 4 } }
        \\    echo "${l.from.x}"
        \\}
        \\
    , 4, 19, 0, 23);
}

test "lsp definition resolves a struct literal field to the field declaration" {
    // Cursor on `.x` inside the `Point{ … }` literal jumps to the `x` field
    // declaration — a literal field is not an `object.member` access.
    try expectDefinition(
        \\const Point = struct { x: Int, y: Int }
        \\const p = Point{ .x = 1, .y = 2 }
        \\echo "${p.y}"
        \\
    , 1, 18, 0, 23);
}

test "lsp definition resolves a nested struct literal field" {
    // The `.x` belongs to the inner `Point{ … }` nested in the `Line{ … }`
    // literal; it must resolve to Point's field, not Line's.
    try expectDefinition(
        \\const Point = struct { x: Int, y: Int }
        \\const Line = struct { from: Point, to: Point }
        \\const l = Line{ .from = Point{ .x = 1, .y = 2 }, .to = Point{ .x = 3, .y = 4 } }
        \\echo "${l.from.x}"
        \\
    , 2, 32, 0, 23);
}

test "lsp definition on an embedded std-module member does not crash" {
    // The member resolves into an embedded std module whose declaration span
    // has a virtual file path with no on-disk location. Resolving that path to a
    // URI fails; the server must reply with an empty result rather than letting
    // the error crash it.
    const allocator = std.testing.allocator;
    const source =
        \\const s = import "std/str.rn"
        \\echo "${s.capitalize "x"}"
        \\
    ;
    // Cursor on `capitalize` in `s.capitalize` (line 1). The request must return
    // (found or not) without crashing the server.
    _ = try singleFileDefinition(allocator, source, 1, 12);
}

test "lsp definition resolves a cimport member to its extern declaration" {
    // Cursor on `pow` in `m.pow` jumps to the `extern fn pow` declaration.
    try expectDefinition(
        \\const c = import "std/ffi.rn"
        \\const m = cimport "libm.so.6" {
        \\    extern fn pow(base: c.Double, exp: c.Double) c.Double
        \\}
        \\echo "${m.pow 2.0 10.0}"
        \\
    , 4, 11, 2, 14);
}

test "lsp definition prefers the struct field over an unrelated same-named binding" {
    // A top-level `const x` exists, but the cursor is on the member `p.x`, so
    // member resolution must win over the plain identifier lookup.
    try expectDefinition(
        \\const x = 99
        \\const Point = struct { x: Int, y: Int }
        \\const p = Point{ .x = 1, .y = 2 }
        \\echo "${p.x}"
        \\
    , 3, 10, 1, 23);
}

test "lsp per-edit recheck stays bounded to open documents, not imported modules" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const module_uri = try fixture.writeDocument("module.rn",
        \\pub const version = "1"
        \\
    );
    allocator.free(module_uri);
    const main_uri = try fixture.writeDocument("main.rn",
        \\const m = import "./module.rn"
        \\echo "${m.version}"
        \\
    );
    defer allocator.free(main_uri);

    // Open ONLY main.rn; module.rn is pulled into the store transitively by the
    // import, so it ends up in the document map without ever being opened.
    const messages = [_][]const u8{
        try makeDidOpen(allocator, main_uri,
            \\const m = import "./module.rn"
            \\echo "${m.version}"
            \\
        ),
        try makeDidChangeWholeDocument(allocator, main_uri, 2,
            \\const m = import "./module.rn"
            \\echo "${m.version} a"
            \\
        ),
        try makeDidChangeWholeDocument(allocator, main_uri, 3,
            \\const m = import "./module.rn"
            \\echo "${m.version} ab"
            \\
        ),
    };
    defer for (messages) |m| allocator.free(m);

    const result = try runServerWithMessagesDetailed(allocator, &messages);
    defer result.deinit(allocator);

    // The imported module is present in the store...
    try std.testing.expect(result.doc_count >= 2);
    // ...but the per-edit recheck only covers the open document, so per-keystroke
    // work does not grow with the number of modules pulled in over a session.
    try std.testing.expectEqual(@as(usize, 1), result.recheck_count);
    // And analysis stays quiet on stderr through the edits.
    try std.testing.expectEqualStrings("", result.stderr);
}

test "lsp survives request on an importer after its imported module is closed" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const module_uri = try fixture.writeDocument("module.rn",
        \\pub const version = "0.0.1"
        \\
    );
    defer allocator.free(module_uri);
    const main_uri = try fixture.writeDocument("main.rn",
        \\const m = import "./module.rn"
        \\echo "${m.version}"
        \\
    );
    defer allocator.free(main_uri);

    // Open both, close the imported module, then — without any intervening edit
    // that would re-heal caches — issue a definition request on the importer
    // that reaches through the (now closed) module's type.
    const messages = [_][]const u8{
        try makeDidOpen(allocator, module_uri,
            \\pub const version = "0.0.1"
            \\
        ),
        try makeDidOpen(allocator, main_uri,
            \\const m = import "./module.rn"
            \\echo "${m.version}"
            \\
        ),
        try makeDidClose(allocator, module_uri),
        try makeDefinitionRequest(allocator, 5, main_uri, 1, 10),
    };
    defer for (messages) |m| allocator.free(m);

    const result = try runServerWithMessagesDetailed(allocator, &messages);
    defer result.deinit(allocator);

    // No crash, no leak (checked by the testing allocator), quiet stderr.
    try std.testing.expectEqualStrings("", result.stderr);

    // The importer's caches were rebuilt on close (the module is re-read from
    // disk), so member resolution still points at the module's real declaration
    // rather than reading through the freed AST.
    const response = try findResponseById(allocator, result.stdout, 5);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    const def = parsed.value.object.get("result").?.object;
    try std.testing.expectEqualStrings(module_uri, def.get("uri").?.string);
    const start = def.get("range").?.object.get("start").?.object;
    try std.testing.expectEqual(@as(i64, 0), start.get("line").?.integer);
    try std.testing.expectEqual(@as(i64, 10), start.get("character").?.integer);
}

test "lsp survives repeated open/change/close/reopen churn" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\echo foo
        \\
    );
    defer allocator.free(uri);

    var message_list = std.ArrayList([]const u8).empty;
    defer {
        for (message_list.items) |m| allocator.free(m);
        message_list.deinit(allocator);
    }

    var round: i64 = 0;
    while (round < 5) : (round += 1) {
        try message_list.append(allocator, try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\echo foo
            \\
        ));
        try message_list.append(allocator, try makeDidChangeWholeDocument(allocator, uri, round * 2 + 2,
            \\const foo = 2
            \\echo foo
            \\
        ));
        try message_list.append(allocator, try makeDidChangeIncremental(allocator, uri, round * 2 + 3, 0, 6, 0, 9, "renamed"));
        try message_list.append(allocator, try makeCompletionRequest(allocator, 100 + round, uri, 1, 5));
        try message_list.append(allocator, try makeDidClose(allocator, uri));
    }

    const result = try runServerWithMessagesDetailed(allocator, message_list.items);
    defer result.deinit(allocator);

    // The document was closed on the last round, so the store ends empty.
    try std.testing.expectEqual(@as(usize, 0), result.doc_count);
    try std.testing.expectEqualStrings("", result.stderr);
}

test "lsp tolerates didChange and didClose for never-opened documents" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("ghost.rn",
        \\const foo = 1
        \\
    );
    defer allocator.free(uri);

    // Change and close a document the server never saw an open for.
    const messages = [_][]const u8{
        try makeDidChangeWholeDocument(allocator, uri, 2,
            \\const foo = 2
            \\
        ),
        try makeDidClose(allocator, uri),
    };
    defer for (messages) |m| allocator.free(m);

    const result = try runServerWithMessagesDetailed(allocator, &messages);
    defer result.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), result.doc_count);
    try std.testing.expectEqualStrings("", result.stderr);
}

test "lsp references search across opened documents" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const module_uri = try fixture.writeDocument("module.rn",
        \\pub const version = "0.0.1"
        \\
    );
    defer allocator.free(module_uri);

    const main_uri = try fixture.writeDocument("main.rn",
        \\const m = import "./module.rn"
        \\echo "${m.version}"
        \\
    );
    defer allocator.free(main_uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, module_uri,
            \\pub const version = "0.0.1"
            \\
        ),
        try makeDidOpen(allocator, main_uri,
            \\const m = import "./module.rn"
            \\echo "${m.version}"
            \\
        ),
        try makeReferencesRequest(allocator, 5, main_uri, 1, 10, true),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 5);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const results = parsed.value.object.get("result").?.array.items;
    try std.testing.expect(results.len >= 2);

    var saw_module = false;
    var saw_main = false;
    for (results) |item| {
        const obj = item.object;
        const uri = obj.get("uri").?.string;
        if (std.mem.eql(u8, uri, module_uri)) saw_module = true;
        if (std.mem.eql(u8, uri, main_uri)) saw_main = true;
    }

    try std.testing.expect(saw_module);
    try std.testing.expect(saw_main);
}

test "lsp references are scoped to the binding under the cursor" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\fn Void a() Int {
        \\    const x = 1
        \\    yield x
        \\}
        \\fn Void b() Int {
        \\    const x = 2
        \\    yield x
        \\}
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // References of `x` from within function a (line 2).
        try makeReferencesRequest(allocator, 2, uri, 2, 10, true),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const results = parsed.value.object.get("result").?.array.items;
    // Only function a's two occurrences of `x` (lines 1 and 2), not b's.
    try std.testing.expectEqual(@as(usize, 2), results.len);
    for (results) |ref| {
        const line = ref.object.get("range").?.object.get("start").?.object.get("line").?.integer;
        try std.testing.expect(line == 1 or line == 2);
    }
}

test "lsp references can exclude declaration" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\echo foo
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\echo foo
            \\
        ),
        try makeReferencesRequest(allocator, 7, uri, 1, 5, false),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 7);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const results = parsed.value.object.get("result").?.array.items;
    try std.testing.expectEqual(@as(usize, 1), results.len);
    const result = results[0].object;
    try std.testing.expectEqualStrings(uri, result.get("uri").?.string);
    const start = result.get("range").?.object.get("start").?.object;
    try std.testing.expectEqual(@as(i64, 1), start.get("line").?.integer);
    try std.testing.expectEqual(@as(i64, 5), start.get("character").?.integer);
}

test "lsp workspace symbol search finds symbols in unopened files" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const alpha_uri = try fixture.writeDocument("alpha.rn",
        \\const alphaSymbol = 1
        \\
    );
    defer allocator.free(alpha_uri);
    const beta_uri = try fixture.writeDocument("beta.rn",
        \\fn Void betaFn(name: String) Void {
        \\    echo "hi"
        \\}
        \\
    );
    defer allocator.free(beta_uri);

    const root_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{fixture.root_path});
    defer allocator.free(root_uri);

    // Initialize with the fixture as the workspace root (indexes both files
    // without either being opened), then search.
    const messages = [_][]const u8{
        try makeInitializeWithRoot(allocator, 1, root_uri),
        try makeWorkspaceSymbolRequest(allocator, 2, "alpha"),
        try makeWorkspaceSymbolRequest(allocator, 3, "betaFn"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // "alpha" → the const in alpha.rn.
    {
        const response = try findResponseById(allocator, output, 2);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        const results = parsed.value.object.get("result").?.array.items;
        try std.testing.expectEqual(@as(usize, 1), results.len);
        try std.testing.expectEqualStrings("alphaSymbol", results[0].object.get("name").?.string);
        try std.testing.expectEqualStrings(alpha_uri, results[0].object.get("location").?.object.get("uri").?.string);
    }

    // "betaFn" → the function in beta.rn.
    {
        const response = try findResponseById(allocator, output, 3);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        const results = parsed.value.object.get("result").?.array.items;
        try std.testing.expectEqual(@as(usize, 1), results.len);
        try std.testing.expectEqualStrings("betaFn", results[0].object.get("name").?.string);
        try std.testing.expectEqualStrings(beta_uri, results[0].object.get("location").?.object.get("uri").?.string);
        try std.testing.expectEqual(@as(i64, 12), results[0].object.get("kind").?.integer); // Function
    }
}

test "lsp go-to-definition resolves across an unopened workspace file" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const helper_uri = try fixture.writeDocument("helper.rn",
        \\const sharedConst = 42
        \\
    );
    defer allocator.free(helper_uri);
    const main_uri = try fixture.writeDocument("main.rn",
        \\echo "${sharedConst}"
        \\
    );
    defer allocator.free(main_uri);

    const root_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{fixture.root_path});
    defer allocator.free(root_uri);

    // Index the workspace (loads helper.rn without opening it), open main.rn,
    // then ask for the definition of `sharedConst` used in main.rn.
    const messages = [_][]const u8{
        try makeInitializeWithRoot(allocator, 1, root_uri),
        try makeDidOpen(allocator, main_uri,
            \\echo "${sharedConst}"
            \\
        ),
        try makeDefinitionRequest(allocator, 2, main_uri, 0, 9),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const result = parsed.value.object.get("result").?.object;
    // Resolves to the declaration in the unopened helper file.
    try std.testing.expectEqualStrings(helper_uri, result.get("uri").?.string);
    const start = result.get("range").?.object.get("start").?.object;
    try std.testing.expectEqual(@as(i64, 0), start.get("line").?.integer);
    try std.testing.expectEqual(@as(i64, 6), start.get("character").?.integer);
}

test "lsp inlay hints label call arguments with parameter names" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\fn Void greet(name: String, times: Int) Void {
        \\    echo "hi"
        \\}
        \\greet "x" 5
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeInlayHintRequest(allocator, 2, uri, 10),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const hints = parsed.value.object.get("result").?.array.items;
    var name_line: ?i64 = null;
    var times_line: ?i64 = null;
    for (hints) |h| {
        const label = h.object.get("label").?.string;
        const line = h.object.get("position").?.object.get("line").?.integer;
        const kind = h.object.get("kind").?.integer;
        if (std.mem.eql(u8, label, "name:")) {
            name_line = line;
            try std.testing.expectEqual(@as(i64, 2), kind); // Parameter
        }
        if (std.mem.eql(u8, label, "times:")) times_line = line;
    }
    // Both parameter hints appear on the call line (line 3).
    try std.testing.expectEqual(@as(?i64, 3), name_line);
    try std.testing.expectEqual(@as(?i64, 3), times_line);
}

test "lsp inlay parameter hints resolve an imported module's function" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const module_uri = try fixture.writeDocument("mod.rn",
        \\pub fn Void tag(label: String) String {
        \\    yield "${label}"
        \\}
        \\
    );
    defer allocator.free(module_uri);
    const main_uri = try fixture.writeDocument("main.rn",
        \\const m = import "./mod.rn"
        \\m.tag "hi"
        \\
    );
    defer allocator.free(main_uri);

    const root_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{fixture.root_path});
    defer allocator.free(root_uri);

    const messages = [_][]const u8{
        try makeInitializeWithRoot(allocator, 1, root_uri),
        try makeDidOpen(allocator, main_uri,
            \\const m = import "./mod.rn"
            \\m.tag "hi"
            \\
        ),
        try makeInlayHintRequest(allocator, 2, main_uri, 10),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const hints = parsed.value.object.get("result").?.array.items;
    // `m.tag "hi"` — the argument is labelled with the module function's param.
    var saw_label = false;
    for (hints) |h| {
        if (std.mem.eql(u8, h.object.get("label").?.string, "label:")) {
            saw_label = true;
            try std.testing.expectEqual(@as(i64, 1), h.object.get("position").?.object.get("line").?.integer);
        }
    }
    try std.testing.expect(saw_label);
}

test "lsp inlay parameter hints reach calls inside a pipeline" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\fn String shout(prefix: String) String {
        \\    echo "${prefix}"
        \\}
        \\echo "hi" | shout "x"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeInlayHintRequest(allocator, 2, uri, 10),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const hints = parsed.value.object.get("result").?.array.items;
    // The `shout "x"` call is the second stage of a pipeline (line 3); the old
    // top-level-only walk would have missed it.
    var saw_prefix = false;
    for (hints) |h| {
        if (std.mem.eql(u8, h.object.get("label").?.string, "prefix:")) {
            saw_prefix = true;
            try std.testing.expectEqual(@as(i64, 3), h.object.get("position").?.object.get("line").?.integer);
        }
    }
    try std.testing.expect(saw_prefix);
}

test "lsp completionItem/resolve promotes detail to documentation" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn", "echo\n");
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, "echo\n"),
        try makeCompletionResolveRequest(allocator, 2, "greet", "(name: String) Void"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const result = parsed.value.object.get("result").?.object;
    // The item is echoed with its fields intact...
    try std.testing.expectEqualStrings("greet", result.get("label").?.string);
    try std.testing.expectEqualStrings("(name: String) Void", result.get("detail").?.string);
    // ...and its detail is promoted to documentation.
    try std.testing.expectEqualStrings("(name: String) Void", result.get("documentation").?.string);
}

test "lsp code action adds an inferred type annotation" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // `x` is referenced so the (separate) remove-unused action does not also fire.
    const source =
        \\const x = 5
        \\echo "${x}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeCodeActionRequest(allocator, 2, uri, 0),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const actions = parsed.value.object.get("result").?.array.items;
    try std.testing.expectEqual(@as(usize, 1), actions.len);
    const action = actions[0].object;
    try std.testing.expectEqualStrings("Add type annotation: Int", action.get("title").?.string);
    // CodeActionKind is a protocol string ("refactor.rewrite"), not a numeric
    // enum code — assert it stays a string so the classification reaches clients.
    if (action.get("kind")) |kind| {
        try std.testing.expect(kind == .string);
    }

    const edits = action.get("edit").?.object.get("documentChanges").?.array
        .items[0].object.get("edits").?.array.items;
    try std.testing.expectEqual(@as(usize, 1), edits.len);
    try std.testing.expectEqualStrings(": Int", edits[0].object.get("newText").?.string);
    // Inserted right after the identifier `x` (column 7 on line 0).
    const start = edits[0].object.get("range").?.object.get("start").?.object;
    try std.testing.expectEqual(@as(i64, 0), start.get("line").?.integer);
    try std.testing.expectEqual(@as(i64, 7), start.get("character").?.integer);
}

test "lsp code action removes an unused binding but not a used one" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const keep = 1
        \\const drop = 2
        \\echo "${keep}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Line 1 is the unused `drop`.
        try makeCodeActionRequest(allocator, 2, uri, 1),
        // Line 0 is the used `keep`.
        try makeCodeActionRequest(allocator, 3, uri, 0),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // `drop` is unreferenced, so a remove action is offered and deletes its line.
    {
        const response = try findResponseById(allocator, output, 2);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        var remove: ?std.json.Value = null;
        for (parsed.value.object.get("result").?.array.items) |a| {
            const title = a.object.get("title").?.string;
            if (std.mem.eql(u8, title, "Remove unused 'drop'")) remove = a;
        }
        try std.testing.expect(remove != null);
        const edit = remove.?.object.get("edit").?.object.get("documentChanges").?.array
            .items[0].object.get("edits").?.array.items[0].object;
        try std.testing.expectEqualStrings("", edit.get("newText").?.string);
        const range = edit.get("range").?.object;
        try std.testing.expectEqual(@as(i64, 1), range.get("start").?.object.get("line").?.integer);
        try std.testing.expectEqual(@as(i64, 2), range.get("end").?.object.get("line").?.integer);
    }
    // `keep` is referenced, so no remove action is offered for it.
    {
        const response = try findResponseById(allocator, output, 3);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        for (parsed.value.object.get("result").?.array.items) |a| {
            const title = a.object.get("title").?.string;
            try std.testing.expect(std.mem.indexOf(u8, title, "Remove unused") == null);
        }
    }
}

test "lsp code action offers remove-all for multiple unused bindings" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const keep = 1
        \\const d1 = 2
        \\const d2 = 3
        \\echo "${keep}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeCodeActionRequest(allocator, 2, uri, 1),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    var all: ?std.json.Value = null;
    for (parsed.value.object.get("result").?.array.items) |a| {
        const title = a.object.get("title").?.string;
        if (std.mem.startsWith(u8, title, "Remove all unused bindings")) all = a;
    }
    try std.testing.expect(all != null);
    // Two unused bindings (d1, d2) -> two delete edits; `keep` is untouched.
    const edits = all.?.object.get("edit").?.object.get("documentChanges").?.array
        .items[0].object.get("edits").?.array.items;
    try std.testing.expectEqual(@as(usize, 2), edits.len);
}

test "lsp folding ranges cover multi-line statements" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\fn Void greet(name: String) Void {
        \\    echo "hi"
        \\}
        \\const x = 1
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeFoldingRangeRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const ranges = parsed.value.object.get("result").?.array.items;
    // The three-line function folds; the single-line `const x` does not.
    try std.testing.expectEqual(@as(usize, 1), ranges.len);
    try std.testing.expectEqual(@as(i64, 0), ranges[0].object.get("startLine").?.integer);
    try std.testing.expectEqual(@as(i64, 2), ranges[0].object.get("endLine").?.integer);
}

test "lsp inlay hints show inferred types for un-annotated bindings" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const x = 5
        \\const y: Int = 3
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeInlayHintRequest(allocator, 2, uri, 10),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const hints = parsed.value.object.get("result").?.array.items;
    // Only `x` (no annotation) gets a hint; `y` is already annotated.
    try std.testing.expectEqual(@as(usize, 1), hints.len);
    const hint = hints[0].object;
    try std.testing.expectEqualStrings(": Int", hint.get("label").?.string);
    try std.testing.expectEqual(@as(i64, 1), hint.get("kind").?.integer); // Type
    const pos = hint.get("position").?.object;
    try std.testing.expectEqual(@as(i64, 0), pos.get("line").?.integer);
    try std.testing.expectEqual(@as(i64, 7), pos.get("character").?.integer);
}

test "lsp document link points an import path at the module file" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const module_uri = try fixture.writeDocument("module.rn",
        \\pub const version = "1"
        \\
    );
    defer allocator.free(module_uri);
    const main_uri = try fixture.writeDocument("main.rn",
        \\const m = import "./module.rn"
        \\echo "${m.version}"
        \\
    );
    defer allocator.free(main_uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, main_uri,
            \\const m = import "./module.rn"
            \\echo "${m.version}"
            \\
        ),
        try makeDocumentLinkRequest(allocator, 7, main_uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 7);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const links = parsed.value.object.get("result").?.array.items;
    try std.testing.expectEqual(@as(usize, 1), links.len);
    const link = links[0].object;

    // Target is the module's file:// URI.
    try std.testing.expectEqualStrings(module_uri, link.get("target").?.string);
    // Range covers the import path string on line 0 (after `const m = import "`).
    const start = link.get("range").?.object.get("start").?.object;
    try std.testing.expectEqual(@as(i64, 0), start.get("line").?.integer);
    try std.testing.expect(start.get("character").?.integer > 0);
}

test "lsp document highlight marks identifier occurrences, ignoring strings and comments" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const foo = 1
        \\echo "foo lives in a string"
        \\# foo lives in a comment
        \\const bar = foo
        \\echo foo
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Cursor on `foo` in its declaration (line 0, char 6).
        try makeDocumentHighlightRequest(allocator, 7, uri, 0, 6),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 7);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const highlights = parsed.value.object.get("result").?.array.items;
    // Three code occurrences: the declaration (line 0), the use on line 3, and
    // the use on line 4 — the string and comment mentions are excluded.
    try std.testing.expectEqual(@as(usize, 3), highlights.len);
    var saw_lines = [_]bool{ false, false, false, false, false };
    for (highlights) |h| {
        const start = h.object.get("range").?.object.get("start").?.object;
        const line: usize = @intCast(start.get("line").?.integer);
        try std.testing.expect(line < saw_lines.len);
        saw_lines[line] = true;
        // Highlight kind is DocumentHighlightKind.Text (1), serialized numeric.
        try std.testing.expectEqual(@as(i64, 1), h.object.get("kind").?.integer);
    }
    try std.testing.expect(saw_lines[0]);
    try std.testing.expect(saw_lines[3]);
    try std.testing.expect(saw_lines[4]);
    try std.testing.expect(!saw_lines[1]); // string
    try std.testing.expect(!saw_lines[2]); // comment
}

test "lsp publishes diagnostics for invalid source" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo =
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo =
            \\
        ),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const notification = try findMethodNotification(allocator, output, "textDocument/publishDiagnostics");
    defer allocator.free(notification);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, notification, .{});
    defer parsed.deinit();

    const params = parsed.value.object.get("params").?.object;
    try std.testing.expectEqualStrings(uri, params.get("uri").?.string);
    const diagnostics = params.get("diagnostics").?.array.items;
    try std.testing.expect(diagnostics.len > 0);
    // Severity must serialize as the numeric DiagnosticSeverity code, not the
    // enum tag name — clients rely on the number to colour the diagnostic.
    if (diagnostics[0].object.get("severity")) |sev| {
        try std.testing.expect(sev == .integer);
        try std.testing.expect(sev.integer >= 1 and sev.integer <= 4);
    }
}

test "lsp didChange and completion stay quiet on stderr" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\echo foo
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\echo foo
            \\
        ),
        try makeDidChangeWholeDocument(allocator, uri, 2,
            \\const food = 1
            \\echo food
            \\
        ),
        try makeCompletionRequest(allocator, 8, uri, 1, 9),
    };
    defer for (messages) |message| allocator.free(message);

    const result = try runServerWithMessagesDetailed(allocator, &messages);
    defer result.deinit(allocator);

    try std.testing.expectEqualStrings("", result.stderr);

    const response = try findResponseById(allocator, result.stdout, 8);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    try std.testing.expect(items.len > 0);
}

test "lsp incremental didChange applies a range edit to the right region" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\
        ),
        // Replace just the identifier "foo" (line 0, chars 6..9) — not the
        // whole document — proving incremental range edits patch the exact span.
        try makeDidChangeIncremental(allocator, uri, 2, 0, 6, 0, 9, "renamed"),
        try makeDocumentSymbolRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const symbols_value = parsed.value.object.get("result").?;
    try std.testing.expect(symbols_value.array.items.len >= 1);
    const first = symbols_value.array.items[0].object;

    // The symbol was renamed by the edit, and its declaration still starts at
    // the same column — the surrounding text was left untouched.
    try std.testing.expectEqualStrings("renamed", first.get("name").?.string);
    const start = first.get("range").?.object.get("start").?.object;
    try std.testing.expectEqual(@as(i64, 0), start.get("line").?.integer);
    try std.testing.expectEqual(@as(i64, 6), start.get("character").?.integer);
}

test "lsp member completion prefers module members over keywords" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const module_uri = try fixture.writeDocument("module.rn",
        \\pub const version = "0.0.1"
        \\pub fn add(x: Int, y: Int) Int {
        \\    return x + y
        \\}
        \\
    );
    defer allocator.free(module_uri);

    const main_uri = try fixture.writeDocument("main.rn",
        \\const m = import "./module.rn"
        \\echo m.v
        \\
    );
    defer allocator.free(main_uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, module_uri,
            \\pub const version = "0.0.1"
            \\pub fn add(x: Int, y: Int) Int {
            \\    return x + y
            \\}
            \\
        ),
        try makeDidOpen(allocator, main_uri,
            \\const m = import "./module.rn"
            \\echo m.v
            \\
        ),
        try makeCompletionRequest(allocator, 9, main_uri, 1, 7),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 9);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    try std.testing.expect(items.len > 0);

    var saw_version = false;
    var saw_add = false;
    var saw_stdout = false;
    var saw_keyword_const = false;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        if (std.mem.eql(u8, label, "version")) saw_version = true;
        if (std.mem.eql(u8, label, "add")) saw_add = true;
        if (std.mem.eql(u8, label, "stdout")) saw_stdout = true;
        if (std.mem.eql(u8, label, "const")) saw_keyword_const = true;
    }

    try std.testing.expect(saw_version);
    try std.testing.expect(saw_add);
    try std.testing.expect(saw_stdout);
    try std.testing.expect(!saw_keyword_const);
}

test "lsp keyword completion emits snippets only when the client supports them" {
    const allocator = std.testing.allocator;

    // A helper: run a completion at a `co` prefix and return the `const`
    // keyword item's insertTextFormat (null when absent) plus its insertText.
    const Probe = struct {
        fn run(alloc: Allocator, snippet_support: bool) !struct { format: ?i64, insert_text: ?[]u8 } {
            var fixture = try TestFixture.init(alloc);
            defer fixture.deinit();

            const uri = try fixture.writeDocument("main.rn", "co\n");
            defer alloc.free(uri);

            const messages = [_][]const u8{
                try makeInitialize(alloc, 1, snippet_support),
                try makeDidOpen(alloc, uri, "co\n"),
                try makeCompletionRequest(alloc, 2, uri, 0, 2),
            };
            defer for (messages) |message| alloc.free(message);

            const output = try runServerWithMessages(alloc, &messages);
            defer alloc.free(output);

            const response = try findResponseById(alloc, output, 2);
            defer alloc.free(response.body);

            const parsed = try std.json.parseFromSlice(std.json.Value, alloc, response.body, .{});
            defer parsed.deinit();

            const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
            for (items) |item| {
                const label = item.object.get("label").?.string;
                if (!std.mem.eql(u8, label, "const")) continue;
                // Optional fields serialize as JSON `null` when unset, so treat
                // a present-but-null value the same as an absent one.
                const format_value = item.object.get("insertTextFormat");
                const format: ?i64 = if (format_value != null and format_value.? != .null) format_value.?.integer else null;
                const text_value = item.object.get("insertText");
                const insert_text: ?[]u8 = if (text_value != null and text_value.? != .null) try alloc.dupe(u8, text_value.?.string) else null;
                return .{ .format = format, .insert_text = insert_text };
            }
            return error.KeywordCompletionMissing;
        }
    };

    // With snippet support the `const` keyword completes to a snippet
    // (insertTextFormat = 2) carrying tab stops.
    const supported = try Probe.run(allocator, true);
    defer if (supported.insert_text) |t| allocator.free(t);
    try std.testing.expectEqual(@as(?i64, 2), supported.format);
    try std.testing.expect(supported.insert_text != null);
    try std.testing.expect(std.mem.indexOf(u8, supported.insert_text.?, "${1:name}") != null);

    // Without snippet support the completion inserts its label verbatim — no
    // snippet format and no raw `${1:...}` tab stops leaking to the editor.
    const unsupported = try Probe.run(allocator, false);
    defer if (unsupported.insert_text) |t| allocator.free(t);
    try std.testing.expectEqual(@as(?i64, null), unsupported.format);
    try std.testing.expect(unsupported.insert_text == null);
}

test "lsp hover shows execution result type for bound command" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const h = echo "hello"
        \\echo "${h.stdout}"
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const h = echo "hello"
            \\echo "${h.stdout}"
            \\
        ),
        try makeHoverRequest(allocator, 10, uri, 0, 6),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 10);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const contents = parsed.value.object.get("result").?.object.get("contents").?.object;
    // MarkupContent.kind is a MarkupKind, which the protocol encodes as a STRING
    // ("markdown"/"plaintext") — not a numeric code like the other kind enums.
    const kind = contents.get("kind").?.string;
    try std.testing.expect(std.mem.eql(u8, kind, "markdown") or std.mem.eql(u8, kind, "plaintext"));
    const value = contents.get("value").?.string;
    try std.testing.expect(std.mem.indexOf(u8, value, "ExecutionResult") != null);
}

test "lsp hover shows execution result member type" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const h = echo "hello"
        \\echo h.stdout
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const h = echo "hello"
            \\echo h.stdout
            \\
        ),
        try makeHoverRequest(allocator, 12, uri, 1, 7),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 12);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const value = parsed.value.object.get("result").?.object.get("contents").?.object.get("value").?.string;
    try std.testing.expect(std.mem.indexOf(u8, value, "stdout") != null);
    try std.testing.expect(std.mem.indexOf(u8, value, "Byte") != null);
}

test "lsp hover shows a cimport extern's C signature" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const c = import "std/ffi.rn"
        \\const m = cimport "libm.so.6" {
        \\    extern fn pow(base: c.Double, exp: c.Double) c.Double
        \\}
        \\echo "${m.pow 2.0 10.0}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Cursor on `pow` in `m.pow` (line 4).
        try makeHoverRequest(allocator, 13, uri, 4, 11),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 13);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const value = parsed.value.object.get("result").?.object.get("contents").?.object.get("value").?.string;
    try std.testing.expect(std.mem.indexOf(u8, value, "extern fn pow(base: c.Double, exp: c.Double) c.Double") != null);
}

test "lsp hover types an alias to a module's cimport value" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const bindings_uri = try fixture.writeDocument("bindings.rn",
        \\const c = import "std/ffi.rn"
        \\const raylib = cimport "libraylib.so" {
        \\    extern fn InitWindow(width: c.Int, height: c.Int) c.Int
        \\}
        \\
    );
    defer allocator.free(bindings_uri);

    const main_source =
        \\const rl = import "./bindings.rn"
        \\const rlf = rl.raylib
        \\echo "${rlf.InitWindow 800 600}"
        \\
    ;
    const main_uri = try fixture.writeDocument("main.rn", main_source);
    defer allocator.free(main_uri);

    const root_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{fixture.root_path});
    defer allocator.free(root_uri);

    const messages = [_][]const u8{
        try makeInitializeWithRoot(allocator, 1, root_uri),
        try makeDidOpen(allocator, main_uri, main_source),
        // Cursor on `rlf` in its `const rlf = rl.raylib` declaration (line 1).
        try makeHoverRequest(allocator, 20, main_uri, 1, 6),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 20);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    // The alias must be typed (previously null): its type is the cimport value,
    // shown concisely as a `cimport` summary rather than the full extern struct.
    const value = parsed.value.object.get("result").?.object.get("contents").?.object.get("value").?.string;
    try std.testing.expect(std.mem.indexOf(u8, value, "cimport") != null);
    try std.testing.expect(std.mem.indexOf(u8, value, "1 extern") != null);
}

test "lsp hover types a module's cimport-value member" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const bindings_uri = try fixture.writeDocument("bindings.rn",
        \\const c = import "std/ffi.rn"
        \\const raylib = cimport "libraylib.so" {
        \\    extern fn InitWindow(width: c.Int, height: c.Int) c.Int
        \\}
        \\
    );
    defer allocator.free(bindings_uri);

    const main_source =
        \\const rl = import "./bindings.rn"
        \\const rlf = rl.raylib
        \\echo "${rlf.InitWindow 800 600}"
        \\
    ;
    const main_uri = try fixture.writeDocument("main.rn", main_source);
    defer allocator.free(main_uri);

    const root_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{fixture.root_path});
    defer allocator.free(root_uri);

    const messages = [_][]const u8{
        try makeInitializeWithRoot(allocator, 1, root_uri),
        try makeDidOpen(allocator, main_uri, main_source),
        // Cursor on `raylib` in `rl.raylib` (line 1, col 17) — the module member,
        // not the `rlf` alias. Previously this member hover showed no type.
        try makeHoverRequest(allocator, 21, main_uri, 1, 17),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 21);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    // The module member `rl.raylib` types as the cimport value, shown as the
    // same concise `cimport` summary as its `rlf` alias.
    const value = parsed.value.object.get("result").?.object.get("contents").?.object.get("value").?.string;
    try std.testing.expect(std.mem.indexOf(u8, value, "cimport") != null);
    try std.testing.expect(std.mem.indexOf(u8, value, "1 extern") != null);
}

test "lsp member completion shows execution result members" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const h = echo "hello"
        \\echo h.
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const h = echo "hello"
            \\echo h.
            \\
        ),
        try makeCompletionRequest(allocator, 11, uri, 1, 7),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 11);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    try std.testing.expect(items.len > 0);

    var saw_stdout = false;
    var saw_stderr = false;
    var saw_exit_code = false;
    var saw_wait = false;
    var saw_keyword_const = false;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        if (std.mem.eql(u8, label, "stdout")) saw_stdout = true;
        if (std.mem.eql(u8, label, "stderr")) saw_stderr = true;
        if (std.mem.eql(u8, label, "exit_code")) saw_exit_code = true;
        if (std.mem.eql(u8, label, "wait")) saw_wait = true;
        if (std.mem.eql(u8, label, "const")) saw_keyword_const = true;
    }

    try std.testing.expect(saw_stdout);
    try std.testing.expect(saw_stderr);
    try std.testing.expect(saw_exit_code);
    try std.testing.expect(saw_wait);
    try std.testing.expect(!saw_keyword_const);

    const first_kind = items[0].object.get("kind").?;
    try std.testing.expect(first_kind == .integer);
}

test "lsp member completion on an imported module excludes injected globals" {
    // Completing `m.` must list the module's own pub exports, not the builtins
    // and primitive types the type checker injects into every module scope
    // (`parseInt`, `parseFloat`, `Int`, …).
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const module_uri = try fixture.writeDocument("module.rn",
        \\pub const version = "0.0.1"
        \\pub fn Void greet() Void { echo "hi" }
        \\
    );
    defer allocator.free(module_uri);

    const main_src =
        \\const m = import "./module.rn"
        \\echo m.
        \\
    ;
    const main_uri = try fixture.writeDocument("main.rn", main_src);
    defer allocator.free(main_uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, module_uri,
            \\pub const version = "0.0.1"
            \\pub fn Void greet() Void { echo "hi" }
            \\
        ),
        try makeDidOpen(allocator, main_uri, main_src),
        try makeCompletionRequest(allocator, 30, main_uri, 1, 7),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 30);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var saw_version = false;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        if (std.mem.eql(u8, label, "version")) saw_version = true;
        // Injected globals must never appear as module members.
        try std.testing.expect(!std.mem.eql(u8, label, "parseInt"));
        try std.testing.expect(!std.mem.eql(u8, label, "parseFloat"));
        try std.testing.expect(!std.mem.eql(u8, label, "Int"));
    }
    try std.testing.expect(saw_version);
}

test "lsp member completion lists a cimport's extern functions" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const c = import "std/ffi.rn"
        \\const m = cimport "libm.so.6" {
        \\    extern fn pow(base: c.Double, exp: c.Double) c.Double
        \\    extern fn cos(x: c.Double) c.Double
        \\}
        \\m.
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Cursor just after `m.` on line 5.
        try makeCompletionRequest(allocator, 21, uri, 5, 2),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 21);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var saw_pow = false;
    var saw_cos = false;
    var pow_detail: ?[]const u8 = null;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        if (std.mem.eql(u8, label, "pow")) {
            saw_pow = true;
            if (item.object.get("detail")) |d| pow_detail = d.string;
        }
        if (std.mem.eql(u8, label, "cos")) saw_cos = true;
    }

    try std.testing.expect(saw_pow);
    try std.testing.expect(saw_cos);
    // The detail carries the extern's C signature.
    try std.testing.expect(pow_detail != null);
    try std.testing.expectEqualStrings("pow(base: c.Double, exp: c.Double) c.Double", pow_detail.?);
}

test "lsp module-path completion follows a symlinked module file" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const real_uri = try fixture.writeDocument("real.rn",
        \\pub const value = 1
        \\
    );
    allocator.free(real_uri);
    // linked.rn -> real.rn (a symlink to a module file).
    try fixture.tmp_dir.dir.symLink(std.testing.io, "real.rn", "linked.rn", .{});

    const main_uri = try fixture.writeDocument("main.rn", "const m = import \"./l\"\n");
    defer allocator.free(main_uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, main_uri, "const m = import \"./l\"\n"),
        // Cursor after `./l` inside the import string on line 0.
        try makeCompletionRequest(allocator, 2, main_uri, 0, 21),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var saw_linked = false;
    for (items) |item| {
        if (std.mem.eql(u8, item.object.get("label").?.string, "linked.rn")) saw_linked = true;
    }
    try std.testing.expect(saw_linked);
}

test "lsp completion offers executables found on PATH" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // A fake executable in a bin directory that PATH will point at.
    try fixture.tmp_dir.dir.createDirPath(std.testing.io, "bin");
    try fixture.tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = "bin/myfakecmd", .data = "#!/bin/sh\n" });
    const bin_dir = try std.fmt.allocPrint(allocator, "{s}/bin", .{fixture.root_path});
    defer allocator.free(bin_dir);

    const uri = try fixture.writeDocument("main.rn", "myfake\n");
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeInitialize(allocator, 1, false),
        try makeDidOpen(allocator, uri, "myfake\n"),
        try makeCompletionRequest(allocator, 2, uri, 0, 6),
    };
    defer for (messages) |message| allocator.free(message);

    const result = try runServerImpl(allocator, &messages, bin_dir);
    defer result.deinit(allocator);

    const response = try findResponseById(allocator, result.stdout, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var cmd_detail: ?[]const u8 = null;
    for (items) |item| {
        if (std.mem.eql(u8, item.object.get("label").?.string, "myfakecmd")) {
            cmd_detail = if (item.object.get("detail")) |d| d.string else null;
        }
    }
    try std.testing.expect(cmd_detail != null);
    // Detail is the resolved path to the executable.
    try std.testing.expect(std.mem.endsWith(u8, cmd_detail.?, "bin/myfakecmd"));
}

test "lsp completion shows a function's signature as detail" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\fn Void greet(name: String, times: Int) Void {
        \\    echo "hi"
        \\}
        \\gr
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Cursor after the `gr` prefix on line 3.
        try makeCompletionRequest(allocator, 12, uri, 3, 2),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 12);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var greet_detail: ?[]const u8 = null;
    for (items) |item| {
        if (std.mem.eql(u8, item.object.get("label").?.string, "greet")) {
            greet_detail = if (item.object.get("detail")) |d| d.string else null;
        }
    }
    try std.testing.expect(greet_detail != null);
    try std.testing.expectEqualStrings("(name: String, times: Int) Void", greet_detail.?);
}

test "lsp member completion shows a struct field's type as detail" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const Point = struct { x: Int, y: String }
        \\const p = Point{ .x = 1, .y = "a" }
        \\echo "${p.}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeCompletionRequest(allocator, 12, uri, 2, 10),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 12);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var x_detail: ?[]const u8 = null;
    var y_detail: ?[]const u8 = null;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        const detail = if (item.object.get("detail")) |d| d.string else null;
        if (std.mem.eql(u8, label, "x")) x_detail = detail;
        if (std.mem.eql(u8, label, "y")) y_detail = detail;
    }
    try std.testing.expect(x_detail != null and y_detail != null);
    try std.testing.expectEqualStrings("Int", x_detail.?);
    try std.testing.expectEqualStrings("String", y_detail.?);
}

test "lsp member completion lists an error set's variants with payload detail" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const E = error { NotFound, Failed: String }
        \\echo "${E.}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeCompletionRequest(allocator, 12, uri, 1, 10),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 12);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var saw_not_found = false;
    var failed_detail: ?[]const u8 = null;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        if (std.mem.eql(u8, label, "NotFound")) saw_not_found = true;
        if (std.mem.eql(u8, label, "Failed")) {
            failed_detail = if (item.object.get("detail")) |d| d.string else null;
        }
    }
    try std.testing.expect(saw_not_found);
    try std.testing.expect(failed_detail != null);
    try std.testing.expect(std.mem.indexOf(u8, failed_detail.?, "String") != null);
}

test "lsp hover shows an error variant with its payload type" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const E = error { Failed: String }
        \\fn Void f() E!Int { yield E.Failed }
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Cursor on `Failed` in `E.Failed` (line 1).
        try makeHoverRequest(allocator, 12, uri, 1, 28),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 12);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const value = parsed.value.object.get("result").?.object.get("contents").?.object.get("value").?.string;
    try std.testing.expect(std.mem.indexOf(u8, value, "Failed") != null);
    try std.testing.expect(std.mem.indexOf(u8, value, "String") != null);
}

test "lsp chained member completion resolves nested struct fields" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const Inner = struct { value: Int, label: String }
        \\const Outer = struct { inner: Inner }
        \\const o = Outer{ .inner = Inner{ .value = 1, .label = "x" } }
        \\echo "${o.inner.}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Cursor right after the trailing dot in `o.inner.` on line 3 — a syntax
        // error that requires scope recovery.
        try makeCompletionRequest(allocator, 12, uri, 3, 16),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 12);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var saw_value = false;
    var saw_label = false;
    var saw_inner = false;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        if (std.mem.eql(u8, label, "value")) saw_value = true;
        if (std.mem.eql(u8, label, "label")) saw_label = true;
        if (std.mem.eql(u8, label, "inner")) saw_inner = true;
    }

    // The chain resolved through `Inner` even though the trailing dot makes the
    // document unparseable — both of `Inner`'s fields complete...
    try std.testing.expect(saw_value);
    try std.testing.expect(saw_label);
    // ...and the outer struct's field does not leak into the inner completion.
    try std.testing.expect(!saw_inner);
}

test "lsp single-level member completion recovers scope after a trailing dot" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source =
        \\const Point = struct { x: Int, y: Int }
        \\const p = Point{ .x = 1, .y = 2 }
        \\echo "${p.}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        // Cursor right after the dot in `p.` on line 2.
        try makeCompletionRequest(allocator, 12, uri, 2, 10),
    };
    defer for (messages) |message| allocator.free(message);

    const result = try runServerWithMessagesDetailed(allocator, &messages);
    defer result.deinit(allocator);

    const response = try findResponseById(allocator, result.stdout, 12);
    defer allocator.free(response.body);

    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var saw_x = false;
    var saw_y = false;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        if (std.mem.eql(u8, label, "x")) saw_x = true;
        if (std.mem.eql(u8, label, "y")) saw_y = true;
    }

    try std.testing.expect(saw_x);
    try std.testing.expect(saw_y);
    // The scratch recovery document was closed again, leaving only the real one.
    try std.testing.expectEqual(@as(usize, 1), result.doc_count);
    try std.testing.expectEqualStrings("", result.stderr);
}

test "lsp initialize advertises capabilities with the correct wire shape" {
    const allocator = std.testing.allocator;
    const messages = [_][]const u8{
        try makeInitialize(allocator, 1, true),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 1);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const root = parsed.value.object;
    // Envelope: a success response carries jsonrpc "2.0" + result and MUST NOT
    // carry a stray `error` field. sendJson serializes with
    // emit_null_optional_fields = false so the JSON-RPC result-XOR-error shape
    // holds; a regression to emit-nulls would put `"error": null` here.
    try std.testing.expectEqualStrings("2.0", root.get("jsonrpc").?.string);
    try std.testing.expect(root.get("result") != null);
    try std.testing.expect(root.get("error") == null);

    const caps = root.get("result").?.object.get("capabilities").?.object;

    // textDocumentSync is an Either(options, kind): it must unwrap to the bare
    // options object (never a `{"textDocumentSyncOptions": ...}` tag envelope),
    // and its `change` must be the numeric TextDocumentSyncKind, not a tag name.
    const sync = caps.get("textDocumentSync").?.object;
    try std.testing.expect(sync.get("textDocumentSyncOptions") == null);
    try std.testing.expectEqual(true, sync.get("openClose").?.bool);
    try std.testing.expectEqual(@as(i64, 2), sync.get("change").?.integer); // incremental

    // A bool-or-options Either capability likewise unwraps to its options object.
    const rename = caps.get("renameProvider").?.object;
    try std.testing.expect(rename.get("renameOptions") == null);
    try std.testing.expectEqual(true, rename.get("prepareProvider").?.bool);

    // completionProvider carries resolveProvider + trigger characters.
    const completion_provider = caps.get("completionProvider").?.object;
    try std.testing.expectEqual(true, completion_provider.get("resolveProvider").?.bool);
    try std.testing.expect(completion_provider.get("triggerCharacters").?.array.items.len > 0);

    // The rest of the advertised providers must be present.
    const providers = [_][]const u8{
        "definitionProvider",        "referencesProvider",
        "hoverProvider",             "documentSymbolProvider",
        "documentHighlightProvider", "documentLinkProvider",
        "inlayHintProvider",         "foldingRangeProvider",
        "codeActionProvider",        "workspaceSymbolProvider",
    };
    for (providers) |name| {
        try std.testing.expect(caps.get(name) != null);
    }
}

test "lsp success responses carry a result and omit the error field" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\
        ),
        try makeDocumentSymbolRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    // A normal (non-initialize) success response must also be result-XOR-error:
    // the `result` is present and the `error` field is absent, not `null`.
    const root = parsed.value.object;
    try std.testing.expectEqualStrings("2.0", root.get("jsonrpc").?.string);
    try std.testing.expect(root.get("result") != null);
    try std.testing.expect(root.get("error") == null);
}

// A single edit/location range, flattened for exact-coordinate assertions.
const RangeSpan = struct {
    line: i64,
    start_char: i64,
    end_line: i64,
    end_char: i64,
    new_text: []const u8,
};

fn spanOf(obj: std.json.ObjectMap, new_text_key: ?[]const u8) RangeSpan {
    const range = obj.get("range").?.object;
    const start = range.get("start").?.object;
    const end = range.get("end").?.object;
    return .{
        .line = start.get("line").?.integer,
        .start_char = start.get("character").?.integer,
        .end_line = end.get("line").?.integer,
        .end_char = end.get("character").?.integer,
        .new_text = if (new_text_key) |k| obj.get(k).?.string else "",
    };
}

test "lsp rename edits span exactly the identifier at precise coordinates" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // `alpha` appears at (0,6)-(0,11) and (1,13)-(1,18): non-zero line and a
    // non-zero column, so both the line and character conversions are exercised,
    // and the 5-char span guards the range width.
    const source =
        \\const alpha = 1
        \\const beta = alpha
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeRenameRequest(allocator, 2, uri, 0, 6, "renamed"),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const changes = parsed.value.object.get("result").?.object.get("documentChanges").?.array.items;
    try std.testing.expectEqual(@as(usize, 1), changes.len);
    const edits = changes[0].object.get("edits").?.array.items;
    try std.testing.expectEqual(@as(usize, 2), edits.len);

    var saw_decl = false;
    var saw_use = false;
    for (edits) |edit| {
        const span = spanOf(edit.object, "newText");
        try std.testing.expectEqualStrings("renamed", span.new_text);
        // Every edit is single-line and spans exactly the 5 chars of `alpha`.
        try std.testing.expectEqual(span.line, span.end_line);
        try std.testing.expectEqual(span.start_char + 5, span.end_char);
        if (span.line == 0 and span.start_char == 6) saw_decl = true;
        if (span.line == 1 and span.start_char == 13) saw_use = true;
    }
    try std.testing.expect(saw_decl);
    try std.testing.expect(saw_use);
}

test "lsp references report exact identifier ranges" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // `count` at declaration (0,6)-(0,11) and use (1,8)-(1,13) — inside the
    // interpolation `echo "${count}"`, where `${` occupies cols 6-7.
    const source =
        \\const count = 3
        \\echo "${count}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeReferencesRequest(allocator, 2, uri, 0, 6, true),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const results = parsed.value.object.get("result").?.array.items;
    try std.testing.expectEqual(@as(usize, 2), results.len);

    var saw_decl = false;
    var saw_use = false;
    for (results) |item| {
        try std.testing.expectEqualStrings(uri, item.object.get("uri").?.string);
        const span = spanOf(item.object, null);
        // Every occurrence is single-line and spans exactly `count` (5 chars).
        try std.testing.expectEqual(span.line, span.end_line);
        try std.testing.expectEqual(span.start_char + 5, span.end_char);
        if (span.line == 0 and span.start_char == 6) saw_decl = true;
        if (span.line == 1 and span.start_char == 8) saw_use = true;
    }
    try std.testing.expect(saw_decl);
    try std.testing.expect(saw_use);
}

test "lsp survives out-of-bounds position requests" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\
        ),
        // A line far past EOF and an over-long character — both used to run the
        // position scan off the end of the buffer and crash the server.
        try makeHoverRequest(allocator, 2, uri, 999, 999),
        try makeDefinitionRequest(allocator, 3, uri, 0, 999),
        // A valid request afterwards must still be answered — proof the server
        // stayed alive through the out-of-bounds ones.
        try makeDocumentSymbolRequest(allocator, 4, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // Each out-of-bounds request gets a well-formed response instead of taking
    // the server down.
    for ([_]i64{ 2, 3 }) |id| {
        const response = try findResponseById(allocator, output, id);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        try std.testing.expect(parsed.value.object.get("result") != null);
    }

    // The later valid request was still served.
    const final = try findResponseById(allocator, output, 4);
    defer allocator.free(final.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, final.body, .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value.object.get("result").?.array.items.len >= 1);
}

test "lsp survives position requests on an empty document" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn", "");
    defer allocator.free(uri);

    // An empty document put the position→offset scan at offset 0 of a zero-length
    // buffer; `extractIdentifier` read `text[0]` and crashed the server.
    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, ""),
        try makeHoverRequest(allocator, 2, uri, 0, 0),
        try makeDefinitionRequest(allocator, 3, uri, 0, 0),
        try makeDocumentHighlightRequest(allocator, 4, uri, 0, 0),
        try makeRenameRequest(allocator, 5, uri, 0, 0, "x"),
        // A later request proves the server stayed alive.
        try makeDocumentSymbolRequest(allocator, 6, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    for ([_]i64{ 2, 3, 4, 5, 6 }) |id| {
        const response = try findResponseById(allocator, output, id);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        try std.testing.expect(parsed.value.object.get("result") != null);
    }
}

test "lsp survives requests on a document with a multibyte identifier" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // A non-ASCII identifier is a byte the lexer rejects; scanning the whole
    // document (rename/highlight occurrence search, the import scan for member
    // completion) used to propagate that error and take the server down.
    const source =
        \\const foo = 1
        \\const 名前 = foo
        \\echo "${foo}"
        \\
    ;
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeDocumentHighlightRequest(allocator, 2, uri, 0, 6),
        try makeRenameRequest(allocator, 3, uri, 0, 6, "bar"),
        try makeCompletionRequest(allocator, 4, uri, 2, 11),
        // Still alive afterwards.
        try makeDocumentSymbolRequest(allocator, 5, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    for ([_]i64{ 2, 3, 4, 5 }) |id| {
        const response = try findResponseById(allocator, output, id);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        try std.testing.expect(parsed.value.object.get("result") != null);
    }
}

test "lsp returns method-not-found for an unknown request method" {
    const allocator = std.testing.allocator;
    const request = try toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = 7,
        .method = "textDocument/thisMethodDoesNotExist",
        .params = .{},
    });
    const messages = [_][]const u8{request};
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    const response = try findResponseById(allocator, output, 7);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    // Error response: `error` present with the JSON-RPC method-not-found code,
    // and no `result` field (result-XOR-error).
    const root = parsed.value.object;
    try std.testing.expect(root.get("result") == null);
    const err = root.get("error").?.object;
    try std.testing.expectEqual(@as(i64, -32601), err.get("code").?.integer);
}

test "lsp rejects a second initialize with an already-initialized error" {
    const allocator = std.testing.allocator;
    const messages = [_][]const u8{
        try makeInitialize(allocator, 1, true),
        try makeInitialize(allocator, 2, true),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // First initialize succeeds.
    {
        const response = try findResponseById(allocator, output, 1);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        try std.testing.expect(parsed.value.object.get("result") != null);
        try std.testing.expect(parsed.value.object.get("error") == null);
    }
    // The second is rejected as invalid (-32600), not silently re-run.
    {
        const response = try findResponseById(allocator, output, 2);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        try std.testing.expect(parsed.value.object.get("result") == null);
        const err = parsed.value.object.get("error").?.object;
        try std.testing.expectEqual(@as(i64, -32600), err.get("code").?.integer);
    }
}

test "lsp clears diagnostics once an invalid document is fixed" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo =
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        // Open with an incomplete binding (a parse error), then fix it.
        try makeDidOpen(allocator, uri,
            \\const foo =
            \\
        ),
        try makeDidChangeWholeDocument(allocator, uri, 2,
            \\const foo = 1
            \\
        ),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // The first publish (on open) reports the error.
    {
        const first = try findMethodNotification(allocator, output, "textDocument/publishDiagnostics");
        defer allocator.free(first);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, first, .{});
        defer parsed.deinit();
        const diagnostics = parsed.value.object.get("params").?.object.get("diagnostics").?.array.items;
        try std.testing.expect(diagnostics.len > 0);
    }

    // After the fix, the server MUST republish an empty diagnostics array to
    // clear the editor — not simply stop publishing, which would leave the stale
    // error on screen.
    {
        const last = try findLastMethodNotification(allocator, output, "textDocument/publishDiagnostics");
        defer allocator.free(last);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, last, .{});
        defer parsed.deinit();
        const params = parsed.value.object.get("params").?.object;
        try std.testing.expectEqualStrings(uri, params.get("uri").?.string);
        try std.testing.expectEqual(@as(usize, 0), params.get("diagnostics").?.array.items.len);
    }
}

test "lsp survives a malformed request body" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn",
        \\const foo = 1
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        // A body that is framed correctly but is not valid JSON — it must be
        // dropped, not end the session.
        try allocator.dupe(u8, "{ this is : not valid json ]"),
        // A valid session afterwards must still be served.
        try makeDidOpen(allocator, uri,
            \\const foo = 1
            \\
        ),
        try makeDocumentSymbolRequest(allocator, 2, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // The request after the malformed one was answered → the server stayed up.
    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();
    try std.testing.expect(parsed.value.object.get("result") != null);
}

test "lsp handles navigation requests for a never-opened document" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    // The file exists on disk but is never sent via didOpen, so the server has
    // no in-memory copy. Every position handler must degrade to a null/empty
    // result rather than dereferencing the missing document.
    const uri = try fixture.writeDocument("ghost.rn",
        \\const foo = 1
        \\echo foo
        \\
    );
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeHoverRequest(allocator, 1, uri, 1, 5),
        try makeDefinitionRequest(allocator, 2, uri, 1, 5),
        try makeReferencesRequest(allocator, 3, uri, 0, 6, true),
        try makeRenameRequest(allocator, 4, uri, 0, 6, "bar"),
        try makeDocumentSymbolRequest(allocator, 5, uri),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);

    // Every request gets a well-formed response (the `result` key is present),
    // proving none of the handlers crashed on the missing document.
    for ([_]i64{ 1, 2, 3, 4, 5 }) |id| {
        const response = try findResponseById(allocator, output, id);
        defer allocator.free(response.body);
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
        defer parsed.deinit();
        try std.testing.expect(parsed.value.object.get("result") != null);
    }
}

test "lsp completion offers the cimport keyword" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const uri = try fixture.writeDocument("main.rn", "ci\n");
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeInitialize(allocator, 1, true),
        try makeDidOpen(allocator, uri, "ci\n"),
        try makeCompletionRequest(allocator, 2, uri, 0, 2),
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);
    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var saw_cimport = false;
    for (items) |item| {
        if (std.mem.eql(u8, item.object.get("label").?.string, "cimport")) saw_cimport = true;
    }
    try std.testing.expect(saw_cimport);
}

test "lsp import-path completion offers the bundled std root" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source = "const m = import \"st\"\n";
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeCompletionRequest(allocator, 2, uri, 0, 20), // after `st` in the import string
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);
    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var saw_std = false;
    for (items) |item| {
        if (std.mem.eql(u8, item.object.get("label").?.string, "std")) saw_std = true;
    }
    try std.testing.expect(saw_std);
}

test "lsp import-path completion offers bundled std submodules" {
    const allocator = std.testing.allocator;
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const source = "const m = import \"std/\"\n";
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeCompletionRequest(allocator, 2, uri, 0, 22), // after `std/` in the import string
    };
    defer for (messages) |message| allocator.free(message);

    const output = try runServerWithMessages(allocator, &messages);
    defer allocator.free(output);
    const response = try findResponseById(allocator, output, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    var saw_list = false;
    var saw_ffi = false;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        if (std.mem.eql(u8, label, "list")) saw_list = true;
        if (std.mem.eql(u8, label, "ffi")) saw_ffi = true;
    }
    try std.testing.expect(saw_list);
    try std.testing.expect(saw_ffi);
}

/// Runs a single trailing-dot member completion and reports whether each of
/// `expected` appears among the offered labels.
fn memberCompletionHas(
    allocator: Allocator,
    source: []const u8,
    line: u32,
    character: u32,
    expected: []const []const u8,
    found: []bool,
) !void {
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();
    const uri = try fixture.writeDocument("main.rn", source);
    defer allocator.free(uri);

    const messages = [_][]const u8{
        try makeDidOpen(allocator, uri, source),
        try makeCompletionRequest(allocator, 2, uri, line, character),
    };
    defer for (messages) |message| allocator.free(message);

    const result = try runServerWithMessagesDetailed(allocator, &messages);
    defer result.deinit(allocator);
    const response = try findResponseById(allocator, result.stdout, 2);
    defer allocator.free(response.body);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, response.body, .{});
    defer parsed.deinit();

    const items = parsed.value.object.get("result").?.object.get("items").?.array.items;
    for (items) |item| {
        const label = item.object.get("label").?.string;
        for (expected, found) |want, *seen| {
            if (std.mem.eql(u8, label, want)) seen.* = true;
        }
    }
}

test "lsp completion offers std submodules after a trailing dot" {
    const source =
        \\const std = import "std"
        \\echo "${std.}"
        \\
    ;
    var found = [_]bool{ false, false, false };
    try memberCompletionHas(std.testing.allocator, source, 1, 12, &.{ "list", "str", "ffi" }, &found);
    for (found) |f| try std.testing.expect(f);
}

test "lsp completion offers cimport externs after a trailing dot" {
    const source =
        \\const c = import "std/ffi.rn"
        \\const m = cimport "libm.so.6" {
        \\    extern fn pow(base: c.Double, exp: c.Double) c.Double
        \\    extern fn cos(x: c.Double) c.Double
        \\}
        \\echo "${m.}"
        \\
    ;
    var found = [_]bool{ false, false };
    try memberCompletionHas(std.testing.allocator, source, 5, 10, &.{ "pow", "cos" }, &found);
    for (found) |f| try std.testing.expect(f);
}

test "lsp completion offers std.ffi C types after a trailing dot" {
    const source =
        \\const c = import "std/ffi.rn"
        \\echo "${c.}"
        \\
    ;
    var found = [_]bool{ false, false, false };
    try memberCompletionHas(std.testing.allocator, source, 1, 10, &.{ "Double", "Int", "Ptr" }, &found);
    for (found) |f| try std.testing.expect(f);
}

const TestFixture = struct {
    allocator: Allocator,
    tmp_dir: std.testing.TmpDir,
    root_path: [:0]const u8,

    fn init(allocator: Allocator) !TestFixture {
        var tmp_dir = std.testing.tmpDir(.{});
        const relative_root_path = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", tmp_dir.sub_path[0..] });
        defer allocator.free(relative_root_path);
        const root_path = try std.Io.Dir.cwd().realPathFileAlloc(std.testing.io, relative_root_path, allocator);
        return .{
            .allocator = allocator,
            .tmp_dir = tmp_dir,
            .root_path = root_path,
        };
    }

    fn deinit(self: *TestFixture) void {
        self.tmp_dir.cleanup();
        self.allocator.free(self.root_path);
    }

    fn writeDocument(self: *TestFixture, name: []const u8, text: []const u8) ![]u8 {
        try self.tmp_dir.dir.writeFile(std.testing.io, .{ .sub_path = name, .data = text });
        const path = try std.fs.path.join(self.allocator, &.{ self.root_path, name });
        defer self.allocator.free(path);
        return try std.fmt.allocPrint(self.allocator, "file://{s}", .{path});
    }
};

fn runServerWithMessages(allocator: Allocator, messages: []const []const u8) ![]u8 {
    const result = try runServerWithMessagesDetailed(allocator, messages);
    defer allocator.free(result.stderr);
    return @constCast(result.stdout);
}

fn runServerWithMessagesDetailed(allocator: Allocator, messages: []const []const u8) !ServerRunResult {
    return runServerImpl(allocator, messages, null);
}

fn runServerImpl(allocator: Allocator, messages: []const []const u8, path_env: ?[]const u8) !ServerRunResult {
    var fixture = try TestFixture.init(allocator);
    defer fixture.deinit();

    const input_path = try std.fs.path.join(allocator, &.{ fixture.root_path, "stdin.jsonrpc" });
    defer allocator.free(input_path);
    const output_path = try std.fs.path.join(allocator, &.{ fixture.root_path, "stdout.jsonrpc" });
    defer allocator.free(output_path);
    const error_path = try std.fs.path.join(allocator, &.{ fixture.root_path, "stderr.log" });
    defer allocator.free(error_path);

    const io = std.testing.io;
    var input_file = try std.Io.Dir.createFileAbsolute(io, input_path, .{ .read = true, .truncate = true });
    defer input_file.close(io);
    var output_file = try std.Io.Dir.createFileAbsolute(io, output_path, .{ .read = true, .truncate = true });
    defer output_file.close(io);
    var error_file = try std.Io.Dir.createFileAbsolute(io, error_path, .{ .read = true, .truncate = true });
    defer error_file.close(io);

    {
        // Positional writer: pwrite leaves the OS file offset at 0, so the
        // server's streaming reader still sees the messages from the start.
        var write_buffer: [4096]u8 = undefined;
        var writer = input_file.writer(io, &write_buffer);
        for (messages) |message| {
            try writer.interface.print("Content-Length: {d}\r\n\r\n{s}", .{ message.len, message });
        }
        try writer.interface.flush();
    }

    var env_map = std.process.Environ.Map.init(allocator);
    defer env_map.deinit();
    if (path_env) |p| try env_map.put("PATH", p);

    var server = try lsp.server.Server.init(io, allocator, &env_map, input_file, output_file, error_file);
    defer server.deinit();
    server.initInterface();
    try server.run();

    const recheck_count = server.documents.recheck_count;
    const doc_count = server.documents.map.count();

    // Positional readers pread from offset 0, regardless of where the server
    // left the shared OS file offset after writing.
    var output_reader = output_file.reader(io, &.{});
    const stdout = try output_reader.interface.allocRemaining(allocator, .unlimited);
    var error_reader = error_file.reader(io, &.{});
    const stderr = try error_reader.interface.allocRemaining(allocator, .unlimited);
    return .{
        .stdout = stdout,
        .stderr = stderr,
        .recheck_count = recheck_count,
        .doc_count = doc_count,
    };
}

fn findResponseById(allocator: Allocator, output: []const u8, wanted_id: i64) !ProtocolResponse {
    var offset: usize = 0;
    while (offset < output.len) {
        const header_end_rel = std.mem.indexOfPos(u8, output, offset, "\r\n\r\n") orelse return error.InvalidProtocolMessage;
        const header_block = output[offset..header_end_rel];
        const content_length = parseContentLength(header_block) orelse return error.InvalidProtocolMessage;
        const body_start = header_end_rel + 4;
        const body_end = body_start + content_length;
        if (body_end > output.len) return error.InvalidProtocolMessage;

        const body = output[body_start..body_end];
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, body, .{});
        defer parsed.deinit();

        const body_id = parsed.value.object.get("id");
        if (body_id != null and body_id.? == .integer and body_id.?.integer == wanted_id) {
            return .{
                .id = wanted_id,
                .body = try allocator.dupe(u8, body),
            };
        }

        offset = body_end;
    }

    return error.ResponseNotFound;
}

fn findMethodNotification(allocator: Allocator, output: []const u8, wanted_method: []const u8) ![]u8 {
    var offset: usize = 0;
    while (offset < output.len) {
        const header_end_rel = std.mem.indexOfPos(u8, output, offset, "\r\n\r\n") orelse return error.InvalidProtocolMessage;
        const header_block = output[offset..header_end_rel];
        const content_length = parseContentLength(header_block) orelse return error.InvalidProtocolMessage;
        const body_start = header_end_rel + 4;
        const body_end = body_start + content_length;
        if (body_end > output.len) return error.InvalidProtocolMessage;

        const body = output[body_start..body_end];
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, body, .{});
        defer parsed.deinit();

        const method = parsed.value.object.get("method");
        if (method != null and method.? == .string and std.mem.eql(u8, method.?.string, wanted_method)) {
            return try allocator.dupe(u8, body);
        }

        offset = body_end;
    }

    return error.ResponseNotFound;
}

/// Like findMethodNotification, but returns the LAST matching notification —
/// used when a method is published more than once (e.g. diagnostics republished
/// after an edit) and the test cares about the final state.
fn findLastMethodNotification(allocator: Allocator, output: []const u8, wanted_method: []const u8) ![]u8 {
    var offset: usize = 0;
    var last: ?[]u8 = null;
    errdefer if (last) |l| allocator.free(l);
    while (offset < output.len) {
        const header_end_rel = std.mem.indexOfPos(u8, output, offset, "\r\n\r\n") orelse break;
        const header_block = output[offset..header_end_rel];
        const content_length = parseContentLength(header_block) orelse break;
        const body_start = header_end_rel + 4;
        const body_end = body_start + content_length;
        if (body_end > output.len) break;

        const body = output[body_start..body_end];
        const parsed = try std.json.parseFromSlice(std.json.Value, allocator, body, .{});
        defer parsed.deinit();

        const method = parsed.value.object.get("method");
        if (method != null and method.? == .string and std.mem.eql(u8, method.?.string, wanted_method)) {
            if (last) |l| allocator.free(l);
            last = try allocator.dupe(u8, body);
        }

        offset = body_end;
    }

    return last orelse error.ResponseNotFound;
}

fn parseContentLength(headers: []const u8) ?usize {
    var lines = std.mem.splitSequence(u8, headers, "\r\n");
    while (lines.next()) |line| {
        if (std.mem.startsWith(u8, line, "Content-Length: ")) {
            return std.fmt.parseInt(usize, line["Content-Length: ".len..], 10) catch null;
        }
    }
    return null;
}

fn makeDidOpen(allocator: Allocator, uri: []const u8, text: []const u8) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .method = "textDocument/didOpen",
        .params = .{
            .textDocument = .{
                .uri = uri,
                .languageId = "runic",
                .version = 1,
                .text = text,
            },
        },
    });
}

fn makeDidClose(allocator: Allocator, uri: []const u8) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .method = "textDocument/didClose",
        .params = .{
            .textDocument = .{ .uri = uri },
        },
    });
}

fn makeDidChangeWholeDocument(
    allocator: Allocator,
    uri: []const u8,
    version: i64,
    text: []const u8,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .method = "textDocument/didChange",
        .params = .{
            .textDocument = .{
                .uri = uri,
                .version = version,
            },
            .contentChanges = &.{
                .{ .text = text },
            },
        },
    });
}

fn makeInitialize(allocator: Allocator, id: i64, snippet_support: bool) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "initialize",
        .params = .{
            .capabilities = .{
                .textDocument = .{
                    .completion = .{
                        .completionItem = .{
                            .snippetSupport = snippet_support,
                        },
                    },
                },
            },
        },
    });
}

fn makeDidChangeIncremental(
    allocator: Allocator,
    uri: []const u8,
    version: i64,
    start_line: u32,
    start_char: u32,
    end_line: u32,
    end_char: u32,
    text: []const u8,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .method = "textDocument/didChange",
        .params = .{
            .textDocument = .{
                .uri = uri,
                .version = version,
            },
            .contentChanges = &.{
                .{
                    .range = .{
                        .start = .{ .line = start_line, .character = start_char },
                        .end = .{ .line = end_line, .character = end_char },
                    },
                    .text = text,
                },
            },
        },
    });
}

fn makeCompletionRequest(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
    line: u32,
    character: u32,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/completion",
        .params = .{
            .textDocument = .{ .uri = uri },
            .position = .{
                .line = line,
                .character = character,
            },
        },
    });
}

fn makeFormattingRequest(allocator: Allocator, id: i64, uri: []const u8) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/formatting",
        .params = .{
            .textDocument = .{ .uri = uri },
            .options = .{
                .tabSize = 4,
                .insertSpaces = true,
            },
        },
    });
}

fn makeInitializeWithRoot(allocator: Allocator, id: i64, root_uri: []const u8) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "initialize",
        .params = .{ .rootUri = root_uri },
    });
}

fn makeWorkspaceSymbolRequest(allocator: Allocator, id: i64, query: []const u8) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "workspace/symbol",
        .params = .{ .query = query },
    });
}

fn makeInlayHintRequest(allocator: Allocator, id: i64, uri: []const u8, end_line: u32) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/inlayHint",
        .params = .{
            .textDocument = .{ .uri = uri },
            .range = .{
                .start = .{ .line = 0, .character = 0 },
                .end = .{ .line = end_line, .character = 0 },
            },
        },
    });
}

fn makeCompletionResolveRequest(allocator: Allocator, id: i64, label: []const u8, detail: []const u8) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "completionItem/resolve",
        .params = .{
            .label = label,
            .kind = 3,
            .detail = detail,
        },
    });
}

fn makeCodeActionRequest(allocator: Allocator, id: i64, uri: []const u8, line: u32) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/codeAction",
        .params = .{
            .textDocument = .{ .uri = uri },
            .range = .{
                .start = .{ .line = line, .character = 0 },
                .end = .{ .line = line, .character = 0 },
            },
            .context = .{ .diagnostics = .{} },
        },
    });
}

/// A code-action request carrying one diagnostic in `context` (as a client
/// passes back a diagnostic it received) over the given identifier range.
fn makeCodeActionWithDiagnostic(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
    line: u32,
    char_start: u32,
    char_end: u32,
    message: []const u8,
) ![]u8 {
    const range = .{
        .start = .{ .line = line, .character = char_start },
        .end = .{ .line = line, .character = char_end },
    };
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/codeAction",
        .params = .{
            .textDocument = .{ .uri = uri },
            .range = range,
            .context = .{ .diagnostics = .{.{ .range = range, .message = message }} },
        },
    });
}

fn makeFoldingRangeRequest(allocator: Allocator, id: i64, uri: []const u8) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/foldingRange",
        .params = .{ .textDocument = .{ .uri = uri } },
    });
}

fn makeDocumentLinkRequest(allocator: Allocator, id: i64, uri: []const u8) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/documentLink",
        .params = .{
            .textDocument = .{ .uri = uri },
        },
    });
}

fn makeDocumentSymbolRequest(allocator: Allocator, id: i64, uri: []const u8) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/documentSymbol",
        .params = .{
            .textDocument = .{ .uri = uri },
        },
    });
}

fn makePrepareRenameRequest(allocator: Allocator, id: i64, uri: []const u8, line: u32, character: u32) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/prepareRename",
        .params = .{
            .textDocument = .{ .uri = uri },
            .position = .{ .line = line, .character = character },
        },
    });
}

fn makeRenameRequest(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
    line: u32,
    character: u32,
    new_name: []const u8,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/rename",
        .params = .{
            .textDocument = .{ .uri = uri },
            .position = .{
                .line = line,
                .character = character,
            },
            .newName = new_name,
        },
    });
}

fn makeDefinitionRequest(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
    line: u32,
    character: u32,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/definition",
        .params = .{
            .textDocument = .{ .uri = uri },
            .position = .{
                .line = line,
                .character = character,
            },
        },
    });
}

fn makeSignatureHelpRequest(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
    line: u32,
    character: u32,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/signatureHelp",
        .params = .{
            .textDocument = .{ .uri = uri },
            .position = .{ .line = line, .character = character },
        },
    });
}

fn makePrepareCallHierarchyRequest(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
    line: u32,
    character: u32,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/prepareCallHierarchy",
        .params = .{
            .textDocument = .{ .uri = uri },
            .position = .{ .line = line, .character = character },
        },
    });
}

fn makeCallHierarchyItemRequest(
    allocator: Allocator,
    id: i64,
    method: []const u8,
    uri: []const u8,
    name: []const u8,
) ![]u8 {
    const zero = .{ .start = .{ .line = 0, .character = 0 }, .end = .{ .line = 0, .character = 0 } };
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = method,
        .params = .{ .item = .{
            .name = name,
            .kind = 12,
            .uri = uri,
            .range = zero,
            .selectionRange = zero,
        } },
    });
}

fn makeSemanticTokensRequest(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/semanticTokens/full",
        .params = .{
            .textDocument = .{ .uri = uri },
        },
    });
}

fn makeHoverRequest(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
    line: u32,
    character: u32,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/hover",
        .params = .{
            .textDocument = .{ .uri = uri },
            .position = .{
                .line = line,
                .character = character,
            },
        },
    });
}

fn makeDocumentHighlightRequest(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
    line: u32,
    character: u32,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/documentHighlight",
        .params = .{
            .textDocument = .{ .uri = uri },
            .position = .{ .line = line, .character = character },
        },
    });
}

fn makeReferencesRequest(
    allocator: Allocator,
    id: i64,
    uri: []const u8,
    line: u32,
    character: u32,
    include_declaration: bool,
) ![]u8 {
    return toJsonAlloc(allocator, .{
        .jsonrpc = "2.0",
        .id = id,
        .method = "textDocument/references",
        .params = .{
            .textDocument = .{ .uri = uri },
            .position = .{
                .line = line,
                .character = character,
            },
            .context = .{
                .includeDeclaration = include_declaration,
            },
        },
    });
}

fn toJsonAlloc(allocator: Allocator, value: anytype) ![]u8 {
    var alloc_writer = std.Io.Writer.Allocating.init(allocator);
    defer alloc_writer.deinit();
    try alloc_writer.writer.print("{f}", .{std.json.fmt(value, .{})});
    return try alloc_writer.toOwnedSlice();
}
