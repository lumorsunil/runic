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
    try std.testing.expect(std.mem.indexOf(u8, new_text, "# keep") != null);
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
    const edits = changes[0].object.get("textDocumentEdit").?.object.get("edits").?.array.items;
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
        const tde = change.object.get("textDocumentEdit").?.object;
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
    const edits = changes[0].object.get("textDocumentEdit").?.object.get("edits").?.array.items;
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

    const value = parsed.value.object.get("result").?.object.get("contents").?.object.get("value").?.string;
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
