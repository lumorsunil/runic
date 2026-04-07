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

const TestFixture = struct {
    allocator: Allocator,
    tmp_dir: std.testing.TmpDir,
    root_path: []const u8,

    fn init(allocator: Allocator) !TestFixture {
        var tmp_dir = std.testing.tmpDir(.{});
        const relative_root_path = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", tmp_dir.sub_path[0..] });
        defer allocator.free(relative_root_path);
        const root_path = try std.fs.cwd().realpathAlloc(allocator, relative_root_path);
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
        try self.tmp_dir.dir.writeFile(.{ .sub_path = name, .data = text });
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

    var input_file = try std.fs.createFileAbsolute(input_path, .{ .read = true, .truncate = true });
    defer input_file.close();
    var output_file = try std.fs.createFileAbsolute(output_path, .{ .read = true, .truncate = true });
    defer output_file.close();
    var error_file = try std.fs.createFileAbsolute(error_path, .{ .read = true, .truncate = true });
    defer error_file.close();

    {
        var writer = input_file.writer(&.{});
        for (messages) |message| {
            try writer.interface.print("Content-Length: {d}\r\n\r\n{s}", .{ message.len, message });
        }
        try writer.interface.flush();
    }

    try input_file.seekTo(0);

    var server = try lsp.server.Server.init(allocator, input_file, output_file, error_file);
    defer server.deinit();
    server.initInterface();
    try server.run();

    try output_file.seekTo(0);
    const stdout = try output_file.readToEndAlloc(allocator, 1024 * 1024);
    try error_file.seekTo(0);
    const stderr = try error_file.readToEndAlloc(allocator, 1024 * 1024);
    return .{
        .stdout = stdout,
        .stderr = stderr,
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
