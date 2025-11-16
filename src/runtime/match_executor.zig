const std = @import("std");

/// MatchExecutor evaluates `match` expressions against runtime values. It
/// supports matching error variants as well as general literal patterns and
/// surface-level capture clauses (`=> |info| ...`) by reporting which case
/// matched and the payload that should be bound to the capture.
pub const MatchExecutor = struct {
    pub const Error = error{
        NoCases,
        NoMatch,
        PayloadCaptureOnNonError,
        MissingPayload,
    };

    pub const Scalar = union(enum) {
        void,
        bool: bool,
        int: i64,
        float: f64,
        string: []const u8,
    };

    pub const Value = union(enum) {
        scalar: Scalar,
        err: ErrorValue,
    };

    pub const ErrorValue = struct {
        set_name: []const u8,
        variant_name: []const u8,
        payload: ?Scalar = null,
    };

    pub const Pattern = union(enum) {
        wildcard,
        literal: LiteralPattern,
        error_variant: ErrorPattern,
    };

    pub const LiteralPattern = struct {
        value: Scalar,
    };

    pub const ErrorPattern = struct {
        set_name: []const u8,
        variant_name: []const u8,
    };

    pub const CaptureBehavior = enum {
        none,
        subject,
        error_payload,
    };

    pub const Case = struct {
        pattern: Pattern,
        capture: CaptureBehavior = .none,
    };

    pub const CaptureValue = union(enum) {
        scalar: Scalar,
        err: ErrorValue,
    };

    pub const Selection = struct {
        case_index: usize,
        capture: ?CaptureValue = null,
    };

    pub fn select(subject: Value, cases: []const Case) Error!Selection {
        if (cases.len == 0) return Error.NoCases;

        for (cases, 0..) |case, idx| {
            if (!patternMatches(case.pattern, subject)) continue;

            const captured = try captureFor(case.capture, subject);
            return .{
                .case_index = idx,
                .capture = captured,
            };
        }

        return Error.NoMatch;
    }

    fn patternMatches(pattern: Pattern, subject: Value) bool {
        return switch (pattern) {
            .wildcard => true,
            .literal => |lit| matchLiteral(lit, subject),
            .error_variant => |err_pattern| matchErrorVariant(err_pattern, subject),
        };
    }

    fn matchLiteral(pattern: LiteralPattern, subject: Value) bool {
        return switch (subject) {
            .scalar => |scalar| scalarsEqual(scalar, pattern.value),
            else => false,
        };
    }

    fn matchErrorVariant(pattern: ErrorPattern, subject: Value) bool {
        return switch (subject) {
            .err => |err| std.mem.eql(u8, err.set_name, pattern.set_name) and
                std.mem.eql(u8, err.variant_name, pattern.variant_name),
            else => false,
        };
    }

    fn scalarsEqual(a: Scalar, b: Scalar) bool {
        const tag_a = std.meta.activeTag(a);
        const tag_b = std.meta.activeTag(b);
        if (tag_a != tag_b) return false;
        return switch (tag_a) {
            .void => true,
            .bool => a.bool == b.bool,
            .int => a.int == b.int,
            .float => a.float == b.float,
            .string => std.mem.eql(u8, a.string, b.string),
        };
    }

    fn captureFor(behavior: CaptureBehavior, subject: Value) Error!?CaptureValue {
        return switch (behavior) {
            .none => null,
            .subject => captureSubject(subject),
            .error_payload => capturePayload(subject),
        };
    }

    fn captureSubject(subject: Value) ?CaptureValue {
        return switch (subject) {
            .scalar => |scalar| .{ .scalar = scalar },
            .err => |err| .{ .err = err },
        };
    }

    fn capturePayload(subject: Value) Error!?CaptureValue {
        return switch (subject) {
            .scalar => Error.PayloadCaptureOnNonError,
            .err => |err| {
                const payload = err.payload orelse return Error.MissingPayload;
                return CaptureValue{ .scalar = payload };
            },
        };
    }
};

test "select matches literal patterns before wildcard" {
    const executor = MatchExecutor;
    const subject = executor.Value{ .scalar = .{ .bool = true } };
    const cases = [_]executor.Case{
        .{ .pattern = .{ .literal = .{ .value = .{ .bool = false } } } },
        .{ .pattern = .{ .literal = .{ .value = .{ .bool = true } } }, .capture = .subject },
        .{ .pattern = .wildcard },
    };

    const result = try executor.select(subject, &cases);
    try std.testing.expectEqual(@as(usize, 1), result.case_index);
    try std.testing.expect(result.capture != null);
    const capture = result.capture.?;
    try std.testing.expect(std.meta.activeTag(capture) == .scalar);
    try std.testing.expect(std.meta.activeTag(capture.scalar) == .bool);
    try std.testing.expectEqual(true, capture.scalar.bool);
}

test "select falls back to wildcard when literals miss" {
    const executor = MatchExecutor;
    const subject = executor.Value{ .scalar = .{ .int = 5 } };
    const cases = [_]executor.Case{
        .{ .pattern = .{ .literal = .{ .value = .{ .int = 3 } } } },
        .{ .pattern = .wildcard },
    };

    const result = try executor.select(subject, &cases);
    try std.testing.expectEqual(@as(usize, 1), result.case_index);
    try std.testing.expect(result.capture == null);
}

test "select captures payloads from error variants" {
    const executor = MatchExecutor;
    const subject = executor.Value{
        .err = .{
            .set_name = "FileError",
            .variant_name = "NotFound",
            .payload = executor.Scalar{ .string = "/tmp/config" },
        },
    };

    const cases = [_]executor.Case{
        .{
            .pattern = .{ .error_variant = .{ .set_name = "FileError", .variant_name = "PermissionDenied" } },
        },
        .{
            .pattern = .{ .error_variant = .{ .set_name = "FileError", .variant_name = "NotFound" } },
            .capture = .error_payload,
        },
    };

    const result = try executor.select(subject, &cases);
    try std.testing.expectEqual(@as(usize, 1), result.case_index);
    const capture = result.capture orelse unreachable;
    try std.testing.expect(std.meta.activeTag(capture) == .scalar);
    try std.testing.expect(std.meta.activeTag(capture.scalar) == .string);
    try std.testing.expectEqualStrings("/tmp/config", capture.scalar.string);
}

test "select rejects invalid payload captures" {
    const executor = MatchExecutor;
    const literal_subject = executor.Value{ .scalar = .{ .int = 42 } };
    const literal_cases = [_]executor.Case{
        .{ .pattern = .wildcard, .capture = .error_payload },
    };
    try std.testing.expectError(executor.Error.PayloadCaptureOnNonError, executor.select(literal_subject, &literal_cases));

    const no_payload_subject = executor.Value{
        .err = .{
            .set_name = "FileError",
            .variant_name = "PermissionDenied",
            .payload = null,
        },
    };
    const payload_cases = [_]executor.Case{
        .{
            .pattern = .{ .error_variant = .{ .set_name = "FileError", .variant_name = "PermissionDenied" } },
            .capture = .error_payload,
        },
    };
    try std.testing.expectError(executor.Error.MissingPayload, executor.select(no_payload_subject, &payload_cases));
}
