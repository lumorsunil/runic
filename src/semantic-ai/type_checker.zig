const std = @import("std");
const token = @import("../frontend/token.zig");
const types = @import("types.zig");

pub const TypeError = error{
    AnnotationMismatch,
    OptionalRequired,
    PromiseRequired,
    ErrorUnionRequired,
    CatchResultMismatch,
    RestrictedErrorSetMismatch,
    MatchSubjectInvalid,
    MatchVariantMissing,
    MatchLiteralMismatch,
    MatchCaptureIncompatible,
    MatchResultConflict,
    ControlFlowConflict,
    EmptyMatch,
    EmptyControlFlow,
};

pub const TypeChecker = struct {
    store: *types.TypeStore,
    last_violation: ?Violation = null,

    pub fn init(store: *types.TypeStore) TypeChecker {
        return .{ .store = store, .last_violation = null };
    }

    pub fn reset(self: *TypeChecker) void {
        self.last_violation = null;
    }

    pub fn violation(self: *TypeChecker) ?Violation {
        return self.last_violation;
    }

    pub fn inferBinding(
        self: *TypeChecker,
        annotation: ?types.TypeRef,
        initializer: types.TypeRef,
        span: token.Span,
    ) TypeError!types.TypeRef {
        self.reset();
        if (annotation) |annotated| {
            if (!self.isAssignable(annotated, initializer)) {
                if (self.detectMissingTryCatch(annotated, initializer, span)) |hint| {
                    self.last_violation = .{ .missing_try_or_catch = hint };
                } else {
                    self.last_violation = .{ .annotation_mismatch = .{
                        .expected = annotated,
                        .actual = initializer,
                        .span = span,
                    } };
                }
                return TypeError.AnnotationMismatch;
            }
            return annotated;
        }
        return initializer;
    }

    pub fn captureTypeForOptional(
        self: *TypeChecker,
        subject: types.TypeRef,
        span: token.Span,
    ) TypeError!types.TypeRef {
        self.reset();
        if (optionalChild(subject)) |child| {
            return child;
        }
        self.last_violation = .{ .optional_required = .{
            .subject = subject,
            .span = span,
        } };
        return TypeError.OptionalRequired;
    }

    pub fn captureTypeForPromise(
        self: *TypeChecker,
        subject: types.TypeRef,
        span: token.Span,
    ) (TypeError || std.mem.Allocator.Error)!types.TypeRef {
        self.reset();
        switch (subject.*) {
            .promise => |promise| return promise.payload,
            .process_handle => |handle| {
                if (handle.mode == .promise_backed) {
                    return try self.store.processHandle(.{
                        .mode = .immediate,
                        .stdout = handle.stdout,
                        .stderr = handle.stderr,
                        .status = handle.status,
                    });
                }
            },
            else => {},
        }

        self.last_violation = .{ .promise_required = .{
            .subject = subject,
            .span = span,
        } };
        return TypeError.PromiseRequired;
    }

    pub fn analyzeTry(self: *TypeChecker, subject: types.TypeRef, span: token.Span) TypeError!TryInfo {
        self.reset();
        return requireErrorUnion(self, subject, span);
    }

    pub fn analyzeCatch(
        self: *TypeChecker,
        subject: types.TypeRef,
        subject_span: token.Span,
        handler_type: types.TypeRef,
        handler_span: token.Span,
    ) TypeError!CatchInfo {
        self.reset();
        const info = try requireErrorUnion(self, subject, subject_span);

        if (!self.isAssignable(info.payload, handler_type)) {
            self.last_violation = .{ .catch_result_mismatch = .{
                .expected = info.payload,
                .actual = handler_type,
                .span = handler_span,
            } };
            return TypeError.CatchResultMismatch;
        }

        return .{
            .error_set = info.error_set,
            .result_type = info.payload,
        };
    }

    pub fn enforceRestrictedErrorSet(
        self: *TypeChecker,
        allowed: types.TypeRef,
        actual: types.TypeRef,
        span: token.Span,
    ) TypeError!void {
        self.reset();
        if (errorSetAllows(allowed, actual)) return;

        self.last_violation = .{ .restricted_error_set_mismatch = .{
            .allowed = allowed,
            .actual = actual,
            .span = span,
        } };
        return TypeError.RestrictedErrorSetMismatch;
    }

    pub fn analyzeMatch(
        self: *TypeChecker,
        subject: types.TypeRef,
        cases: []const MatchCaseDesc,
    ) TypeError!types.TypeRef {
        self.reset();
        if (cases.len == 0) return TypeError.EmptyMatch;

        var inferred: ?types.TypeRef = null;
        for (cases) |case_desc| {
            try self.validatePattern(subject, case_desc.pattern);

            if (case_desc.capture) |capture_info| {
                try self.validateCapture(subject, case_desc.pattern, capture_info);
            }

            inferred = try self.unifyResult(inferred, case_desc.body_type, case_desc.body_span);
        }

        return inferred.?;
    }

    pub fn analyzeFunctionReturns(self: *TypeChecker, desc: FunctionReturnDesc) TypeError!types.TypeRef {
        self.reset();
        if (desc.paths.len == 0) return self.reportEmptyFlow(.function_return, desc.span);

        if (desc.annotation) |annotated| {
            for (desc.paths) |path| {
                if (self.isAssignable(annotated, path.result_type)) continue;

                if (self.detectMissingTryCatch(annotated, path.result_type, path.span)) |hint| {
                    self.last_violation = .{ .missing_try_or_catch = hint };
                } else {
                    self.last_violation = .{ .annotation_mismatch = .{
                        .expected = annotated,
                        .actual = path.result_type,
                        .span = path.span,
                    } };
                }
                return TypeError.AnnotationMismatch;
            }
            return annotated;
        }

        return self.inferControlFlow(.function_return, desc.paths, desc.span);
    }

    pub fn analyzeConditionalBranches(
        self: *TypeChecker,
        branches: []const FlowResultDesc,
        span: token.Span,
    ) TypeError!types.TypeRef {
        self.reset();
        return self.inferControlFlow(.conditional, branches, span);
    }

    pub fn analyzeOptionalIf(self: *TypeChecker, desc: OptionalIfDesc) TypeError!OptionalIfInfo {
        self.reset();
        const payload = try self.captureTypeForOptional(desc.subject_type, desc.subject_span);
        const branches = [_]FlowResultDesc{
            desc.then_result,
            desc.else_result,
        };
        const result_type = try self.analyzeConditionalBranches(&branches, desc.span);
        return .{
            .payload_type = payload,
            .result_type = result_type,
        };
    }

    pub fn analyzeAsyncBlock(self: *TypeChecker, desc: AsyncBlockDesc) std.mem.Allocator.Error!types.TypeRef {
        self.reset();
        return self.store.promise(desc.body_result);
    }

    pub fn analyzeAwait(self: *TypeChecker, desc: AwaitDesc) (TypeError || std.mem.Allocator.Error)!AwaitInfo {
        self.reset();
        const awaited_type = try self.captureTypeForPromise(desc.subject_type, desc.subject_span);
        const payload_type = promiseSuccessPayload(awaited_type);

        var result_type = if (desc.success_result) |success| success.result_type else payload_type;
        var error_set: ?types.TypeRef = null;

        if (desc.success_result) |success| {
            if (desc.catch_result) |catch_desc| {
                const try_info = try self.analyzeTry(awaited_type, desc.subject_span);
                error_set = try_info.error_set;
                const branches = [_]FlowResultDesc{
                    success,
                    catch_desc,
                };
                result_type = try self.analyzeConditionalBranches(&branches, desc.span);
            }
        } else if (desc.catch_result) |catch_desc| {
            const catch_info = try self.analyzeCatch(awaited_type, desc.subject_span, catch_desc.result_type, catch_desc.span);
            error_set = catch_info.error_set;
            result_type = catch_info.result_type;
        }

        return .{
            .payload_type = payload_type,
            .result_type = result_type,
            .error_set = error_set,
        };
    }

    pub fn analyzeLoopResults(
        self: *TypeChecker,
        results: []const FlowResultDesc,
        span: token.Span,
    ) TypeError!types.TypeRef {
        self.reset();
        return self.inferControlFlow(.loop, results, span);
    }

    fn inferControlFlow(
        self: *TypeChecker,
        context: ControlFlowContext,
        paths: []const FlowResultDesc,
        span: token.Span,
    ) TypeError!types.TypeRef {
        if (paths.len == 0) return self.reportEmptyFlow(context, span);

        var inferred: ?types.TypeRef = null;
        for (paths) |path| {
            inferred = try self.unifyControlFlow(context, inferred, path.result_type, path.span);
        }

        return inferred.?;
    }

    fn unifyControlFlow(
        self: *TypeChecker,
        context: ControlFlowContext,
        current: ?types.TypeRef,
        candidate: types.TypeRef,
        span: token.Span,
    ) TypeError!types.TypeRef {
        if (current) |existing| {
            if (self.isAssignable(existing, candidate)) return existing;
            if (self.isAssignable(candidate, existing)) return candidate;

            self.last_violation = .{ .control_flow_conflict = .{
                .context = context,
                .established = existing,
                .conflicting = candidate,
                .span = span,
            } };
            return TypeError.ControlFlowConflict;
        }

        return candidate;
    }

    fn reportEmptyFlow(self: *TypeChecker, context: ControlFlowContext, span: token.Span) TypeError {
        self.last_violation = .{ .empty_control_flow = .{
            .context = context,
            .span = span,
        } };
        return TypeError.EmptyControlFlow;
    }

    fn unifyResult(
        self: *TypeChecker,
        current: ?types.TypeRef,
        candidate: types.TypeRef,
        span: token.Span,
    ) TypeError!types.TypeRef {
        if (current) |existing| {
            if (self.isAssignable(existing, candidate)) return existing;
            if (self.isAssignable(candidate, existing)) return candidate;

            self.last_violation = .{ .match_result_conflict = .{
                .established = existing,
                .conflicting = candidate,
                .span = span,
            } };
            return TypeError.MatchResultConflict;
        }

        return candidate;
    }

    fn validatePattern(
        self: *TypeChecker,
        subject: types.TypeRef,
        pattern: MatchPatternDesc,
    ) TypeError!void {
        switch (pattern) {
            .wildcard => {},
            .type_match => |type_pattern| {
                if (self.isAssignable(subject, type_pattern.ty) or self.isAssignable(type_pattern.ty, subject)) {
                    return;
                }

                self.last_violation = .{ .match_literal_mismatch = .{
                    .subject = subject,
                    .pattern = type_pattern.ty,
                    .span = type_pattern.span,
                } };
                return TypeError.MatchLiteralMismatch;
            },
            .variant => |variant_pattern| {
                _ = try self.lookupVariant(subject, variant_pattern);
            },
        }
    }

    fn validateCapture(
        self: *TypeChecker,
        subject: types.TypeRef,
        pattern: MatchPatternDesc,
        capture: CaptureDesc,
    ) TypeError!void {
        switch (pattern) {
            .wildcard, .type_match => {
                if (self.isAssignable(subject, capture.ty)) return;

                self.last_violation = .{ .match_capture_incompatible = .{
                    .expected = subject,
                    .actual = capture.ty,
                    .span = capture.span,
                } };
                return TypeError.MatchCaptureIncompatible;
            },
            .variant => |variant_pattern| {
                const lookup = try self.lookupVariant(subject, variant_pattern);
                const payload = lookup.variant.payload orelse {
                    self.last_violation = .{ .match_capture_incompatible = .{
                        .expected = null,
                        .actual = capture.ty,
                        .span = capture.span,
                    } };
                    return TypeError.MatchCaptureIncompatible;
                };

                if (self.isAssignable(payload, capture.ty)) return;

                self.last_violation = .{ .match_capture_incompatible = .{
                    .expected = payload,
                    .actual = capture.ty,
                    .span = capture.span,
                } };
                return TypeError.MatchCaptureIncompatible;
            },
        }
    }

    fn lookupVariant(
        self: *TypeChecker,
        subject: types.TypeRef,
        variant_pattern: MatchPatternDesc.VariantPattern,
    ) TypeError!VariantLookup {
        const error_set = switch (subject.*) {
            .error_set => subject,
            else => {
                self.last_violation = .{ .match_subject_invalid = .{
                    .subject = subject,
                    .span = variant_pattern.span,
                } };
                return TypeError.MatchSubjectInvalid;
            },
        };

        const set_info = error_set.*;
        switch (set_info) {
            .error_set => |set_data| {
                for (set_data.variants) |*variant| {
                    if (std.mem.eql(u8, variant.name, variant_pattern.name)) {
                        return .{ .set = error_set, .variant = variant };
                    }
                }

                self.last_violation = .{ .match_variant_missing = .{
                    .variant_name = variant_pattern.name,
                    .span = variant_pattern.span,
                } };
                return TypeError.MatchVariantMissing;
            },
            else => unreachable,
        }
    }

    fn isAssignable(self: *TypeChecker, target: types.TypeRef, value: types.TypeRef) bool {
        if (typesEqual(target, value)) return true;

        if (optionalChild(target)) |child| {
            if (typesEqual(child, value)) return true;
        }

        if (value.* == .error_set and target.* == .error_set) {
            return errorSetAllows(target, value);
        }

        switch (target.*) {
            .error_union => |target_union| {
                if (value.* == .error_union) {
                    const value_union = value.*.error_union;
                    return self.isAssignable(target_union.set, value_union.set) and self.isAssignable(target_union.payload, value_union.payload);
                }
            },
            else => {},
        }

        return false;
    }

    fn detectMissingTryCatch(
        self: *TypeChecker,
        expected: types.TypeRef,
        actual: types.TypeRef,
        span: token.Span,
    ) ?Violation.MissingTryOrCatch {
        return switch (actual.*) {
            .error_union => |info| {
                if (self.isAssignable(expected, info.payload)) {
                    return .{
                        .expected = expected,
                        .actual = actual,
                        .payload = info.payload,
                        .span = span,
                    };
                }
                return null;
            },
            else => null,
        };
    }
};

fn requireErrorUnion(self: *TypeChecker, subject: types.TypeRef, span: token.Span) TypeError!TryInfo {
    return switch (subject.*) {
        .error_union => |union_info| .{
            .error_set = union_info.set,
            .payload = union_info.payload,
        },
        else => {
            self.last_violation = .{ .error_union_required = .{
                .subject = subject,
                .span = span,
            } };
            return TypeError.ErrorUnionRequired;
        },
    };
}

pub const MatchCaseDesc = struct {
    pattern: MatchPatternDesc,
    capture: ?CaptureDesc = null,
    body_type: types.TypeRef,
    body_span: token.Span,
};

pub const CaptureDesc = struct {
    ty: types.TypeRef,
    span: token.Span,
};

pub const MatchPatternDesc = union(enum) {
    wildcard: token.Span,
    type_match: TypePattern,
    variant: VariantPattern,

    pub const TypePattern = struct {
        ty: types.TypeRef,
        span: token.Span,
    };

    pub const VariantPattern = struct {
        name: []const u8,
        span: token.Span,
    };
};

pub const FlowResultDesc = struct {
    result_type: types.TypeRef,
    span: token.Span,
};

pub const OptionalIfDesc = struct {
    subject_type: types.TypeRef,
    subject_span: token.Span,
    then_result: FlowResultDesc,
    else_result: FlowResultDesc,
    span: token.Span,
};

pub const OptionalIfInfo = struct {
    payload_type: types.TypeRef,
    result_type: types.TypeRef,
};

pub const AsyncBlockDesc = struct {
    body_result: types.TypeRef,
    span: token.Span,
};

pub const AwaitDesc = struct {
    subject_type: types.TypeRef,
    subject_span: token.Span,
    success_result: ?FlowResultDesc = null,
    catch_result: ?FlowResultDesc = null,
    span: token.Span,
};

pub const AwaitInfo = struct {
    payload_type: types.TypeRef,
    result_type: types.TypeRef,
    error_set: ?types.TypeRef = null,
};

pub const FunctionReturnDesc = struct {
    annotation: ?types.TypeRef = null,
    paths: []const FlowResultDesc,
    span: token.Span,
};

pub const ControlFlowContext = enum {
    function_return,
    conditional,
    loop,
};

pub const Violation = union(enum) {
    annotation_mismatch: AnnotationMismatch,
    optional_required: CaptureContextViolation,
    promise_required: CaptureContextViolation,
    error_union_required: ErrorUnionViolation,
    missing_try_or_catch: MissingTryOrCatch,
    match_subject_invalid: MatchSubjectViolation,
    match_variant_missing: MatchVariantMissing,
    match_literal_mismatch: MatchLiteralMismatch,
    match_capture_incompatible: MatchCaptureViolation,
    match_result_conflict: MatchResultConflict,
    catch_result_mismatch: CatchResultMismatch,
    restricted_error_set_mismatch: RestrictedErrorSetMismatch,
    control_flow_conflict: ControlFlowConflict,
    empty_control_flow: EmptyControlFlow,

    pub const AnnotationMismatch = struct {
        expected: types.TypeRef,
        actual: types.TypeRef,
        span: token.Span,
    };

    pub const MissingTryOrCatch = struct {
        expected: types.TypeRef,
        actual: types.TypeRef,
        payload: types.TypeRef,
        span: token.Span,
    };

    pub const CaptureContextViolation = struct {
        subject: types.TypeRef,
        span: token.Span,
    };

    pub const ErrorUnionViolation = struct {
        subject: types.TypeRef,
        span: token.Span,
    };

    pub const MatchSubjectViolation = struct {
        subject: types.TypeRef,
        span: token.Span,
    };

    pub const MatchVariantMissing = struct {
        variant_name: []const u8,
        span: token.Span,
    };

    pub const MatchLiteralMismatch = struct {
        subject: types.TypeRef,
        pattern: types.TypeRef,
        span: token.Span,
    };

    pub const MatchCaptureViolation = struct {
        expected: ?types.TypeRef,
        actual: types.TypeRef,
        span: token.Span,
    };

    pub const MatchResultConflict = struct {
        established: types.TypeRef,
        conflicting: types.TypeRef,
        span: token.Span,
    };

    pub const CatchResultMismatch = struct {
        expected: types.TypeRef,
        actual: types.TypeRef,
        span: token.Span,
    };

    pub const RestrictedErrorSetMismatch = struct {
        allowed: types.TypeRef,
        actual: types.TypeRef,
        span: token.Span,
    };

    pub const ControlFlowConflict = struct {
        context: ControlFlowContext,
        established: types.TypeRef,
        conflicting: types.TypeRef,
        span: token.Span,
    };

    pub const EmptyControlFlow = struct {
        context: ControlFlowContext,
        span: token.Span,
    };
};

const VariantLookup = struct {
    set: types.TypeRef,
    variant: *const types.Type.ErrorSet.Variant,
};

pub const TryInfo = struct {
    error_set: types.TypeRef,
    payload: types.TypeRef,
};

pub const CatchInfo = struct {
    error_set: types.TypeRef,
    result_type: types.TypeRef,
};

fn optionalChild(ty: types.TypeRef) ?types.TypeRef {
    return switch (ty.*) {
        .optional => |opt| opt.child,
        else => null,
    };
}

fn promiseSuccessPayload(ty: types.TypeRef) types.TypeRef {
    return switch (ty.*) {
        .error_union => |union_info| union_info.payload,
        else => ty,
    };
}

fn isAnyErrorSet(ty: types.TypeRef) bool {
    return switch (ty.*) {
        .error_set => |set| set.behavior == types.Type.ErrorSet.Behavior.any,
        else => false,
    };
}

fn errorSetsEqual(a: types.TypeRef, b: types.TypeRef) bool {
    if (a == b) return true;
    const set_a = switch (a.*) {
        .error_set => |info| info,
        else => return false,
    };
    const set_b = switch (b.*) {
        .error_set => |info| info,
        else => return false,
    };

    if (set_a.behavior != set_b.behavior) return false;
    const a_name = set_a.name orelse "";
    const b_name = set_b.name orelse "";
    if (!std.mem.eql(u8, a_name, b_name)) return false;
    if (set_a.variants.len != set_b.variants.len) return false;

    for (set_a.variants, 0..) |variant_a, idx| {
        const variant_b = set_b.variants[idx];
        if (!std.mem.eql(u8, variant_a.name, variant_b.name)) return false;

        switch (variant_a.payload) {
            null => if (variant_b.payload != null) return false,
            else => |payload| {
                if (variant_b.payload) |payload_b| {
                    if (!typesEqual(payload, payload_b)) return false;
                } else return false;
            },
        }
    }

    return true;
}

fn errorSetAllows(target: types.TypeRef, candidate: types.TypeRef) bool {
    if (target == candidate) return true;
    const target_set = switch (target.*) {
        .error_set => |info| info,
        else => return false,
    };
    const candidate_set = switch (candidate.*) {
        .error_set => |info| info,
        else => return false,
    };

    if (isAnyErrorSet(target)) return true;
    if (isAnyErrorSet(candidate)) return false;

    for (candidate_set.variants) |variant| {
        const allowed_variant = findVariant(target_set.variants, variant.name) orelse return false;
        if (!variantPayloadsEqual(allowed_variant.payload, variant.payload)) return false;
    }

    return true;
}

fn findVariant(haystack: []const types.Type.ErrorSet.Variant, name: []const u8) ?types.Type.ErrorSet.Variant {
    for (haystack) |variant| {
        if (std.mem.eql(u8, variant.name, name)) {
            return variant;
        }
    }
    return null;
}

fn variantPayloadsEqual(expected: ?types.TypeRef, actual: ?types.TypeRef) bool {
    return switch (expected) {
        null => actual == null,
        else => |expected_payload| switch (actual) {
            null => false,
            else => |actual_payload| typesEqual(expected_payload, actual_payload),
        },
    };
}

fn slicesEqual(a: []const types.TypeRef, b: []const types.TypeRef) bool {
    if (a.len != b.len) return false;
    for (a, 0..) |item, idx| {
        if (!typesEqual(item, b[idx])) return false;
    }
    return true;
}

fn structFieldsEqual(a: []const types.Type.StructType.Field, b: []const types.Type.StructType.Field) bool {
    if (a.len != b.len) return false;
    for (a, 0..) |field_a, idx| {
        const field_b = b[idx];
        if (!std.mem.eql(u8, field_a.name, field_b.name)) return false;
        if (!typesEqual(field_a.ty, field_b.ty)) return false;
    }
    return true;
}

fn typesEqual(a: types.TypeRef, b: types.TypeRef) bool {
    if (a == b) return true;
    const lhs = a.*;
    const rhs = b.*;

    if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) {
        return false;
    }

    return switch (lhs) {
        .primitive => |left| switch (rhs) {
            .primitive => |right| left == right,
            else => false,
        },
        .array => |left| switch (rhs) {
            .array => |right| typesEqual(left.element, right.element),
            else => false,
        },
        .map => |left| switch (rhs) {
            .map => |right| typesEqual(left.key, right.key) and typesEqual(left.value, right.value),
            else => false,
        },
        .optional => |left| switch (rhs) {
            .optional => |right| typesEqual(left.child, right.child),
            else => false,
        },
        .promise => |left| switch (rhs) {
            .promise => |right| typesEqual(left.payload, right.payload),
            else => false,
        },
        .function => |left| switch (rhs) {
            .function => |right| left.is_async == right.is_async and slicesEqual(left.params, right.params) and
                typesEqual(left.return_type, right.return_type),
            else => false,
        },
        .error_set => |_| errorSetsEqual(a, b),
        .error_union => |left| switch (rhs) {
            .error_union => |right| typesEqual(left.set, right.set) and typesEqual(left.payload, right.payload),
            else => false,
        },
        .structure => |left| switch (rhs) {
            .structure => |right| structFieldsEqual(left.fields, right.fields),
            else => false,
        },
        .process_handle => |left| switch (rhs) {
            .process_handle => |right| left.mode == right.mode and
                left.stdout.delivery == right.stdout.delivery and
                left.stderr.delivery == right.stderr.delivery and
                typesEqual(left.stdout.payload, right.stdout.payload) and
                typesEqual(left.stderr.payload, right.stderr.payload) and
                typesEqual(left.status, right.status),
            else => false,
        },
    };
}

fn fakeSpan(offset: usize) token.Span {
    return .{
        .start = .{ .line = 1, .column = offset + 1, .offset = offset },
        .end = .{ .line = 1, .column = offset + 2, .offset = offset + 1 },
    };
}

test "inferBinding enforces annotations and optional conversions" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const int = try store.primitive(.int);
    const optional_str = try store.optional(str);

    var checker = TypeChecker.init(&store);

    const inferred = try checker.inferBinding(null, str, fakeSpan(0));
    try std.testing.expectEqual(str, inferred);

    const annotated = try checker.inferBinding(optional_str, str, fakeSpan(2));
    try std.testing.expectEqual(optional_str, annotated);

    try std.testing.expectError(TypeError.AnnotationMismatch, checker.inferBinding(int, str, fakeSpan(4)));
    const violation = checker.violation().?;
    switch (violation) {
        .annotation_mismatch => |info| {
            try std.testing.expectEqual(int, info.expected);
            try std.testing.expectEqual(str, info.actual);
            try std.testing.expectEqual(@as(usize, 4), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "inferBinding suggests try/catch when assigning error unions to plain bindings" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const file_errors = try store.errorSet(.{
        .name = "FileError",
        .variants = &.{},
    });
    const union_str = try store.errorUnion(file_errors, str);

    var checker = TypeChecker.init(&store);
    try std.testing.expectError(
        TypeError.AnnotationMismatch,
        checker.inferBinding(str, union_str, fakeSpan(6)),
    );

    switch (checker.violation().?) {
        .missing_try_or_catch => |hint| {
            try std.testing.expectEqual(str, hint.expected);
            try std.testing.expectEqual(union_str, hint.actual);
            try std.testing.expectEqual(str, hint.payload);
            try std.testing.expectEqual(@as(usize, 6), hint.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "struct annotations compare shape and ordering" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const bool_ty = try store.primitive(.bool);

    const result_alias = try store.structure(.{
        .fields = &.{
            .{ .name = "value", .ty = str },
            .{ .name = "ok", .ty = bool_ty },
        },
    });

    const result_copy = try store.structure(.{
        .fields = &.{
            .{ .name = "value", .ty = str },
            .{ .name = "ok", .ty = bool_ty },
        },
    });

    const reordered = try store.structure(.{
        .fields = &.{
            .{ .name = "ok", .ty = bool_ty },
            .{ .name = "value", .ty = str },
        },
    });

    var checker = TypeChecker.init(&store);
    const inferred = try checker.inferBinding(result_alias, result_copy, fakeSpan(10));
    try std.testing.expectEqual(result_alias, inferred);

    try std.testing.expectError(
        TypeError.AnnotationMismatch,
        checker.inferBinding(result_alias, reordered, fakeSpan(12)),
    );
    switch (checker.violation().?) {
        .annotation_mismatch => |info| {
            try std.testing.expectEqual(result_alias, info.expected);
            try std.testing.expectEqual(reordered, info.actual);
        },
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "captureTypeForOptional and captureTypeForPromise validate wrappers" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const int = try store.primitive(.int);
    const opt_int = try store.optional(int);
    const promise_str = try store.promise(str);
    const bytes = try store.primitive(.bytes);
    const status = try store.primitive(.exit_status);
    const promise_handle = try store.processHandle(.{
        .mode = .promise_backed,
        .stdout = .{ .payload = bytes, .delivery = .buffered },
        .stderr = .{ .payload = bytes, .delivery = .streaming },
        .status = status,
    });

    var checker = TypeChecker.init(&store);

    const unwrapped = try checker.captureTypeForOptional(opt_int, fakeSpan(0));
    try std.testing.expectEqual(int, unwrapped);

    try std.testing.expectError(TypeError.OptionalRequired, checker.captureTypeForOptional(str, fakeSpan(3)));
    switch (checker.violation().?) {
        .optional_required => |info| try std.testing.expectEqual(str, info.subject),
        else => return TestViolationError.UnexpectedViolation,
    }

    const payload = try checker.captureTypeForPromise(promise_str, fakeSpan(5));
    try std.testing.expectEqual(str, payload);

    try std.testing.expectError(TypeError.PromiseRequired, checker.captureTypeForPromise(opt_int, fakeSpan(7)));
    switch (checker.violation().?) {
        .promise_required => |info| try std.testing.expectEqual(opt_int, info.subject),
        else => return TestViolationError.UnexpectedViolation,
    }

    const awaited_handle = try checker.captureTypeForPromise(promise_handle, fakeSpan(9));
    switch (awaited_handle.*) {
        .process_handle => |handle| {
            try std.testing.expectEqual(types.Type.ProcessHandleType.Mode.immediate, handle.mode);
            try std.testing.expectEqual(bytes, handle.stdout.payload);
            try std.testing.expectEqual(bytes, handle.stderr.payload);
            try std.testing.expectEqual(types.Type.ProcessHandleType.StreamDelivery.buffered, handle.stdout.delivery);
            try std.testing.expectEqual(types.Type.ProcessHandleType.StreamDelivery.streaming, handle.stderr.delivery);
            try std.testing.expectEqual(status, handle.status);
        },
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "analyzeAsyncBlock wraps results in promises" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);

    var checker = TypeChecker.init(&store);

    const promise_ty = try checker.analyzeAsyncBlock(.{
        .body_result = str,
        .span = fakeSpan(50),
    });

    switch (promise_ty.*) {
        .promise => |promise_info| try std.testing.expectEqual(str, promise_info.payload),
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "analyzeTry and analyzeCatch enforce error handling" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const int = try store.primitive(.int);
    const file_err = try store.errorSet(.{
        .name = "FileError",
        .variants = &.{.{ .name = "NotFound", .payload = null }},
    });
    const err_union = try store.errorUnion(file_err, str);

    var checker = TypeChecker.init(&store);

    const try_info = try checker.analyzeTry(err_union, fakeSpan(0));
    try std.testing.expectEqual(file_err, try_info.error_set);
    try std.testing.expectEqual(str, try_info.payload);

    try std.testing.expectError(TypeError.ErrorUnionRequired, checker.analyzeTry(str, fakeSpan(2)));
    switch (checker.violation().?) {
        .error_union_required => |info| {
            try std.testing.expectEqual(str, info.subject);
            try std.testing.expectEqual(@as(usize, 2), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    const catch_info = try checker.analyzeCatch(err_union, fakeSpan(4), str, fakeSpan(5));
    try std.testing.expectEqual(file_err, catch_info.error_set);
    try std.testing.expectEqual(str, catch_info.result_type);

    try std.testing.expectError(TypeError.CatchResultMismatch, checker.analyzeCatch(err_union, fakeSpan(6), int, fakeSpan(7)));
    switch (checker.violation().?) {
        .catch_result_mismatch => |info| {
            try std.testing.expectEqual(str, info.expected);
            try std.testing.expectEqual(int, info.actual);
            try std.testing.expectEqual(@as(usize, 7), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    try std.testing.expectError(TypeError.ErrorUnionRequired, checker.analyzeCatch(str, fakeSpan(8), str, fakeSpan(9)));
    switch (checker.violation().?) {
        .error_union_required => |info| try std.testing.expectEqual(str, info.subject),
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "analyzeMatch enforces variant existence, captures, and body inference" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const void_ty = try store.primitive(.void);
    const file_error = try store.errorSet(.{
        .name = "FileError",
        .variants = &.{
            .{ .name = "NotFound", .payload = str },
            .{ .name = "PermissionDenied", .payload = null },
        },
    });

    var checker = TypeChecker.init(&store);
    const cases = [_]MatchCaseDesc{
        .{
            .pattern = .{ .variant = .{ .name = "NotFound", .span = fakeSpan(0) } },
            .capture = .{ .ty = str, .span = fakeSpan(1) },
            .body_type = str,
            .body_span = fakeSpan(2),
        },
        .{
            .pattern = .{ .wildcard = fakeSpan(3) },
            .capture = null,
            .body_type = str,
            .body_span = fakeSpan(4),
        },
    };

    const match_type = try checker.analyzeMatch(file_error, &cases);
    try std.testing.expectEqual(str, match_type);

    const conflict_cases = [_]MatchCaseDesc{
        .{
            .pattern = .{ .wildcard = fakeSpan(5) },
            .capture = null,
            .body_type = str,
            .body_span = fakeSpan(6),
        },
        .{
            .pattern = .{ .wildcard = fakeSpan(7) },
            .capture = null,
            .body_type = void_ty,
            .body_span = fakeSpan(8),
        },
    };

    try std.testing.expectError(TypeError.MatchResultConflict, checker.analyzeMatch(file_error, &conflict_cases));
    switch (checker.violation().?) {
        .match_result_conflict => |info| {
            try std.testing.expectEqual(str, info.established);
            try std.testing.expectEqual(void_ty, info.conflicting);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    const capture_error = [_]MatchCaseDesc{
        .{
            .pattern = .{ .variant = .{ .name = "PermissionDenied", .span = fakeSpan(9) } },
            .capture = .{ .ty = str, .span = fakeSpan(10) },
            .body_type = str,
            .body_span = fakeSpan(11),
        },
    };

    try std.testing.expectError(TypeError.MatchCaptureIncompatible, checker.analyzeMatch(file_error, &capture_error));
    switch (checker.violation().?) {
        .match_capture_incompatible => |info| {
            try std.testing.expectEqual(@as(?types.TypeRef, null), info.expected);
            try std.testing.expectEqual(str, info.actual);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    const missing_variant = [_]MatchCaseDesc{
        .{
            .pattern = .{ .variant = .{ .name = "Unknown", .span = fakeSpan(12) } },
            .capture = null,
            .body_type = str,
            .body_span = fakeSpan(13),
        },
    };

    try std.testing.expectError(TypeError.MatchVariantMissing, checker.analyzeMatch(file_error, &missing_variant));
}

test "match literal patterns must agree with the subject type" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const bool_ty = try store.primitive(.bool);
    const str = try store.primitive(.string);

    var checker = TypeChecker.init(&store);
    const cases = [_]MatchCaseDesc{
        .{
            .pattern = .{ .type_match = .{ .ty = bool_ty, .span = fakeSpan(0) } },
            .capture = null,
            .body_type = bool_ty,
            .body_span = fakeSpan(1),
        },
    };

    _ = try checker.analyzeMatch(bool_ty, &cases);

    const mismatch = [_]MatchCaseDesc{
        .{
            .pattern = .{ .type_match = .{ .ty = str, .span = fakeSpan(2) } },
            .capture = null,
            .body_type = bool_ty,
            .body_span = fakeSpan(3),
        },
    };

    try std.testing.expectError(TypeError.MatchLiteralMismatch, checker.analyzeMatch(bool_ty, &mismatch));
}

test "enforceRestrictedErrorSet validates subsets" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const file_error = try store.errorSet(.{
        .name = "FileError",
        .variants = &.{
            .{ .name = "NotFound", .payload = str },
            .{ .name = "PermissionDenied", .payload = null },
        },
    });
    const only_not_found = try store.errorSet(.{
        .name = "FileError",
        .variants = &.{
            .{ .name = "NotFound", .payload = str },
        },
    });
    const network_error = try store.errorSet(.{
        .name = "NetworkError",
        .variants = &.{
            .{ .name = "Timeout", .payload = null },
        },
    });
    const any_error = try store.errorSet(.{ .behavior = types.Type.ErrorSet.Behavior.any });

    var checker = TypeChecker.init(&store);
    try checker.enforceRestrictedErrorSet(file_error, file_error, fakeSpan(0));
    try checker.enforceRestrictedErrorSet(file_error, only_not_found, fakeSpan(1));
    try checker.enforceRestrictedErrorSet(any_error, network_error, fakeSpan(2));

    try std.testing.expectError(
        TypeError.RestrictedErrorSetMismatch,
        checker.enforceRestrictedErrorSet(file_error, network_error, fakeSpan(3)),
    );
    switch (checker.violation().?) {
        .restricted_error_set_mismatch => |info| {
            try std.testing.expectEqual(file_error, info.allowed);
            try std.testing.expectEqual(network_error, info.actual);
            try std.testing.expectEqual(@as(usize, 3), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    try std.testing.expectError(
        TypeError.RestrictedErrorSetMismatch,
        checker.enforceRestrictedErrorSet(file_error, any_error, fakeSpan(4)),
    );
}

test "analyzeFunctionReturns enforces annotations, inference, and diagnostics" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const int = try store.primitive(.int);

    var checker = TypeChecker.init(&store);

    const annotated_paths = [_]FlowResultDesc{
        .{ .result_type = str, .span = fakeSpan(0) },
        .{ .result_type = str, .span = fakeSpan(1) },
    };
    const annotated_type = try checker.analyzeFunctionReturns(.{
        .annotation = str,
        .paths = &annotated_paths,
        .span = fakeSpan(10),
    });
    try std.testing.expectEqual(str, annotated_type);

    const mismatch_paths = [_]FlowResultDesc{
        .{ .result_type = str, .span = fakeSpan(2) },
        .{ .result_type = int, .span = fakeSpan(3) },
    };
    try std.testing.expectError(
        TypeError.AnnotationMismatch,
        checker.analyzeFunctionReturns(.{
            .annotation = str,
            .paths = &mismatch_paths,
            .span = fakeSpan(11),
        }),
    );
    switch (checker.violation().?) {
        .annotation_mismatch => |info| {
            try std.testing.expectEqual(str, info.expected);
            try std.testing.expectEqual(int, info.actual);
            try std.testing.expectEqual(@as(usize, 3), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    const network_errors = try store.errorSet(.{ .name = "NetworkError", .variants = &.{} });
    const union_result = try store.errorUnion(network_errors, str);
    const union_paths = [_]FlowResultDesc{
        .{ .result_type = union_result, .span = fakeSpan(8) },
    };
    try std.testing.expectError(
        TypeError.AnnotationMismatch,
        checker.analyzeFunctionReturns(.{
            .annotation = str,
            .paths = &union_paths,
            .span = fakeSpan(14),
        }),
    );
    switch (checker.violation().?) {
        .missing_try_or_catch => |info| {
            try std.testing.expectEqual(str, info.expected);
            try std.testing.expectEqual(union_result, info.actual);
            try std.testing.expectEqual(str, info.payload);
            try std.testing.expectEqual(@as(usize, 8), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    const inferred_paths = [_]FlowResultDesc{
        .{ .result_type = str, .span = fakeSpan(4) },
        .{ .result_type = str, .span = fakeSpan(5) },
    };
    const inferred_type = try checker.analyzeFunctionReturns(.{
        .paths = &inferred_paths,
        .span = fakeSpan(12),
    });
    try std.testing.expectEqual(str, inferred_type);

    const conflict_paths = [_]FlowResultDesc{
        .{ .result_type = str, .span = fakeSpan(6) },
        .{ .result_type = int, .span = fakeSpan(7) },
    };
    try std.testing.expectError(
        TypeError.ControlFlowConflict,
        checker.analyzeFunctionReturns(.{
            .paths = &conflict_paths,
            .span = fakeSpan(13),
        }),
    );
    switch (checker.violation().?) {
        .control_flow_conflict => |info| {
            try std.testing.expectEqual(ControlFlowContext.function_return, info.context);
            try std.testing.expectEqual(str, info.established);
            try std.testing.expectEqual(int, info.conflicting);
            try std.testing.expectEqual(@as(usize, 7), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    const empty_paths = [_]FlowResultDesc{};
    try std.testing.expectError(
        TypeError.EmptyControlFlow,
        checker.analyzeFunctionReturns(.{
            .paths = &empty_paths,
            .span = fakeSpan(14),
        }),
    );
    switch (checker.violation().?) {
        .empty_control_flow => |info| {
            try std.testing.expectEqual(ControlFlowContext.function_return, info.context);
            try std.testing.expectEqual(@as(usize, 14), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "analyzeLoopResults enforces consistent exit payloads" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const bool_ty = try store.primitive(.bool);
    const str = try store.primitive(.string);

    var checker = TypeChecker.init(&store);

    const ok_paths = [_]FlowResultDesc{
        .{ .result_type = bool_ty, .span = fakeSpan(0) },
        .{ .result_type = bool_ty, .span = fakeSpan(1) },
    };
    const loop_type = try checker.analyzeLoopResults(&ok_paths, fakeSpan(20));
    try std.testing.expectEqual(bool_ty, loop_type);

    const conflict_paths = [_]FlowResultDesc{
        .{ .result_type = bool_ty, .span = fakeSpan(2) },
        .{ .result_type = str, .span = fakeSpan(3) },
    };
    try std.testing.expectError(
        TypeError.ControlFlowConflict,
        checker.analyzeLoopResults(&conflict_paths, fakeSpan(21)),
    );
    switch (checker.violation().?) {
        .control_flow_conflict => |info| {
            try std.testing.expectEqual(ControlFlowContext.loop, info.context);
            try std.testing.expectEqual(bool_ty, info.established);
            try std.testing.expectEqual(str, info.conflicting);
            try std.testing.expectEqual(@as(usize, 3), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    const empty_paths = [_]FlowResultDesc{};
    try std.testing.expectError(
        TypeError.EmptyControlFlow,
        checker.analyzeLoopResults(&empty_paths, fakeSpan(22)),
    );
    switch (checker.violation().?) {
        .empty_control_flow => |info| {
            try std.testing.expectEqual(ControlFlowContext.loop, info.context);
            try std.testing.expectEqual(@as(usize, 22), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "analyzeConditionalBranches requires matching branch results" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const int = try store.primitive(.int);
    const optional_str = try store.optional(str);

    var checker = TypeChecker.init(&store);

    const balanced = [_]FlowResultDesc{
        .{ .result_type = str, .span = fakeSpan(0) },
        .{ .result_type = str, .span = fakeSpan(1) },
    };
    const balanced_type = try checker.analyzeConditionalBranches(&balanced, fakeSpan(30));
    try std.testing.expectEqual(str, balanced_type);

    const widening = [_]FlowResultDesc{
        .{ .result_type = str, .span = fakeSpan(2) },
        .{ .result_type = optional_str, .span = fakeSpan(3) },
    };
    const widened = try checker.analyzeConditionalBranches(&widening, fakeSpan(31));
    try std.testing.expectEqual(optional_str, widened);

    const conflict_paths = [_]FlowResultDesc{
        .{ .result_type = str, .span = fakeSpan(4) },
        .{ .result_type = int, .span = fakeSpan(5) },
    };
    try std.testing.expectError(
        TypeError.ControlFlowConflict,
        checker.analyzeConditionalBranches(&conflict_paths, fakeSpan(32)),
    );
    switch (checker.violation().?) {
        .control_flow_conflict => |info| {
            try std.testing.expectEqual(ControlFlowContext.conditional, info.context);
            try std.testing.expectEqual(str, info.established);
            try std.testing.expectEqual(int, info.conflicting);
            try std.testing.expectEqual(@as(usize, 5), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }

    const empty_paths = [_]FlowResultDesc{};
    try std.testing.expectError(
        TypeError.EmptyControlFlow,
        checker.analyzeConditionalBranches(&empty_paths, fakeSpan(33)),
    );
    switch (checker.violation().?) {
        .empty_control_flow => |info| {
            try std.testing.expectEqual(ControlFlowContext.conditional, info.context);
            try std.testing.expectEqual(@as(usize, 33), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "analyzeOptionalIf unwraps optionals and enforces branch agreement" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const int = try store.primitive(.int);
    const optional_str = try store.optional(str);

    var checker = TypeChecker.init(&store);

    const info = try checker.analyzeOptionalIf(.{
        .subject_type = optional_str,
        .subject_span = fakeSpan(0),
        .then_result = .{ .result_type = str, .span = fakeSpan(1) },
        .else_result = .{ .result_type = str, .span = fakeSpan(2) },
        .span = fakeSpan(40),
    });
    try std.testing.expectEqual(str, info.payload_type);
    try std.testing.expectEqual(str, info.result_type);

    try std.testing.expectError(
        TypeError.OptionalRequired,
        checker.analyzeOptionalIf(.{
            .subject_type = str,
            .subject_span = fakeSpan(3),
            .then_result = .{ .result_type = str, .span = fakeSpan(4) },
            .else_result = .{ .result_type = str, .span = fakeSpan(5) },
            .span = fakeSpan(41),
        }),
    );
    switch (checker.violation().?) {
        .optional_required => |info_violation| try std.testing.expectEqual(str, info_violation.subject),
        else => return TestViolationError.UnexpectedViolation,
    }

    try std.testing.expectError(
        TypeError.ControlFlowConflict,
        checker.analyzeOptionalIf(.{
            .subject_type = optional_str,
            .subject_span = fakeSpan(6),
            .then_result = .{ .result_type = str, .span = fakeSpan(7) },
            .else_result = .{ .result_type = int, .span = fakeSpan(8) },
            .span = fakeSpan(42),
        }),
    );
    switch (checker.violation().?) {
        .control_flow_conflict => |info_violation| {
            try std.testing.expectEqual(ControlFlowContext.conditional, info_violation.context);
            try std.testing.expectEqual(str, info_violation.established);
            try std.testing.expectEqual(int, info_violation.conflicting);
            try std.testing.expectEqual(@as(usize, 8), info_violation.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "optional-aware conditionals widen branch payloads and reject incompatible optionals" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const int = try store.primitive(.int);
    const optional_str = try store.optional(str);
    const optional_int = try store.optional(int);

    var checker = TypeChecker.init(&store);

    const widened = try checker.analyzeOptionalIf(.{
        .subject_type = optional_str,
        .subject_span = fakeSpan(10),
        .then_result = .{ .result_type = str, .span = fakeSpan(11) },
        .else_result = .{ .result_type = optional_str, .span = fakeSpan(12) },
        .span = fakeSpan(50),
    });
    try std.testing.expectEqual(str, widened.payload_type);
    try std.testing.expectEqual(optional_str, widened.result_type);

    try std.testing.expectError(
        TypeError.ControlFlowConflict,
        checker.analyzeOptionalIf(.{
            .subject_type = optional_str,
            .subject_span = fakeSpan(13),
            .then_result = .{ .result_type = str, .span = fakeSpan(14) },
            .else_result = .{ .result_type = optional_int, .span = fakeSpan(15) },
            .span = fakeSpan(51),
        }),
    );
    switch (checker.violation().?) {
        .control_flow_conflict => |info| {
            try std.testing.expectEqual(ControlFlowContext.conditional, info.context);
            try std.testing.expectEqual(str, info.established);
            try std.testing.expectEqual(optional_int, info.conflicting);
            try std.testing.expectEqual(@as(usize, 15), info.span.start.offset);
        },
        else => return TestViolationError.UnexpectedViolation,
    }
}

test "analyzeAwait enforces promise captures and catch semantics" {
    var store = types.TypeStore.init(std.testing.allocator);
    defer store.deinit();

    const str = try store.primitive(.string);
    const void_ty = try store.primitive(.void);
    const net_errors = try store.errorSet(.{
        .name = "NetworkError",
        .variants = &.{.{ .name = "Timeout" }},
    });
    const union_str = try store.errorUnion(net_errors, str);
    const promise_str = try store.promise(str);
    const promise_union = try store.promise(union_str);

    var checker = TypeChecker.init(&store);

    const simple = try checker.analyzeAwait(.{
        .subject_type = promise_str,
        .subject_span = fakeSpan(0),
        .span = fakeSpan(60),
    });
    try std.testing.expectEqual(str, simple.payload_type);
    try std.testing.expectEqual(str, simple.result_type);
    try std.testing.expectEqual(@as(?types.TypeRef, null), simple.error_set);

    const with_catch = try checker.analyzeAwait(.{
        .subject_type = promise_union,
        .subject_span = fakeSpan(2),
        .catch_result = .{ .result_type = str, .span = fakeSpan(3) },
        .span = fakeSpan(61),
    });
    try std.testing.expectEqual(str, with_catch.payload_type);
    try std.testing.expectEqual(str, with_catch.result_type);
    try std.testing.expectEqual(net_errors, with_catch.error_set.?);

    const block = try checker.analyzeAwait(.{
        .subject_type = promise_union,
        .subject_span = fakeSpan(4),
        .success_result = .{ .result_type = void_ty, .span = fakeSpan(5) },
        .catch_result = .{ .result_type = void_ty, .span = fakeSpan(6) },
        .span = fakeSpan(62),
    });
    try std.testing.expectEqual(str, block.payload_type);
    try std.testing.expectEqual(void_ty, block.result_type);
    try std.testing.expectEqual(net_errors, block.error_set.?);

    try std.testing.expectError(
        TypeError.ErrorUnionRequired,
        checker.analyzeAwait(.{
            .subject_type = promise_str,
            .subject_span = fakeSpan(7),
            .success_result = .{ .result_type = void_ty, .span = fakeSpan(8) },
            .catch_result = .{ .result_type = void_ty, .span = fakeSpan(9) },
            .span = fakeSpan(63),
        }),
    );
    switch (checker.violation().?) {
        .error_union_required => |violation| try std.testing.expectEqual(str, violation.subject),
        else => return TestViolationError.UnexpectedViolation,
    }

    try std.testing.expectError(
        TypeError.ErrorUnionRequired,
        checker.analyzeAwait(.{
            .subject_type = promise_str,
            .subject_span = fakeSpan(10),
            .catch_result = .{ .result_type = str, .span = fakeSpan(11) },
            .span = fakeSpan(64),
        }),
    );
    switch (checker.violation().?) {
        .error_union_required => |violation| try std.testing.expectEqual(str, violation.subject),
        else => return TestViolationError.UnexpectedViolation,
    }
}

const TestViolationError = error{UnexpectedViolation};
