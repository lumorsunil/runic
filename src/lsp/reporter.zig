const std = @import("std");
const Workspace = @import("workspace.zig").Workspace;
const types = @import("types.zig");
const Diagnostic = @import("diagnostics.zig").Diagnostic;

pub fn reportErrors(
    workspace: *Workspace,
    err_info: anytype,
) !void {
    const document_store = workspace.documents;
    const allocator = document_store.allocator;

    for (err_info.diagnostics()) |d| {
        const diag_uri = try document_store.resolveUri(d.span().start.file);
        const diag_doc = document_store.get(diag_uri).?;

        try diag_doc.diagnostics.append(allocator, .{
            .uri = diag_uri,
            .message = try allocator.dupe(u8, d.message),
            .span = d.span(),
            .severity = std.meta.stringToEnum(
                types.DiagnosticSeverity,
                d.severity(),
            ).?,
        });
    }
}
