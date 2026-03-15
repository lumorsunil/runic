const std = @import("std");

const Error = error{ Error1, Error2, Fatal, Fallback };

fn error_function() Error!usize {
    return Error.Error1;
}

pub fn main() !void {
    const n = error_function() catch |err| switch (err) {
        Error.Error1 => brk: {
            std.log.err("Error1 happened", .{});
            break :brk 1;
        },
        Error.Error2 => brk: {
            std.log.err("Error2 happened", .{});
            break :brk 2;
        },
        Error.Fatal => return err,
        Error.Fallback => 3,
    };

    std.log.debug("n = {}", .{n});
}
