# stdin: pull model (inherit the fd, read `&0` on demand)

Status: **shipped** — the pull model is live for all stdin (pipes, files, and
interactive terminals). One optional follow-up remains (chunked forwarding for
command-output throughput). Owner: Lumor.

## Problem

Runic used to feed every command stdin by **pushing**: a background "stdin
drainer" thread eagerly read the real fd 0 into a buffer and wrote it into each
command's stdin pipe. This has three bad consequences:

- A command that **doesn't read stdin** (`printf`, `echo`) still had the
  already-available input pushed into its pipe and **lost** on exit — so a
  following `&0` read saw nothing. (The prompt-then-read pattern failed when
  input was piped/immediate.)
- There was **no way to know how much a command consumed**, so leftover input
  couldn't be handed to the next command / `&0`.
- The drainer read fd 0 **a byte per syscall** — slow on large input.

## The model (what we want)

Don't push. A pipe/fd **signals availability**; a consumer **pulls** what it
wants; whatever it doesn't take **stays** for the next reader. The kernel already
implements exactly this for a shared file descriptor (it tracks the read offset),
so the design is: **commands read the real fd directly (inherit), and Runic reads
`&0` from that same fd on demand.** No middle buffer, no pushing, consumption
tracked by the kernel.

## What shipped

The pull model is used for all stdin (`run_script.zig` sets
`stream.Config.real_stdin = true` unconditionally):

- Top-level commands **inherit the real fd** (`childStdinBehavior` →
  `.inherit` for a `real_stdin` pipe; no `connectDestination`). They read it
  directly; the kernel offset means leftover stays for the next command / `&0`.
- The stdin drainer is **demand-gated**: the `.stream` driver for a `real_stdin`
  pipe idles unless a `&0` read is waiting (`context.stdin_demand`, set/cleared by
  the byte read instructions) or a destination is connected (`&0 | …`). So it
  never competes with an inheriting command for the fd.
- `&0` reads (`collect_stdin` / `read_stream_element` / `stream_take_bytes` /
  `stream_until_bytes`) pull from the pipe on demand via that gated drainer.
- **Process completion no longer depends on a stdin pipe.** It used to be driven
  by the stdin pipe's write side EPIPE-ing when the child exited; with stdin
  inherited there is no such pipe, and with stdout/stderr also inherited (a TTY)
  no stream ever reaches EOF, so the old `check_close_parent` path never fired and
  a top-level command's `.wait` spun forever. Fix: `ProcessCloseable.getResult`
  (`process.zig`) now **reaps the child non-blockingly** (`waitpid(WNOHANG)`) when
  both stdout and stderr are inherited (nothing to drain), setting `term` once it
  exits. (With a pipe on stdout/stderr the stream-close path still drives it.)

Delivered (all verified): interactive (PTY) `printf "prompt"` then `&0 | until
"\n"` reads the typed line; the same piped reads the input; `head -c 5` then
`const rest = &0` splits input exactly (`ABCDE` / `FGHIJ`); `cat | wc` reads the
full input at 70 KB and 5 MB; `&0 | cat` forwards stdin to a command. Covered by
`tests/features/stdin_shared_regression.rn`.

## Follow-up: chunked forwarding for throughput

`PipeReader.stream` (`process.zig`) reads **one byte per `read()`**, breaking at
newline. This is now the bottleneck for **command-output forwarding** (e.g.
`cat | wc` on MB-scale data — ~8s, mostly `sys` in per-byte reads), because the
inherit change already removed per-byte reads from the *stdin* path. A bulk read
(e.g. 4 KB per call) is a ~100× syscall reduction.

**Caution (learned the hard way):** `PipeReader` is shared by stdin *and* every
command's stdout/stderr, and its line-at-a-time cadence is currently acting as
the cooperative scheduler's **backpressure**. Naively bulk-reading deadlocked
basic pipelines (`cat | wc` hung at all sizes) and, with EPIPE guards, OOM'd on
large pipelines. So this is **not** a chunk-size tweak — it requires giving the
`stream.zig` forward loop real backpressure (read a bounded amount, non-blocking
write downstream, pull more only once the downstream drained), applied uniformly
to stdin and inter-stage pipes. Do it as its own change, with the full pipeline
suite + a large-input `cat | wc` as guards.
