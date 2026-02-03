# todo-ir

## data

- [x] void
- [x] exit code
- [x] unsigned integer
- [x] string (as slice)
- [x] stream
- [x] addr
- [x] struct
- [x] closeable (handles) (processes/etc)
- [x] pipe (handles)
  - [x] generic sources/destinations
    - [x] executable (file handle)
    - [x] blocks
    - [x] function calls
    - [ ] values
- [x] blocks
- [x] blocks as pipes

## instructions

- [x] instruction addr
- [x] labels
- [x] stack push
- [x] stack pop
- [x] arithmetic
- [x] compare
- [x] logical
- [x] new ref
- [x] set addr to value
- [x] unconditional jump
- [x] conditional jump if true
- [x] conditional jump if false
- [x] call executable
- [x] wait
- [x] fork
- [x] new pipe
- [x] set pipe option
- [x] forward pipe
- [x] exit

## compiler

- [x] sharing bindings between scopes/threads
  - [x] each thread has it's own closure
  - [x] closure-relative addresses
  - [x] shared binding for deeply nested closures
  - [ ] mutable variables
- [x] address mapping
- [x] basic executable calls
- [x] if else
- [ ] for loops
- [x] read-only data
- [x] instructions
- [x] labels
- [x] refs
- [x] structs
  - [ ] user-land structs
- [x] struct types
  - [ ] used defined struct types
- [x] arithmetic
- [x] boolean algebra
- [x] pipelines
  - [x] multiple sources in streams ReaderWriterStream (stderr usually connects many sources)
  - [ ] error handling?
  - [ ] && / ||
  - [ ] redirections
- [x] function declarations
  - [ ] arguments
- [x] functions calls
  - [ ] arguments
  - [ ] fix ref bug (calling same function multiple times to trigger)
- [x] bindings

## execution context

- [x] (virtual) threads

- One execution step:
  - Main Thread
    1. If thread is asleep, check wake up condition
    2. Otherwise, execute next instructions
    3. When exits, store the exit code so we can return it at the end of the program
  - Background Threads
    1. If thread is asleep, check wake up condition
    2. Otherwise, execute next instructions
  - Pipe Threads
    1. If data is available on source, then consume that data and forward it to destination
    2. Otherwise, return
    3. If stream ended, remove pipe thread
- When all threads have exited, exit the program with the exit code from the Main thread

### shared between threads

- [x] read-only data
- [x] instructions
- [x] struct types
- [x] labels

### per thread

- [x] refs
- [x] stack and stack counter
- [x] instruction counter

## evaluator

- [x] instruction step
- [x] new ref
- [x] push
- [x] pop
- [x] set
- [x] jump
- [x] executables
  - [x] execute
  - [x] wait for executable
- [x] exit
- [x] pipes
  - [x] new pipe
  - [x] stream pipe (blocking)
  - [x] set pipe option
  - [x] forward pipe
- [x] threads
  - [x] fork
  - [x] wait for thread
- [x] materialize string
  - [x] stream
  - [x] slice
  - [x] unsigned integer
  - [x] dereference addr
  - [x] exit code
  - [ ] execution result
- [x] arithmetic
- [x] logical
- [x] compare

## debugger

- [x] commands
  - [x] step
  - [x] quit
  - [x] threads
  - [x] instructions
  - [x] pipes
  - [x] breakpoint
    - [x] add
    - [x] remove
    - [x] list
- [x] command history per session
- [x] stdin processing
  - [x] up/down arrow -> move command history cursor
  - [x] autocomplete
    - [x] main commands (only first part of the command is autocompleted right now)
    - [ ] sub-commands
    - [ ] arguments
  - [x] undo word/line
- [ ] memory view
- [ ] IR instructions view
- [ ] source code view
- [ ] output view (stdout/stderr/combined)
- [ ] tui lib?

## bugs

- [x] grep is not outputting anything, see ir-blocks.rn
- [ ] `cat | grep "hello"` does not react on stdin
- [x] cannot call function multiple times
- [x] debugger: cannot paste to stdin
- [ ] `cat` on it's own does not echo back
