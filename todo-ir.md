# todo-ir

## data

- [x] void
- [x] exit code
- [x] unsigned integer
- [x] string (as slice)
- [x] stream
- [x] addr
- [x] struct
- [ ] pipe
  - [ ] generic sources/destinations
    - [ ] executable (file handle)
    - [ ] blocks
    - [ ] function calls
    - [ ] values
- [ ] blocks
- [ ] blocks as pipes

## instructions

- [x] instruction addr
- [x] labels
- [x] stack push
- [x] stack pop
- [ ] arithmetic
- [ ] compare
- [ ] logical
- [x] new ref
- [x] set addr to value
- [x] unconditional jump
- [x] conditional jump if true
- [x] conditional jump if false
- [x] call executable
- [x] exit

## compiler

- [x] address mapping
- [x] basic executable calls
- [x] if else
- [x] read-only data
- [x] instructions
- [x] labels
- [x] refs
- [x] structs
  - [ ] user-land structs
- [x] struct types
  - [ ] used defined struct types
- [ ] arithmetic
- [ ] boolean algebra
- [ ] pipelines

## execution context

- [ ] (virtual) threads

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

- [ ] threads
- [x] instruction step
- [x] new ref
- [x] push
- [x] pop
- [x] set
- [x] jump
- [x] call executable
- [x] exit
- [x] materialize string
  - [x] stream
  - [x] slice
  - [x] unsigned integer
  - [x] dereference addr
  - [x] exit code
  - [ ] execution result

## debugger

- [x] commands
  - [x] step
  - [x] quit
- [x] command history per session
- [x] stdin processing
  - [x] up/down arrow -> move command history cursor
- [ ] memory view
- [ ] IR instructions view
- [ ] source code view
- [ ] output view (stdout/stderr/combined)
- [ ] tui lib?
