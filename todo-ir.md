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

- [ ] address mapping
- [ ] arithmetic
- [ ] boolean algebra
- [ ] basic executable calls
- [ ] pipelines

## execution context

- [ ] threads

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
