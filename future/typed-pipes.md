# typed pipes

## introduction

We would like to have typed pipes for stdin and stdout in functions.
The stdin and stdout types are part of a function's signature:
`fn String hello() ExecutableError!String echo "hello"`
In the example above, stdin has type `String` while stdout has type `ExecutableError!String`.
When a function has type `Void` for a pipe, it should close that pipe immediately.
Any side effects using stdin or stdout in the function body has to match the type given in the function signature. For example this function declaration should result in a type error of mismatched stdin types since it is calling a function that takes `Void` as stdin and not `String` as the caller does:

```runic
fn Void hello() !String echo "hello"
fn String greetings() !String hello
```

## executable calls

We can't deduce the types of each executable, so we have to have a catch-all type for all executable signatures: `fn String @() ExecutableError!String`

## typed pipes

Look at this example:

```runic
fn String parseInt() ParseIntError!Int {
    // some kind of magic implementation
}

const x = echo "1234" | parseInt
```

This pipeline is essentially changing the type of the output assigned to `x`.
Traditionally (i.e. in `bash`), pipelines only work with strings. However, we have the opportunity to allow for typed pipes, and the pipe operator `|` can glue together functions with compatible stdin/stdout types. As long as the stdout of the previous expression matches the stdin of the next, this should be allowed. The type checker should error if the types does not match. Some coercion might be needed, such as non-optionals into optionals, non-error unions into error unions, etc. The coercion rules still needs to be ironed out. The compiler needs to be able to glue these pipelines together as well, realizing that not all stdin and stdout are represented by our current structure of buffering byte streams. We do need to layout a plan for this feature more carefully.
