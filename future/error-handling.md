# error handling

## introduction

We would like to have zig-like error handling.
Both with values and types.
So for example, you could define an error type:
`const MyError = error { MySpecificError }`
Then, it can be used in a binding for example:
`const value_or_error: MyError!String = MyError.MySpecificError`
Also as a return value from a function:

```runic
const ParseIntError = error { ExpectedNumber }
fn String parseInt() ParseIntError!Int {
  return ParseIntError.ExpectedNumber
}
```

## `catch` keyword

`const int = echo "1234" | parseInt catch 0`
`catch` takes an error union value, checks if it's an error and if it is, evaluates to a default value (0 in the example above), if it was not an error, the original value is used
so `ParseIntError.ExpectedNumber catch "hello"` will evaluate into `"hello"`
while `"success" catch "hello"` will evaluate into "success".
Now this was just an example to show how catch should work, we should probably not allow non-error types to be followed by the `catch` keyword. That should be a type check error.

## `try` keyword

Next up is the `try` keyword. The try keyword is syntactic sugar. The following expression:
`const int = try (echo "1234" | parseInt)`
Will turn into:
`const int = (echo "1234" | parseInt) catch |err| return err`

## executable calls

All executable calls will have an inherent error union type as the stdout type:
`const result: ExecutableError!String = echo "this should succeed"`

## inferred error union types

Error union types should be able to be inferred:

```runic
const result = echo "hello" infers to ExecutableError!String
fn Void thisCouldError() !String { infers to ExecutableError!String
  grep "--invalid-flag"
}
```
