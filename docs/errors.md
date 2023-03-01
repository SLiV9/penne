---
layout: default
title: Error Code Reference
nav_order: 10
---

{% annotatederrorreference %}

## Error code E100

Unexpected end of file.

### Example of erroneous code

```penne
fn main() -> i32
{
    var x: []i32 = [17, 50];
    if true == true
    {
        foo(
```

## Error code E101

The source file contains zero bytes of data.

### Example of erroneous code

```penne
```

Any whitespace character is enough to suppress this error.

```penne


```

Alternatively a comment can be used.

```penne
// This file intentionally left blank.
```

### Explanation

A source file being completely empty is quite uncommon, and it may indicate some sort of IO error.
One particular example is writing source code to a newly created temporary file and passing the name of that file to the compiler *before* flushing and closing the temporary file.
Because a Penne source file without declarations produces no other errors, this error code is used to warn about such a scenario.

## Error code E110

Unexpected character in source file or string literal.

### Example of erroneous code

```penne
fn main() -> i32
{
    var x = 1 @ 2;
}
```

### Explanation

Penne source files must be US-ASCII or UTF-8 encoded. In addition, ASCII control characters (`U+0000` through `U+001F`, and `U+007F`) are disallowed inside string literals and must be escaped. Outside of string literals, only letters, digits, underscores and characters used in Penne operators are allowed.

## Error code E140

An integer literal is larger than the maximum value for its type, or smaller than the minimum value.

### Example of erroneous code

```penne
fn main() -> i32
{
    return: 2138712987321987319321987317931831928i32
}
```

## Error code E141

An integer literal has a suffix that is not a valid integer type.

### Example of erroneous code

```penne
fn main()
{
    var x = 123127312i63;
}
```

## Error code E142

An integer literal without type suffix is too big to parse.

### Example of erroneous code

```penne
fn main()
{
    var x: i128 = 1180591620717411303424;
}
```

To fix this, add an explicit type suffix.

```penne
fn main()
{
    var x: i128 = 1180591620717411303424i128;
}
```

### Explanation

An integer literal must have a type suffix if it fits neither an `i64` nor a `u64` --- that is, if it is smaller than `-(2^63 - 1)` or larger than `2^64 - 1`.
This type suffix must be `i128` or `u128`.

## Error code E143

A hexadecimal or binary integer literal is too big to parse.

### Example of erroneous code

```penne
fn main()
{
    var x: u128 = 0x46252a329482eaf72058e10b93e6f52;
}
```

### Explanation

Hexadecimal and binary integer literals have to fit a `u64` --- that is, they cannot be larger than `2^64 - 1`.

## Error code E160

A string literal is not closed before the end of the line.

### Example of erroneous code

```penne
fn main()
{
    var x = "Hello
    world!";
}
```

Multiline strings are not supported. Newlines in strings have to be escaped.

```penne
fn main()
{
    var x = "Hello\nworld!";
}
```

If a string is too large to fit one line according to your formatting preferences, it can be closed and then reopened on a new line.

```penne
fn main()
{
    var x = "Hello"
    " world!";
}
```

This string is identical to `"Hello world!"`.

## Error code E161

A string literal contains a backslash just before the end of the line.

### Example of erroneous code

```penne
fn main()
{
    var x = "Hello \
    world!";
}
```

This form of line continuation is seen in other languages but is not supported in Penne.
To wrap a string across multiple lines, it can be closed and then reopened on a new line.

```penne
fn main()
{
    var x = "Hello "
    "world!";
}
```

## Error code E162

A string literal contains an invalid escape sequence or an unescaped backslash.

### Example of erroneous code

```penne
fn main()
{
    var x = "C:\Program Files";
}
```

Backslashes must be escaped inside string literals.

```penne
fn main()
{
    var x = "C:\\Program Files";
}
```

## Error code E300

The parser expected a different token based on context.

### Example of erroneous code

```penne
fn main()
{
    var usize = true;
}
```

## Error code E301

A non-keyword identifier was followed directly by a semicolon.

### Example of erroneous code

```penne
fn main()
{
    stop;
}
```

## Error code E302

A return value was followed by a semicolon.

### Example of erroneous code

```penne
fn main() -> i32
{
    return: 10;
}
```

To fix this, remove the semicolon after the return value.

```penne
fn main() -> i32
{
    return: 10
}
```

### Explanation

Only statements are terminated by semicolons. There is no return statement. A function's *return value* is separated from the rest of the function body by the special `return` label.

## Error code E330

The return type is missing from a function with a return value.

### Example of erroneous code

```penne
fn foo()
{
    return: true
}
```

To fix this, add an explicit return type.

```penne
fn foo() -> bool
{
    return: true
}
```

## Error code E331

The return type is missing from a function with an ambiguous return value.
This is usually accompanied by another type inference error, such as E581 or E582.

### Example of erroneous code

```penne
fn foo()
{
    return: 500
}
```

To fix both errors, add an explicit return type.

```penne
fn foo() -> i32
{
    return: 500
}
```

### Explanation

Type inference does not cross function borders and functions with a return value need an explicit return type.
In some cases the compiler cannot infer the type of the return value in order to give a useful suggestion.

## Error code E333

The return type does not match the type of the return value.

### Example of erroneous code

```penne
fn main() -> bool
{
    return: 50
}
```

## Error code E334

A function with a return type is missing its return value.

### Example of erroneous code

```penne
fn main() -> bool
{
    var result = true;
}
```

In order to return a value from a function, use the special `return` label followed by the value you want to return.

```penne
fn main() -> bool
{
    var result = true;
    return: result
}
```

## Error code E335

A return value is missing after a `return` label.

### Example of erroneous code

```penne
fn main()
{
    return:
}
```

Functions that do not return a value must use a different label instead.

```penne
fn main()
{
    end:
}
```

### Explanation

Although `return` is not keyword, the label `return` has a special meaning. It is used in the body of a function to separate the statements from the return value. Even functions without statements must use the `return` label to return a value, and `return` must always be the last label used.

In functions that do not return a value, any identifier other than `return` may be used as the last label.

## Error code E343

A constant declaration is missing its type.

### Example of erroneous code

```penne
const X = 98;
```

To fix this, add an explicit type.

```penne
const X: i32 = 98;
```

## Error code E344

A function parameter is missing its type.

### Example of erroneous code

```penne
fn foo(x) -> i32
{
    return: x * x
}
```

To fix this, add an explicit type.

```penne
fn foo(x: i32) -> i32
{
    return: x * x
}
```

## Error code E346

A structure member is missing its type.

### Example of erroneous code

```penne
struct Foo
{
    x,
    y,
}
```

To fix this, add an explicit type.

```penne
struct Foo
{
    x: i32,
    y: i32,
}
```

## Error code E350

A compound type is deemed invalid.

### Example of erroneous code

```penne
fn foo()
{
    var x: [][]i32;
}
```

### Explanation

In addition to primitive types such as `i32` or `bool`, compound types can be created such as `[10]i32` (an array of `i32` of length 10), `&bool` (a pointer to a `bool`) or `&[]u8` (a pointer to an array slice of `u8`). Not all such compounds are valid.

In particular, `[10]T` and `[]T` are only valid if the type `T` is valid and has a size that is known at compile time. The type `[]T` does not have a compile-time known size, hence compound types such as `[10][]u8` and `[][]i32` are invalid.

## Error code E351

A type that cannot be returned is used as a return type.

### Example of erroneous code

```penne
fn foo() -> [1000]i32;
```

### Explanation

Not all valid types can be used as a return type.
In particular structs, arrays and array slices cannot be returned.

## Error code E352

A type that cannot be assigned is used to declare a variable.

### Example of erroneous code

```penne
fn main()
{
    var x: void;
}
```

## Error code E353

A type that cannot be a constant is used to declare a constant.

### Example of erroneous code

```penne
const X: []i32 = [10, 20, 30];
```

To fix this, add an explicit size to the array type.

```penne
const X: [3]i32 = [10, 20, 30];
```

## Error code E354

A type that cannot be used as a parameter is used to declare a parameter.

### Example of erroneous code

```penne
fn foo(x: void);
```

## Error code E356

A type that cannot be part of a struct is used to declare a struct member, or a type that cannot be part of a word is used to declare a word member.

### Example of erroneous code

```penne
word64 Foo
{
    x: &i32,
}
```

### Explanation

The members of word can be fixed size integers, `bool` or other words.

## Error code E358

A type that is not part of the external ABI is used in a function marked `extern`.

### Example of erroneous code

```penne
extern fn foo(x: u128);
```

### Explanation

For a detailed list of the types that allowed in `extern`, see the documentation about the [external ABI](features.html#interoperability-with-c).

## Error code E360

A constant expression contains an unsupported operation.

### Example of erroneous code

```penne
const A: i32 = 200;
const X: &i32 = &A;
```

### Explanation

The value assigned to a constant must be evaluated at compile time.
In addition to literals and other constants, constant expressions may use basic arithmetic, bitwise operations and primitive casts.

## Error code E361

A constant expression contains a function call.

### Example of erroneous code

```penne
fn calculate_value() -> i32;
const A: i32 = calculate_value();
```

### Explanation

Function calls are not allowed in constant expressions because their return value is not known at compile time.

## Error code E380

The declared size of a word does not match the total size of its members.

### Example of erroneous code

```penne
word64 Xyz
{
    x: i32,
    y: i32,
    z: i32,
}
```

One way to fix this is to use a larger word size and add padding.

```penne
word128 Xyz
{
    x: i32,
    y: i32,
    z: i32,
    _padding: i32,
}
```

Alternatively a `struct` can be used, which has no size restrictions.

```penne
struct Xyz
{
    x: i32,
    y: i32,
    z: i32,
}
```

### Explanation

Words are declared with a keyword that also specifies the size of the word in bits. For example, a `word32` could consist of a single `i32` member, or two `u16` members, or a `word16`, a `word8` and an `i8`.

## Error code E400

A `goto` statement uses a label that is not defined or not in scope.

### Example of erroneous code

```penne
fn main() -> i32
{
    goto inner;
    var i = 1;
    {
        var x = 10;
        {
            inner:
            var y = 20;
            i = y;
        }
        i = x;
    }
    return: i
}
```

### Explanation

See the documentation on [label scopes and `goto`](features.html#scoped-goto-statements).

## Error code E401

A function call refers to a function that has not been declared.

### Example of erroneous code

```penne
fn foo()
{
    bar();
}
```

Functions that are resolved at link time must be predeclared.

```penne
fn bar();

fn foo()
{
    bar();
}
```

## Error code E402

An expression refers to a variable, parameter or constant that has not been declared or that is not in scope.

### Example of erroneous code

```penne
fn foo()
{
    return: x
}
```

## Error code E405

An identifier is used as a type, but no struct or word with that name has been declared.

### Example of erroneous code

```penne
fn foo() -> UndeclaredStruct;
```

## Error code E406

A structure literal or dereference refers to a member that is not part of that structure's declaration.

### Example of erroneous code

```penne
struct Digest
{
    buffer: [1024]u8,
    len: usize,
}

fn foo()
{
    var digest = Digest {
        length: 0usize,
    };
    var buffer = &digest.buffre;
}
```

## Error code E413

The definition of a constant depends on its own value.

### Example of erroneous code

```penne
const X: i32 = Y + 1;
const Y: i32 = X + 1;
```

## Error code E415

A structure is embedded into one of its own members.

### Example of erroneous code

```penne
struct Buffer
{
    data: [1024]u8,
    metadata: Metadata,
}

struct Metadata
{
    debug_code: i32,
    message: Buffer,
}
```

## Error code E420

Two labels with the same name are present in the same scope.

### Example of erroneous code

```penne
fn main()
{
    next:
    next:
}
```

## Error code E421

Two functions with the same name are declared.

### Example of erroneous code

```penne
fn foo(x: i32);

fn foo(x: i64);
```

## Error code E422

A variable is declared with the same name as another variable, parameter or constant in scope.

### Example of erroneous code

```penne
fn foo(x: i32) -> i32
{
    var x = 0;
    return: x
}
```

## Error code E423

Two constants with the same name are declared.

### Example of erroneous code

```penne
const X: i32 = 200;
const Y: i32 = 250;
const X: i32 = 300;
```

## Error code E424

A parameter is declared with the same name as another parameter or constant in scope.

### Example of erroneous code

```penne
fn draw_rectangle(x: i32, y: i32, w: i32, w: i32);
```

## Error code E425

Two structures with the same name are declared.

### Example of erroneous code

```penne
struct Position
{
    x: i32,
    y: i32,
    z: i32,
}

word16 Position
{
    row: i8,
    col: i8,
}
```

## Error code E426

A structure is declared with two members of the same name.

### Example of erroneous code

```penne
struct ItemCollection
{
    items: [1024]u64,
    items: usize,
}
```

## Error code E470

An `import` declaration could not be resolved.

### Example of erroneous code

```penne
import "nice_functionality.pn";
```

### Explanation

The Penne compiler does not search for additional files on its own.
In order to include a source file in the compilation process, add it as an extra argument after the main entry point.

```console
$ penne build src/main.pn src/nice_functionality.pn
```

## Error code E482

A `goto` statement jumps past a variable declaration to a label, but this variable is used afterwards. This results in a variable that is declared in some branches, but not others, which is unsound.

### Example of erroneous code

```penne
fn main(input: i32) -> i32
{
    var result = 0;
    if input == 1
        goto next;
    var a = 20;
    var b = 5;
    result = result + a + b;
    next:
    result = result + a;
    return: result
}
```

To fix this, move the variable declaration in front of the offending `goto` statement.

```penne
fn main(input: i32) -> i32
{
    var result = 0;
    var a = 20;
    if input == 1
        goto next;
    var b = 5;
    result = result + a + b;
    next:
    result = result + a;
    return: result
}
```

## Error code E800

A `loop` statement is not the final statement of the block in which it appears.

### Example of erroneous code

```penne
fn main() -> i32
{
    var result;
    var i: i32 = 0;
    {
        var y = 3000 + i;
        i = i + 1;
        if i == 10
            goto exit_loop;
        loop;
        exit_loop:
        result = y;
    }
    result = result * 2;
    return: result
}
```

A `loop` statement may only appear as the final statement in a block.
The code needs to be restructured.

```penne
fn main() -> i32
{
    var result;
    var i: i32 = 0;
    {
        var y = 3000 + i;
        i = i + 1;
        if i == 10
        {
            result = y;
            goto exit_loop;
        }
        loop;
    }
    exit_loop:
    result = result * 2;
    return: result
}
```

## Error code E801

A `loop` statement appears on its own in the body of a function.

### Example of erroneous code

```penne
fn read_from_client(buffer: &[]u8, exact_size: usize);
fn handle_message_from_client(buffer: []u8, message_size: usize);

fn listen_to_client()
{
    var header_buffer: [1]u8;
    var message_buffer: [256]u8;
    read_from_client(&header_buffer, 1);
    var header: u8 = header_buffer[0];
    var message_size: usize = header as usize;
    read_from_client(&message_buffer, message_size);
    handle_message_from_client(message_buffer, message_size);
    loop;
}
```

To fix this, wrap the looping statements in a block.

```penne
fn read_from_client(buffer: &[]u8, exact_size: usize);
fn handle_message_from_client(buffer: []u8, message_size: usize);

fn listen_to_client()
{
    var header_buffer: [1]u8;
    var message_buffer: [256]u8;
    {
        read_from_client(&header_buffer, 1);
        var header: u8 = header_buffer[0];
        var message_size: usize = header as usize;
        read_from_client(&message_buffer, message_size);
        handle_message_from_client(message_buffer, message_size);
        loop;
    }
}
```

## Error code E840

Missing braces around a conditional branch.

### Example of erroneous code

```penne
fn main(x: i32) -> i32
{
    var result = 1;
    if x == 0
        result = 10;
    return: result
}
```

To fix this, add braces around the statement (or statements) that belong to that branch.

```penne
fn main(x: i32) -> i32
{
    var result = 1;
    if x == 0
    {
        result = 10;
    }
    return: result
}
```

## Error code L1800

A `loop` statement appears on its own in the branch of an `if` statement.

### Example of erroneous code

```penne
fn main() -> i32
{
    var x = 33;
    var i = 1;
    {
        x = x * i;
        i = i + 1;
        if i != 10
        {
            loop;
        }
    }
    x = x + 1;
    return: x
}
```

### Explanation

The branches of an `if` statement must be either `goto` statements or block statements. Therefore the following code is not valid:

```penne
        if i != 10
            loop;
```

One might attempt to solve this by wrapping the `loop` statement in braces:

```penne
        if i != 10
        {
            loop;
        }
```

However this usually does not have the intended effect. Instead of looping back into whatever block contains the `if` statement, the branch is its own block containing only a `loop` statement.
Because a `loop` statement causes execution to continue with the first statement of the block it is contained in, entering this block immediately creates an inescapable infinite loop.
If that *is* intended, this lint can be suppresed by adding a harmless second statement to the block, such as an unused label.

```penne
        if i != 10
        {
            this_is_an_infinite_loop:
            loop;
        }
```

If the intention is to create a conditional loop, such as in the erroneous example above, it is usually better to invert the condition and use a `goto` statement.

```penne
fn main() -> i32
{
    var x = 33;
    var i = 1;
    {
        x = x * i;
        i = i + 1;
        if i == 10
            goto end;
        loop;
    }
    end:
    x = x + 1;
    return: x
}
```

{% endannotatederrorreference %}
