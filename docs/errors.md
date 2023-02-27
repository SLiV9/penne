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
    return:
    x
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
    return:
    x
}
```

{% endannotatederrorreference %}
