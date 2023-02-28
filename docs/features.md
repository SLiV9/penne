---
layout: default
title: Language Features
nav_order: 2
---

## Language Features

An overview of the more unique language features of Penne:

### Scoped goto statements

In Penne, `goto` is a local forward-only jump. This is achieved by giving labels a reverse scope: similar to how variables cannot be referenced before they are declared, labels cannot be jumped to after they are declared.

```penne
fn foo() -> i32
{
    var x = 0;
    { // The scope of the label `end` ends here.
        goto end;
        x = 10; // This line is not executed.
        end: // The scope of the label `end` starts here.
    }
    x = x + 1;
    return: x
}
```

Like variables, label scopes extend into inner blocks. Thus, a `goto` statement can jump out any number of nested blocks.

```penne
fn foo()
{
    var i = 1;
    {
        var x = 10;
        {
            var y = 20;
            i = 20; // Valid because the variable `i` is in scope here.
            goto end;  // Valid because the label `end` is in scope here.
        }
        i = x; // This line is not executed.
    }
    end:
}
```

In order to keep `goto` statements sound and easy to understand, a `goto` statement cannot jump into an inner block.

```penne
fn foo()
{
    goto inner; // Invalid, the label `inner` is not in scope here.
    var i = 1;
    {
        var x = 10;
        { // The scope of the label `inner` ends here.
            inner: // The scope of the label `inner` starts here.
            var y = 20;
            i = y;
        }
        i = x;
    }
}
```

This compiler tells you that it cannot find the label in scope.

<pre class="terminal-output f9 b9">
<span class="bold"><span class="f1"><span class="f1">[E400] Error:</span></span></span> Undefined label
   <span class="ef246">╭─[</span>tests/samples/invalid/jump_to_inner.pn:3:12<span class="ef246">]</span>
   <span class="ef246">│</span>
 <span class="ef246">3 │</span> <span class="ef249">    goto </span><span class="f3">inner</span><span class="ef249">; // Invalid, the label `inner` is not in scope here.</span>
 <span class="ef246">  ·</span>          <span class="f3">──┬──</span>
 <span class="ef246">  ·</span>            <span class="f3">╰────</span> Reference to undefined label '<span class="f3">inner</span>'.
<span class="ef246">───╯</span>
</pre>

Similarly, labels that have at least one in-bound `goto` statement automatically *prune* the scope of variables declared higher up in the same block. This prevents `goto` statements from jumping over the declaration of a variable into code that references that variable.

```penne
// ...

fn overlapping(input: i32) -> i32
{
    var result = 200;
    if input == 0
        goto return;
    if input == 1
        goto next;
    var a = 20;
    result = result + a;
    if input == 2
        goto return;
    next:
    var b = 10;
    result = result + a + b;
    return: result
}
```

In `input` were to equal `1`, the declaration of the variable `a` would be skipped, which would leave the expression `result + a + b` undefined. The compiler refuses the compile this code.

<pre class="terminal-output f9 b9">
<span class="f1">[E482] Error:</span> Variable declaration may be skipped
    <span class="ef246">╭─[</span>tests/samples/invalid/conditional_declaration.pn:32:21<span class="ef246">]</span>
    <span class="ef246">│</span>
 <span class="ef246">25 │</span> <span class="ef249">       </span><span class="f6">goto</span><span class="ef249"> next;</span>
 <span class="ef246">   ·</span>        <span class="f6">──┬─</span>
 <span class="ef246">   ·</span>          <span class="f6">╰───</span> A jump from this <span class="f6">`goto`</span> statement to '<span class="f5">next</span>'...
 <span class="ef246">26 │</span> <span class="ef249">    var </span><span class="f3">a</span><span class="ef249"> = 20;</span>
 <span class="ef246">   ·</span>         <span class="f3">┬</span>
 <span class="ef246">   ·</span>         <span class="f3">╰──</span> ...may skip the declaration of the variable '<span class="f3">a</span>'.
 <span class="ef246">   ·</span>
 <span class="ef246">30 │</span> <span class="ef249">    </span><span class="f5">next:</span>
 <span class="ef246">   ·</span>     <span class="f5">──┬──</span>
 <span class="ef246">   ·</span>       <span class="f5">╰────</span> After this label, the existence of '<span class="f3">a</span>' is dubious.
 <span class="ef246">   ·</span>
 <span class="ef246">32 │</span> <span class="ef249">    result = result + </span><span class="f3">a</span><span class="ef249"> + b;</span>
 <span class="ef246">   ·</span>                       <span class="f3">┬</span>
 <span class="ef246">   ·</span>                       <span class="f3">╰──</span> The variable is referenced here.
<span class="ef246">────╯</span>
</pre>

### Scoped loop statements

The only way to jump back is with the `loop` statement.

```penne
fn foo() -> i32
{
    var x = 0;

    {
        x = x + 1;
        loop;
    }

    return: x // This line is never reached.
}
```

In order to keep backward jumps isolated and easy to find, `loop` can only appears as the last statement in a block.

### Views

Function arguments other than pointers (see below), primitives and words are passed as a view. For arrays this means an array view (or "slice") is created and passed into the function. Array views remember the length of their array, which can be accessed with the length operation `|x|`.

```penne
fn foo()
{
    var data: [4]i32 = [1, 2, 3, 4];
    var total = sum(data);
}

fn sum(x: []i32) -> i32
{
    var total = 0;
    var i = 0;
    {
        if i == |x|
            goto return;
        total = total + x[i];
        i = i + 1;
        loop;
    }
    return: total
}
```

### Reference pointers

Views allow you to pass a large value by reference, but they only give immutable access. For mutable access, a pointer is needed. They can be created by taking the address of a value. Unlike in most other languages, reference pointers in Penne automatically dereference to their base type, which is any type that isn't a reference pointer.

```penne
    var x: i32 = 17;
    var a: &i32 = &x;
    var y: i32 = a;
    a = 30;
    // Now x == 30 and y == 17.
```

To change which value a reference pointer points to, you need to explicitly modify the address.

```penne
    var x: i32 = 17;
    var y: i32 = 30;
    var z: i32 = 88;
    var a: &i32 = &x;
    &a = &y;
    // Now a points to y instead of x.
    var b: &i32 = &z;
    &a = &b;
    // Now a and b both point to z.
```

Reference pointers allow a function to modify its arguments, but require the caller to explicitly pass in an address.

```penne
fn foo()
{
    var data: [4]i32 = [1, 2, 3, 4];
    set_to_zero(&data);
}

fn set_to_zero(x: &[]i32)
{
    var i = 0;
    {
        if i == |x|
            goto end;
        x[i] = 0;
        i = i + 1;
        loop;
    }
    end:
}
```

### Structs and words

Like arrays, structural types declared with the `struct` keyword are implicitly passed as a view and cannot be used as the return value of a function. Fixed size structures, declared with `word8`, `word16`, `word32`, `word64` or `word128`, are passed by value.

### Imports

The `import` keyword is used to import all function signatures, structures and constants marked `pub` from a source file into the destination file. Imports are themselves not public and hence are not re-imported.

### Interoperability with C

Functions marked `extern` use the C ABI, which means it is possible (though not necessarily safe) to call them from C code compiled by LLVM. Conversely, declaring a function header such as

```penne
extern fn foo(buffer: []u8, length: usize);
```

allows you to call a C function from Penne code. Interacting with other programming languages that utilize or support the C ABI, such as C++, Rust, Zig or WebAssembly, is also possible.

Only array views, pointers and the primitive types `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64` and `usize` are allowed in the signature of an `extern` function. Array views in `extern` functions correspond to (const) pointers in C, do not have a length (`|x|`) and must not be null.
In a future version of Penne, pointers will also be assumed to be non-null and an "optional" type must be used to mark nullable pointers.

Structures and constants can also be declared `extern`, but as of v0.3.0 this has no effect.
