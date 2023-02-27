---
layout: default
title: Home
nav_order: 1
---

# The Penne Programming Language

Penne is an [esoteric programming language](https://esolangs.org/wiki/Esoteric_programming_language) that imagines a world where, instead of being ostracized for leading to so-called "spaghetti code", the humble `goto` statement became the dominant method of control flow, surpassing `for` loops and `switch` statements, and ultimately obviating the need for the invention of [RAII](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization) and object-oriented programming in general.
By applying modern sensibilities to the use of the `goto` statement instead of banishing it altogether, Penne seeks to bring about a rennaissance of pasta-oriented programming.

## A quick taste

Penne's general aesthetic is inspired by modern programming languages (in particular Rust), with the notable exception of labels and the `goto` statement, which are (at least syntactically) taken from C, and the `loop` statement.

```penne
// Calculate the number of Collatz steps needed to reach 1.
// The Collatz conjecture states that this function always terminates.
fn determine_collatz_number(start: i32) -> i32
{
    var x = start;
    var steps = 0;
    {
        if x == 1
            goto return;
        do_collatz_step(&x);
        steps = steps + 1;
        loop;
    }

    return: steps
}

// If x is even, divide it by 2. Otherwise calculate 3 * x + 1.
// Do this without division or modulo operators (for demonstrative purposes).
fn do_collatz_step(x: &i32)
{
    var y = x;
    {
        if y == 0
        {
            if y + y == x
            {
                x = y;
                goto end;
            }
            y = y + 1;
            loop;
        }
        else if y == 1
        {
            x = 3 * x + 1;
            goto end;
        }
        y = y - 2;
        loop;
    }
    end:
}
```

## Language features

Penne is characterized by:

* scoped goto statements;
* automatically dereferencing pointers;
* type inference;
* interoperability with C.

See [Language Features](features.html) for a detailed explanation.

### Non-features

Penne is an esoteric language, not a general purpose or systems programming language. Certain modern features that you or I may think essential for a good programming language in 2023 to have, are omitted. This is either because including them would contradict the premise of Penne (see above) or to simplify its implementation.
As such, the following are decidedly *not* features of Penne:

* classes;
* generics;
* iterators;
* support for pointers larger than 64 bits;
* a string type guaranteed to be UTF-8;
* memory safety of any kind.
