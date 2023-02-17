<div align="center">

![Penne logo](docs/assets/images/penne_logo_header.png)

# The Penne Programming Language

[![Latest](https://img.shields.io/github/v/release/SLiV9/penne)](https://github.com/SLiV9/penne/releases/latest)
[![Crate](https://img.shields.io/crates/v/penne.svg?colorB=319e8c)](https://crates.io/crates/penne)
[![License](https://img.shields.io/github/license/SLiV9/penne)](https://github.com/SLiV9/penne/blob/main/LICENSE)
[![Stars](https://img.shields.io/github/stars/SLiV9/penne)](https://github.com/SLiV9/penne/stargazers)
[![GitHub repo size](https://img.shields.io/github/repo-size/SLiV9/penne)](https://github.com/SLiV9/penne/releases)
[![Lines of code](https://img.shields.io/tokei/lines/github/SLiV9/penne)](https://github.com/SLiV9/penne/tree/main/src)

[![Build](https://github.com/SLiV9/penne/actions/workflows/build.yml/badge.svg?event=push)](https://github.com/SLiV9/penne/actions)
[![Release](https://github.com/SLiV9/penne/actions/workflows/release.yml/badge.svg?event=push)](https://github.com/SLiV9/penne/actions/workflows/release.yml)
[![Tests](https://img.shields.io/testspace/tests/SLiV9/SLiV9:penne/main)](https://sliv9.testspace.com/spaces/204870/current)
[![Coverage](https://img.shields.io/coveralls/github/SLiV9/penne)](https://coveralls.io/github/SLiV9/penne)
[![Issues](https://img.shields.io/github/issues/SLiV9/penne)](https://github.com/SLiV9/penne/issues)

</div>

Penne is an [esoteric programming language](https://esolangs.org/wiki/Esoteric_programming_language) that imagines a world where, instead of being ostracized for leading to so-called "spaghetti code", the humble `goto` statement became the dominant method of control flow, surpassing `for` loops and `switch` statements, and ultimately obviating the need for the invention of [RAII](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization) and object-oriented programming in general.
By applying modern sensibilities to the use of the `goto` statement instead of banishing it altogether, Penne seeks to bring about a rennaissance of pasta-oriented programming.

## A quick taste

Penne's general aesthetic is inspired by modern programming languages (in particular Rust), with the notable exception of labels and the `goto` statement, which are (at least syntactically) taken from C, and the `loop` statement.

```
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

## Usage

The compiler uses LLVM as its backend. It requires LLVM version 6.0 or newer to be installed.

```shell
# Install it (requires Rust 1.60.0 or newer):
cargo install penne

# Compile a source file:
penne examples/addition.pn

# Or run it directly (using lli):
penne run examples/addition.pn
# Output: 10
```

There are [precompiled binaries](https://github.com/SLiV9/penne/releases/latest) for Ubuntu 20.04.

## Language features

A brief overview of the more unique language features of Penne:

### `goto` and `loop`

In Penne, `goto` is a local forward-only jump. This is achieved by giving labels a reverse scope: similar to how variables cannot be referenced before they are declared, labels cannot be jumped to _after_ they are declared.

```
fn foo() -> i32
{
	var x = 0;
	goto end;
	x = 10; // this line is not executed
	end:
	x = x + 1;
	return: x
}
```

This function returns 1, not 11.

The only way to jump back is with the `loop` block:

```
fn foo() -> i32
{
	var x = 0;

	{
		x = x + 1;
		loop;
	}

	return: x
}
```

Here the line `x = x + 1` is executed forever and the end of the function is never reached.

### Views

Function arguments other than pointers (see below), primitives and words are passed as a view. For arrays this means an array view (or "slice") is created and passed into the function. Array views remember the length of their array, which can be accessed with the length operation `|x|`.

```
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

```
	var x: i32 = 17;
	var a: &i32 = &x;
	var y: i32 = a;
	a = 30;
	// Now x == 30 and y == 17.
```

To change which value a reference pointer points to, you need to explicitly modify the address.

```
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

```
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

Like arrays, structural types declared with the `struct` keyword are implicitly passed as a view and cannot be used as the return value of a function. However fixed size structures, declared with `word8`, `word16`, `word32`, `word64` or `word128`, are passed by value.

### Imports

The `import` keyword is used to import all function signatures, structures and constants marked `pub` from a source file into the destination file. Imports are themselves not public and hence are not re-imported.

### Interoperability with C

Functions marked `extern` use the C ABI, which means it is possible (though not necessarily safe) to call them from C code compiled by LLVM. Conversely, declaring a function header such as

```
    extern fn foo(buffer: []u8, length: usize);
```

allows you to call a C function from Penne code. Interacting with other programming languages that utilize or support the C ABI, such as C++, Rust, Zig or WebAssembly, is also possible.

Only array views, pointers and the primitive types `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64` and `usize` are allowed in the signature of an `extern` function. Array views in `extern` functions correspond to (const) pointers in C, do not have a length (`|x|`) and must not be null.
In a future version of Penne, pointers will also be assumed to be non-null and an "optional" type must be used to mark nullable pointers.

Structures and constants can also be declared `extern`, but as of v0.3.0 this has no effect.

## Non-features

Penne is an esoteric language, not a general purpose or systems programming language. Certain modern features that you or I may think essential for a good programming language in 2023 to have, are omitted. This is either because including them would contradict the premise of Penne (see above) or to simplify its implementation.
As such, the following are decidedly *not* features of Penne:

* classes;
* generics;
* iterators;
* support for pointers larger than 64 bits;
* a string type guaranteed to be UTF-8;
* memory safety of any kind.

## Contributing

Penne and its compiler are still in development, and many language features (enums, modules) are yet to be implemented. However it is my intention for any gaps in functionality to raise a proper error message until they are implemented. If you encounter a compiler segfault or panic, or if the compiler generates invalid LLVM IR, opening an issue with a minimal reproducible example would be much appreciated.

## License

This library was created by Sander in 't Veld.
It is made available to you under the MIT License,
as specified in `LICENSE.txt`.

The software is provided "as is", without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose and noninfringement. In no event shall the authors or copyright holders be liable for any claim, damages or other liability, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or the use or other dealings in the software.
