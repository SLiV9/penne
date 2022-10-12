# Penne

![Penne logo](assets/penne_logo_200x60.png)

Penne is an [esoteric programming language](https://esolangs.org/wiki/Esoteric_programming_language) that imagines a world where, instead of being ostracized for leading to so-called "spaghetti code", the humble `goto` statement became the dominant method of control flow, surpassing `for` loops and `switch` statements, and ultimately obviating the need for the invention of [RAII](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization) and object-oriented programming in general.
By applying modern sensibilities to the use of the `goto` statement instead of banishing it altogether, Penne seeks to bring about a rennaissance of pasta-oriented programming.

## A quick taste

Penne's general aesthetic is inspired by modern programming languages (in particular [Rust](https://www.rust-lang.org/)), with the notable exception of labels and the `goto` statement, which are (at least syntactically) taken from C, and the `loop` statement.

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

Function arguments other than pointers (see below) and primitives are passed as a view. For arrays this means an array view is created and passed into the function. Array views remember the length of their array, which can be accessed with the length operation `|x|`.

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

## Contributing

Penne and its compiler are still in development, and many language features (structs, enums, modules) are yet to be implemented. However it is my intention for any gaps in functionality to raise a proper error message until they are implemented. If you encounter a compiler segfault or panic (*not* a program segfault; memory safety is decidedly not a language feature), or if the compiler generates invalid LLVM IR, opening an issue with a minimal reproducible example would be much appreciated.

## License

This library was created by Sander in 't Veld.
It is made available to you under the MIT License,
as specified in `LICENSE.txt`.

The software is provided "as is", without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose and noninfringement. In no event shall the authors or copyright holders be liable for any claim, damages or other liability, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or the use or other dealings in the software.
