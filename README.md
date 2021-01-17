# Penne

Penne is an [esoteric programming language](https://esolangs.org/wiki/Esoteric_programming_language) that imagines a world where, instead of being ostracized for leading to so-called "spaghetti code", the humble `goto` statement became the dominant method of control flow, surpassing `for` loops and `switch` statements, and ultimately obviating the need for the invention of [RAII](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization) and object-oriented programming in general.
By applying modern sensibilities to the use of the `goto` statement instead of banishing it altogether, Penne seeks to bring about a rennaissance of pasta-oriented programming.

## A quick taste

Penne's general aesthetic is inspired by modern programming languages (in particular [Rust](https://www.rust-lang.org/)), with the notable exception of labels and the `goto` statement, which are (at least syntactically) taken from C, and the `loop` statement.

_For demonstrative purposes, the following sample does not use multiplication, division or modulo operators._
```
fn determine_collatz_number(x: i32) -> i32
{
	var steps = 0;
	{
		if x == 1
			goto return;
		var y = x;
		{
			if y == 0
			{
				// x is even, divide it by 2
				if y + y == x
				{
					x = y;
					goto next;
				}
				y = y + 1;
				loop;
			}
			else if y == 1
			{
				// x is odd, calculate 3 * x + 1
				x = x + x + x + 1;
				goto next;
			}
			y = y - 2;
			loop;
		}
		next:
		steps = steps + 1;
		loop;
	}

	return:
	steps
}
```
An equivalent implementation in C:
```c
int determine_collatz_number(int x)
{
	int steps = 0;
	while (x > 1)
	{
		for (int y = x; y > 1; y = y - 2)
		{
			if (y == 0)
			{
				// x is even, divide it by 2
				while (y + y != x)
				{
					y = y + 1;
				}
				x = y;
			}
			else
			{
				// x is odd, calculate 3 * x + 1
				x = x + x + x + 1;
			}
		}
		steps = steps + 1;
	}
	return steps;
}
```

## `goto` and `loop`

In Penne, `goto` is a local forward-only jump. This is achieved by giving labels a reverse scope: similar to how variables cannot be referenced before they are declared, labels cannot be jumped to _after_ they are declared.
```
fn foo() -> i32
{
	var x = 0;
	goto end;
	x = 10; // this line is not executed
	end:
	x = x + 1;
	return:
	x
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

	return:
	x
}
```
Here the line `x = x + 1` is executed forever and the end of the function is never reached.
