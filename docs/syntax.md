---
layout: default
title: Syntax Overview
nav_order: 11
---

## Syntax Overview

The following codesample displays a broad range of syntax.

```penne
/// Example doc-style comment.

const COLOR: u32 = 0xfb4934ff;
const BIG_NUMBER: i32 = 500_000_000;

const TRIPLE_BUFFER: [3]u32 = [128, 256, 512];

const DATA_HEIGHT: usize = 4;
const DATA: [DATA_HEIGHT]u8 = [
    0b00000000,
    0b11000011,
    0b10000001,
    0b00100100,
];

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

fn solve_all_our_problems()
{
    // Left as an exercise for the reader.
}

fn do_nothing()
{
    nothing:
}

word64 Position
{
    x: i32,
    y: i32,
}

fn sum_of_position(pos: Position) -> i32
{
    return: pos.x + pos.y
}

struct FourPositions
{
    positions: [4]Position,
}

pub extern fn something_with_pointers(u: &&i32)
{
    var i: i32 = 17;
    var x: &i32 = &i;
    var a: &&i32 = &&x;
    &&a = &&u;
    var y: &i32 = cast &x as &i32;
    var text = "Save up to \u{20ac}50 or \xA350 or more!\0";
}

fn something_with_casting()
{
    var a: bool = true;
    var data: []i16 = [80, 20];
    var result = a as u8 + |data| as u8;
}

fn main() -> i32
{
    var from = Position { x: 10, y: 10 };
    var four_positions = FourPositions {
        positions: [
            from,
            Position { x: 0, y: 0 },
            Position { x: from.x, y: 0 },
            Position { x: 0, y: from.y },
        ],
    };
    return: 0
}
```
