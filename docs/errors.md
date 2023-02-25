---
layout: default
title: Error Codes
nav_order: 10
---

{% annotatederrorreference %}

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

{% endannotatederrorreference %}
