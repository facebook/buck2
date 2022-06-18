<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# builtins



### Members

| Member | Type | Description |
|--------|------|-------------|
| abs | `(i32) -> i32` |  |
| all | `(Value < 'v >) -> bool` | [all]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#all ): returns true if all values in the iterable object have a truth value of true. |
| any | `(Value < 'v >) -> bool` | [any]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#any ): returns true if any value in the iterable object have a truth value of true. |
| bool | `(Option < Value >) -> bool` | [bool]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#bool ): returns the truth value of any starlark value. |
| breakpoint | `() -> NoneType` |  |
| chr | `(Value) -> String` | [chr]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#bool ): returns a string encoding a codepoint. |
| debug | `(Value) -> String` | Print the value with full debug formatting. The result may not be stable over time, mostly intended for debugging purposes. |
| dedupe | `(Value < 'v >) -> Value < 'v >` | Remove duplicates in a list. Uses identity of value (pointer), rather than by equality. |
| dict | `() -> Dict < 'v >` | [dict]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#dict ): creates a dictionary. |
| dir | `(Value) -> Vec < String >` | [dir]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#dir ): list attributes of a value. |
| enum | `(*Vec < Value < 'v > >) -> Value < 'v >` |  |
| enumerate | `(Value < 'v >, i32) -> Value < 'v >` | [enumerate]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#enumerate ): return a list of (index, element) from an iterable. |
| experimental_regex | `(& str) -> StarlarkRegex` | Creates a regex which can be used for matching |
| fail | `(*Vec < Value >) -> NoneType` | fail: fail the execution |
| field | `(Value < 'v >, Option < Value < 'v > >) -> Field < 'v >` | Creates a field record. |
| filter | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |
| float | `(Option < Value >) -> f64` | [float]( https://github.com/google/skylark/blob/a5f7082aabed29c0e429c722292c66ec8ecf9591/doc/spec.md#float ): interprets its argument as a floating-point number. |
| getattr | `(Value < 'v >, & str, Option < Value < 'v > >) -> Value < 'v >` | [getattr]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#getattr ): returns the value of an attribute |
| hasattr | `(Value, & str) -> bool` | [hasattr]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#hasattr ): test if an object has an attribute |
| hash | `(& str) -> i32` | [hash]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#hash ): returns the hash number of a value. |
| int | `(Option < Value < 'v > >, Option < Value < 'v > >) -> Value < 'v >` | [int]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#int ): convert a value to integer. |
| len | `(Value) -> i32` | [len]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#len ): get the length of a sequence |
| list | `(Option < Value < 'v > >) -> Value < 'v >` | [list]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#list ): construct a list. |
| map | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |
| max | `(*Vec < Value < 'v > >, Option < Value < 'v > >) -> Value < 'v >` | [max]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#max ): returns the maximum of a sequence. |
| min | `(*Vec < Value < 'v > >, Option < Value < 'v > >) -> Value < 'v >` | [min]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#min ): returns the minimum of a sequence. |
| ord | `(Value) -> i32` | [ord]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.mdord ): returns the codepoint of a character |
| partial | `(Value < 'v >, *Value < 'v >, **DictRef < 'v >) -> Partial < 'v >` |  |
| pprint | `(*Vec < Value >) -> NoneType` |  |
| print | `(*Vec < Value >) -> NoneType` |  |
| range | `(i32, Option < i32 >, i32) -> Range` | [range]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#range ): return a range of integers |
| record | `(**SmallMap < String, Value < 'v > >) -> RecordType < 'v >` |  |
| repr | `(Value < 'v >) -> StringValue < 'v >` | [repr]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#repr ): formats its argument as a string. |
| reversed | `(Value < 'v >) -> Value < 'v >` | [reversed]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#reversed ): reverse a sequence |
| sorted | `(Value < 'v >, Option < Value < 'v > >, Option < Value < 'v > >) -> Value < 'v >` | [sorted]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#sorted ): sort a sequence |
| str | `(Value < 'v >) -> StringValue < 'v >` | [str]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#str ): formats its argument as a string. |
| struct | `() -> Struct < 'v >` |  |
| tuple | `(Option < Value < 'v > >) -> Value < 'v >` | [tuple]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#tuple ): returns a tuple containing the elements of the iterable x. |
| type | `(Value) -> Value < 'v >` | [type]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#type ): returns a string describing the type of its operand. |
| zip | `(*Vec < Value < 'v > >) -> Value < 'v >` | [zip]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#zip ): zip several iterables together |


## abs

```python
def abs(x: i32) -> i32
```

---
## all

```python
def all(x: Value < 'v >) -> bool
```

[all]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#all ): returns true if all values in the iterable object have a truth value of true.

### Details

Examples:

```
all([1, True]) == True
all([1, 1]) == True
all([0, 1, True]) == False
all([True, 1, True]) == True
all([0, 0]) == False
all([0, False]) == False
all([True, 0]) == False
all([1, False]) == False
```

---
## any

```python
def any(x: Value < 'v >) -> bool
```

[any]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#any ): returns true if any value in the iterable object have a truth value of true.

### Details

Examples:

```
any([0, True]) == True
any([0, 1]) == True
any([0, 1, True]) == True
any([0, 0]) == False
any([0, False]) == False
```

---
## bool

```python
def bool(x: Option < Value > = None) -> bool
```

[bool]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#bool ): returns the truth value of any starlark value.

### Details

Examples:

```
bool() == False
bool([]) == False
bool([1]) == True
bool(True) == True
bool(False) == False
bool(None) == False
bool(bool) == True
bool(1) == True
bool(0) == False
bool({}) == False
bool({1:2}) == True
bool(()) == False
bool((1,)) == True
bool("") == False
bool("1") == True
```

---
## breakpoint

```python
def breakpoint() -> NoneType
```

---
## chr

```python
def chr(i: Value) -> String
```

[chr]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#bool ): returns a string encoding a codepoint.

### Details

`chr(i)` returns a returns a string that encodes the single Unicode code
point whose value is specified by the integer `i`. `chr` fails
unless `0 ≤ i ≤ 0x10FFFF`.

Examples:

```
chr(65) == 'A'
chr(1049) == 'Й'
chr(0x1F63F) == '😿'
```

---
## debug

```python
def debug(val: Value) -> String
```

Print the value with full debug formatting. The result may not be stable over time, mostly intended for debugging purposes.

---
## dedupe

```python
def dedupe(val: Value < 'v >) -> Value < 'v >
```

Remove duplicates in a list. Uses identity of value (pointer), rather than by equality.

---
## dict

```python
def dict() -> Dict < 'v >
```

[dict]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#dict ): creates a dictionary.

### Details

`dict` creates a dictionary. It accepts up to one positional argument,
which is interpreted as an iterable of two-element sequences
(pairs), each specifying a key/value pair in the
resulting dictionary.

`dict` also accepts any number of keyword arguments, each of which
specifies a key/value pair in the resulting dictionary; each keyword
is treated as a string.

Examples:

```
dict() == {}
dict(**{'a': 1}) == {'a': 1}
dict([(1, 2), (3, 4)]) == {1: 2, 3: 4}
dict([(1, 2), ['a', 'b']]) == {1: 2, 'a': 'b'}
dict(one=1, two=2) == {'one': 1, 'two': 2}
dict([(1, 2)], x=3) == {1: 2, 'x': 3}
dict([('x', 2)], x=3) == {'x': 3}
x = {'a': 1}
y = dict([('x', 2)], **x)
x == {'a': 1} and y == {'x': 2, 'a': 1}
```

---
## dir

```python
def dir(x: Value) -> Vec < String >
```

[dir]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#dir ): list attributes of a value.

### Details

`dir(x)` returns a list of the names of the attributes (fields and
methods) of its operand. The attributes of a value `x` are the names
`f` such that `x.f` is a valid expression.

Examples:

```
"capitalize" in dir("abc")
```

---
## enum

```python
def enum(*args: Vec < Value < 'v > >) -> Value < 'v >
```

---
## enumerate

```python
def enumerate(it: Value < 'v >, start: i32 = None) -> Value < 'v >
```

[enumerate]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#enumerate ): return a list of (index, element) from an iterable.

### Details

`enumerate(x)` returns a list of `(index, value)` pairs, each containing
successive values of the iterable sequence and the index of the
value within the sequence.

The optional second parameter, `start`, specifies an integer value to
add to each index.

Examples:

```
enumerate(["zero", "one", "two"]) == [(0, "zero"), (1, "one"), (2, "two")]
enumerate(["one", "two"], 1) == [(1, "one"), (2, "two")]
```

---
## experimental_regex

```python
def experimental_regex(regex: & str) -> StarlarkRegex
```

Creates a regex which can be used for matching

---
## fail

```python
def fail(*args: Vec < Value >) -> NoneType
```

fail: fail the execution

### Details

Examples:
```
fail("this is an error")  # fail: this is an error
fail("oops", 1, False)  # fail: oops 1 False
```

---
## field

```python
def field(typ: Value < 'v >, default: Option < Value < 'v > > = None) -> Field < 'v >
```

Creates a field record.

### Details

Examples:

```
rec_type = record(host=field(str.type), port=field(int.type), mask=field(int.type, default=255))
rec = rec_type(host="localhost", port=80)
rec.port == 80
rec.mask == 255
```

---
## filter

```python
def filter(func: Value < 'v >, seq: Value < 'v >) -> Value < 'v >
```

---
## float

```python
def float(a: Option < Value > = None) -> f64
```

[float]( https://github.com/google/skylark/blob/a5f7082aabed29c0e429c722292c66ec8ecf9591/doc/spec.md#float ): interprets its argument as a floating-point number.

### Details

If x is a `float`, the result is x.
if x is an `int`, the result is the nearest floating point value to x.
If x is a string, the string is interpreted as a floating-point literal.
With no arguments, `float()` returns `0.0`.

```
float() == 0.0
float(1) == 1.0
float('1') == 1.0
float('1.0') == 1.0
float('.25') == 0.25
float('1e2') == 100.0
float(False) == 0.0
float(True) == 1.0
float("hello")   # error: not a valid number
float([])   # error: argument must be a string, a number, or a boolean
```

---
## getattr

```python
def getattr(a: Value < 'v >, attr: & str, default: Option < Value < 'v > > = None) -> Value < 'v >
```

[getattr]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#getattr ): returns the value of an attribute

### Details

`getattr(x, name)` returns the value of the attribute (field or method)
of x named `name`. It is a dynamic error if x has no such attribute.

`getattr(x, "f")` is equivalent to `x.f`.

```
getattr("banana", "split")("a") == ["b", "n", "n", ""] # equivalent to "banana".split("a")
```

---
## hasattr

```python
def hasattr(a: Value, attr: & str) -> bool
```

[hasattr]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#hasattr ): test if an object has an attribute

### Details

`hasattr(x, name)` reports whether x has an attribute (field or method)
named `name`.

---
## hash

```python
def hash(a: & str) -> i32
```

[hash]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#hash ): returns the hash number of a value.

### Details

`hash(x)`` returns an integer hash value for x such that `x == y`
implies `hash(x) == hash(y)``.

`hash` fails if x, or any value upon which its hash depends, is
unhashable.

```
hash("hello") != hash("world")
```

---
## int

```python
def int(a: Option < Value < 'v > > = None, base: Option < Value < 'v > > = None) -> Value < 'v >
```

[int]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#int ): convert a value to integer.

### Details

`int(x[, base])` interprets its argument as an integer.

If x is an `int`, the result is x.
If x is a `float`, the result is the integer value nearest to x,
truncating towards zero; it is an error if x is not finite (`NaN`,
`+Inf`, `-Inf`).
If x is a `bool`, the result is 0 for `False` or 1 for `True`.

If x is a string, it is interpreted like a string literal;
an optional base prefix (`0`, `0b`, `0B`, `0x`, `0X`) determines which
base to use. The string may specify an arbitrarily large integer,
whereas true integer literals are restricted to 64 bits.
If a non-zero `base` argument is provided, the string is interpreted
in that base and no base prefix is permitted; the base argument may
specified by name.

`int()` with no arguments returns 0.

```
int() == 0
int(1) == 1
int(False) == 0
int(True) == 1
int('1') == 1
int('16') == 16
int('16', 10) == 16
int('16', 8) == 14
int('16', 16) == 22
int(0.0) == 0
int(3.14) == 3
int(-12345.6789) == -12345
int(2e9) == 2000000000
int("hello")   # error: not a valid number
int(1e100)   # error: overflow
int(float("nan"))   # error: cannot convert NaN to int
int(float("inf"))   # error: cannot convert infinity to int
```

---
## len

```python
def len(a: Value) -> i32
```

[len]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#len ): get the length of a sequence

### Details

`len(x)` returns the number of elements in its argument.

It is a dynamic error if its argument is not a sequence.

```
len(()) == 0
len({}) == 0
len([]) == 0
len([1]) == 1
len([1,2]) == 2
len({'16': 10}) == 1
len(True)    # error: not supported
```

---
## list

```python
def list(a: Option < Value < 'v > > = None) -> Value < 'v >
```

[list]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#list ): construct a list.

### Details

`list(x)` returns a new list containing the elements of the
iterable sequence x.

With no argument, `list()` returns a new empty list.

```
list()        == []
list((1,2,3)) == [1, 2, 3]
list("strings are not iterable") # error: not supported
```

---
## map

```python
def map(func: Value < 'v >, seq: Value < 'v >) -> Value < 'v >
```

---
## max

```python
def max(*args: Vec < Value < 'v > >, key: Option < Value < 'v > > = None) -> Value < 'v >
```

[max]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#max ): returns the maximum of a sequence.

### Details

`max(x)` returns the greatest element in the iterable sequence x.

It is an error if any element does not support ordered comparison,
or if the sequence is empty.

The optional named parameter `key` specifies a function to be applied
to each element prior to comparison.

```
max([3, 1, 4, 1, 5, 9])               == 9
max("two", "three", "four")           == "two"    # the lexicographically greatest
max("two", "three", "four", key=len)  == "three"  # the longest
```

---
## min

```python
def min(*args: Vec < Value < 'v > >, key: Option < Value < 'v > > = None) -> Value < 'v >
```

[min]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#min ): returns the minimum of a sequence.

### Details

`min(x)` returns the least element in the iterable sequence x.

It is an error if any element does not support ordered comparison,
or if the sequence is empty.

```
min([3, 1, 4, 1, 5, 9])                 == 1
min("two", "three", "four")             == "four"  # the lexicographically least
min("two", "three", "four", key=len)    == "two"   # the shortest
```

---
## ord

```python
def ord(a: Value) -> i32
```

[ord]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.mdord ): returns the codepoint of a character

### Details

`ord(s)` returns the integer value of the sole Unicode code point
encoded by the string `s`.

If `s` does not encode exactly one Unicode code point, `ord` fails.
Each invalid code within the string is treated as if it encodes the
Unicode replacement character, U+FFFD.

Example:

```
ord("A")                                == 65
ord("Й")                                == 1049
ord("😿")                               == 0x1F63F
```

---
## partial

```python
def partial(func: Value < 'v >, *args: Value < 'v >, **kwargs: DictRef < 'v >) -> Partial < 'v >
```

---
## pprint

```python
def pprint(*args: Vec < Value >) -> NoneType
```

---
## print

```python
def print(*args: Vec < Value >) -> NoneType
```

---
## range

```python
def range(a1: i32, a2: Option < i32 > = None, step: i32 = None) -> Range
```

[range]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#range ): return a range of integers

### Details

`range` returns a tuple of integers defined by the specified interval
and stride.

```python
range(stop)                             # equivalent to range(0, stop)
range(start, stop)                      # equivalent to range(start, stop, 1)
range(start, stop, step)
```

`range` requires between one and three integer arguments.
With one argument, `range(stop)` returns the ascending sequence of
non-negative integers less than `stop`.
With two arguments, `range(start, stop)` returns only integers not less
than `start`.

With three arguments, `range(start, stop, step)` returns integers
formed by successively adding `step` to `start` until the value meets or
passes `stop`. A call to `range` fails if the value of `step` is
zero.

```
list(range(10))                         == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
list(range(3, 10))                      == [3, 4, 5, 6, 7, 8, 9]
list(range(3, 10, 2))                   == [3, 5, 7, 9]
list(range(10, 3, -2))                  == [10, 8, 6, 4]
```

---
## record

```python
def record(**kwargs: SmallMap < String, Value < 'v > >) -> RecordType < 'v >
```

---
## repr

```python
def repr(a: Value < 'v >) -> StringValue < 'v >
```

[repr]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#repr ): formats its argument as a string.

### Details

All strings in the result are double-quoted.

```
repr(1)                 == '1'
repr("x")               == "\"x\""
repr([1, "x"])          == "[1, \"x\"]"
repr("test \"'")        == "\"test \\\"'\""
repr("x\"y😿 \\'")      == "\"x\\\"y\\U0001f63f \\\\'\""
"#);
```

---
## reversed

```python
def reversed(a: Value < 'v >) -> Value < 'v >
```

[reversed]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#reversed ): reverse a sequence

### Details

`reversed(x)` returns a new list containing the elements of the iterable
sequence x in reverse order.

```
reversed(['a', 'b', 'c'])              == ['c', 'b', 'a']
reversed(range(5))                     == [4, 3, 2, 1, 0]
reversed("stressed".elems())           == ["d", "e", "s", "s", "e", "r", "t", "s"]
reversed({"one": 1, "two": 2}.keys())  == ["two", "one"]
```

---
## sorted

```python
def sorted(x: Value < 'v >, key: Option < Value < 'v > > = None, reverse: Option < Value < 'v > > = None) -> Value < 'v >
```

[sorted]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#sorted ): sort a sequence

### Details

`sorted(x)` returns a new list containing the elements of the iterable
sequence x, in sorted order.  The sort algorithm is stable.

The optional named parameter `reverse`, if true, causes `sorted` to
return results in reverse sorted order.

The optional named parameter `key` specifies a function of one
argument to apply to obtain the value's sort key.
The default behavior is the identity function.

```
sorted([3, 1, 4, 1, 5, 9])                               == [1, 1, 3, 4, 5, 9]
sorted([3, 1, 4, 1, 5, 9], reverse=True)                 == [9, 5, 4, 3, 1, 1]
sorted(["two", "three", "four"], key=len)                == ["two", "four", "three"] # shortest to longest
sorted(["two", "three", "four"], key=len, reverse=True)  == ["three", "four", "two"] # longest to shortest
```

---
## str

```python
def str(a: Value < 'v >) -> StringValue < 'v >
```

[str]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#str ): formats its argument as a string.

### Details

If x is a string, the result is x (without quotation).
All other strings, such as elements of a list of strings, are
double-quoted.

```
str(1)                          == '1'
str("x")                        == 'x'
str([1, "x"])                   == "[1, \"x\"]"
```

---
## struct

```python
def struct() -> Struct < 'v >
```

---
## tuple

```python
def tuple(a: Option < Value < 'v > > = None) -> Value < 'v >
```

[tuple]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#tuple ): returns a tuple containing the elements of the iterable x.

### Details

With no arguments, `tuple()` returns the empty tuple.

```
tuple() == ()
tuple([1,2,3]) == (1, 2, 3)
```

---
## type

```python
def type(a: Value) -> Value < 'v >
```

[type]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#type ): returns a string describing the type of its operand.

### Details

```
type(None)              == "NoneType"
type(0)                 == "int"
type(1)                 == "int"
type(())                == "tuple"
type("hello")           == "string"
```

---
## zip

```python
def zip(*args: Vec < Value < 'v > >) -> Value < 'v >
```

[zip]( https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#zip ): zip several iterables together

### Details

`zip()` returns a new list of n-tuples formed from corresponding
elements of each of the n iterable sequences provided as arguments to
`zip`.  That is, the first tuple contains the first element of each of
the sequences, the second element contains the second element of each
of the sequences, and so on.  The result list is only as long as the
shortest of the input sequences.

```
zip()                           == []
zip(range(5))                   == [(0,), (1,), (2,), (3,), (4,)]
zip(range(5), "abc".elems())    == [(0, "a"), (1, "b"), (2, "c")]
```
