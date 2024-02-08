# Starlark Types

The Starlark 'types' extension is highly experimental and likely to be modified
in the future.

Types can be added to function arguments, or function return types.

For example:

```python
def fib(i: int) -> int:
    ...
```

There are moments where types can be checked:

1. At runtime, as a function is executed, when a value of the appropriate type
   is available.
2. Statically, without executing anything.
3. At compile time, when the definitions of all symbols imported using `load`
   are available.

Currently runtime is the normal way of checking, but other systems built on
Starlark (e.g. Buck2) may also perform additional types of checking. In all
cases the meaning of the types is the same.

The rest of this document lays out what types mean and what type-supporting
values are available (records and enums).

## What does a type mean?

A type is a Starlark expression that has a meaning as a type:

- When `fib(3)` is called, the _value_ `3` is passed to `fib` as parameter `i`.
- When the execution of `fib` is started, the _expression_ `int` is evaluated to
  the value of the `int` function.
- A check is then made that the value `3` matches the type represented by `int`.

If the value doesn't match, it is a runtime error. Similarly, on `return`
statements, or the end of the function, a check is made that result type matches
`int`.

As some examples of types:

- The type `typing.Any` matches any value, with no restrictions.
- The types `int`, `bool`, `str` all represent the values produced by the
  respective functions.
- The type `None` represents the value `None`.
- The type `list[int]` represents a list of `int` types, e.g. `list[typing.Any]`
  represents a list containing any types.
- The type `dict[int, bool]` represents a dictionary with `int` keys and `bool`
  values.
- The type `tuple[int, bool, str]` represents a tuple of arity 3 with components
  being `int`, `bool` and `str`.
- The type `tuple[int, ...]` represents a tuple of unknown arity where all the
  components are of type `int`.
- The type `int | bool` represents a value that is either an `int` or a `bool`.
- The type `typing.Callable` represents something that can be called as a
  function.
- The type `typing.Iterable` represents something that can be iterated on.
- The type `typing.Never` represents a type with no valid values - e.g. the
  result of `fail` is `typing.Never` as the return value of `fail` can never be
  observed, given the program terminates.

The goals of this type system are:

- Reuse the existing machinery of Starlark as much as possible, avoiding
  inventing a special class of type values. As a consequence, any optimisations
  for values like string/list are reused.
- Provide a pleasing syntax.
- Some degree of compatibility with Python, which allows types as expressions in
  the same places Buck2 allows them (but with different meaning and different
  checking).
- And finally, a non-goal is to provide a complete type system capable of
  representing every type invariant: it's intended to be a lossy approximation.

In addition to these built-in types, records and enumerations are provided as
special concepts.

## Record types

A `record` type represents a set of named values, each with their own type.

For example:

```python
MyRecord = record(host=str, port=int)
```

This above statement defines a record `MyRecord` with 2 fields, the first named
`host` that must be of type `str`, and the second named `port` that must be of
type `int`.

Now `MyRecord` is defined, it's possible to do the following:

- Create values of this type with `MyRecord(host="localhost", port=80)`. It is a
  runtime error if any arguments are missed, of the wrong type, or if any
  unexpected arguments are given.
- Get the type of the record suitable for a type annotation with `MyRecord`.
- Get the fields of the record. For example,
  `v = MyRecord(host="localhost", port=80)` will provide `v.host == "localhost"`
  and `v.port == 80`. Similarly, `dir(v) == ["host", "port"]`.

It is also possible to specify default values for parameters using the `field`
function.

For example:

```python
MyRecord = record(host=str, port=field(int, 80))
```

Now the `port` field can be omitted, defaulting to `80` is not present (for
example, `MyRecord(host="localhost").port == 80`).

Records are stored deduplicating their field names, making them more memory
efficient than dictionaries.

## Enum types

The `enum` type represents one value picked from a set of values.

For example:

```python
MyEnum = enum("option1", "option2", "option3")
```

This statement defines an enumeration `MyEnum` that consists of the three values
`"option1"`, `"option2"` and `"option3"`.

Now `MyEnum` is defined, it's possible to do the following:

- Create values of this type with `MyEnum("option2")`. It is a runtime error if
  the argument is not one of the predeclared values of the enumeration.
- Get the type of the enum suitable for a type annotation with `MyEnum`.
- Given a value of the enum (for example, `v = MyEnum("option2")`), get the
  underlying value `v.value == "option2"` or the index in the enumeration
  `v.index == 1`.
- Get a list of the values that make up the array with
  `MyEnum.values() == ["option1", "option2", "option3"]`.
- Treat `MyEnum` a bit like an array, with `len(MyEnum) == 3`,
  `MyEnum[1] == MyEnum("option2")` and iteration over enums
  `[x.value for x in MyEnum] == ["option1", "option2", "option3"]`.

Enumeration types store each value once, which are then efficiently referenced
by enumeration values.
