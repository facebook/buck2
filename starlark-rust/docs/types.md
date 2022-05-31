# Starlark Types

We have extended Starlark with types. This extension is highly experimental and likely to be modified in future, as we gain experience with it.

Types can be added to function arguments, or function return types, for example:

```python
def fib(i: int.type) -> int.type:
    ...
```

These types are checked _at runtime_ - there is currently no static checking or linting for them. The rest of this document lays out what types mean, and what type-supporting objects have been written using them.

## What does a type mean?

A type is just an arbitrary expression that evaluates to a value. That value is then treated as a type, which is matched against values. To break that down:

* When we call `fib(3)` the _value_ `3` is passed to `fib` as parameter `i`.
* When we start executing `fib`, we evaluate the _expression_ `int.type` to the _value_ `"int"`.
* We then check that the value `3` matches the type represented by `"int"`.

If the value doesn't match it is a runtime error. Similarly, on `return` statements or the end of the function we check the result type matches `int.type`.

Types match using the following rules:

* The type `""` means anything.
* The type `"foo"` means any value of type `foo`, where the type of `x` is computed by doing `type(x)`. That means that `"int"`, `"bool"` and `"string"` are common types.
* Most constructor functions provide a `.type` property to obtain the type they produce, allowing `int.type`, `bool.type` and `str.type` etc.
* Any string starting with an underscore `_`, e.g. `"_a"` means anything - but the name is often used as a hint to say where types go in polymorhpic functions.
* The type `None` means the result must be `None`.
* The singleton list `[t]` means a list where each element must be of type `t`. If you want a list of any types, use `[""]`.
* Multiple element lists `[t1,t2]` are OR types, where the value must be either type `t1` OR type `t2`.
* A tuple `(t1, t2, t3)` matches tuples of the same length (3 in this case), where each element of the value must match the corresponding element of the tuple.
* A singleton dictionary `{k: v}` means a dictionary where all the keys have type `k`, and all the values have type `v`.
* It is possible to define functions that return types, e.g. `def StrDict(t): return {str.type: t}` would mean `StrDict(int.type)` was a valid type.

The goals of this type system are:

* Reuse the existing machinery of Starlark as much as possible, avoiding inventing a special class of type values. As a consequence, any optimisations for values like string/list are reused.
* Provide a pleasing syntax.
* Some degree of compatibilty with Python, which allows types as expressions in the same places we allow them (but with different meaning and different checking).
* And finally, a non-goal is to provide a complete type system capable of representing every type invariant - it's intended to be a lossy approximation.

In addition to these built-in types, we provide records and enumerations as special concepts.

## Record types

We provides a `record` type, representing a set of named values, each with their own type. For example:

```python
MyRecord = record(host=str.type, port=int.type)
```

This statement defines a record `MyRecord` with 2 fields, the first named `host` which must be of type `str.type`, and the second named `port` which must be of type `int.type`. Now we have `MyRecord` we can:

* Create values of this type with `MyRecord(host="localhost", port=80)`. It is a runtime error if any arguments are missed, of the wrong type, or if any unexpected arguments are given.
* Get the type of the record suitable for a type annotation with `MyRecord.type`.
* Get the fields of the record, e.g. `v = MyRecord(host="localhost", port=80)` will provide `v.host == "localhost"` and `v.port == 80`. Similarly `dir(v) == ["host", "port"]`.

It is also possible to specify default values for parameters using the `field` function, for example:

```python
MyRecord = record(host=str.type, port=field(int.type, 80))
```

Now the `port` field can be omitted, defaulting to `80` is not present - e.g. `MyRecord(host="localhost").port == 80`.

Records are stored deduplicating their field names, making them more memory efficient than dictionaries.

## Enum types

We provide an `enum` type, representing one value picked from a set of values. For example:

```python
MyEnum = enum("option1", "option2", True)
```

This statement defines an enumeration `MyEnum` that consists of the three values `"option1"`, `"option2"` and `True`. Now we have `MyEnum`, we can:

* Create values of this type with `MyEnum("option2")`. It is a runtime error if the argument is not one of the predeclared values of the enumeration.
* Get the type of the enum suitable for a type annotation with `MyEnum.type`.
* Given a value of the enum, e.g. `v = MyEnum("option2")`, get the underlying value `v.value == "option2"` or the index in the enumeration `v.index = 1`.
* Get a list of the values that make up the array with `MyEnum.values() == ["option1", "option2", True]`.
* Treat `MyEnum` a bit like an array, with `len(MyEnum) == 3`, `MyEnum[1] == MyEnum("option2")` and iteration over enums `[x.value for x in MyEnum] == ["option1", "option2", True]`.

Enumeration types store each value once, which are then efficiently referenced by enumeration values.
