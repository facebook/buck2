# Value Representation

_**Warning**: Some of the information in this document is outdated, but the explanation of the problem, and thought process behind it, remains useful. In particular, we now use a garbage collected heap for `Value`._

This document explains how values are represented in this Starlark interpreter, ignoring some incidental details.

Importantly, in Starlark, any identifiers from modules you import are "frozen". That means that if you have a module that defines a list, then once you have imported the module, the list is now immutable. This design means that you can safely share imports with multiple users, without any expensive copying, and use the imports in parallel.

## Frozen vs unfrozen values

We segregate values which are frozen from those which are not.

* Frozen values are those you import, and (assuming no GC) these want to be ref-counted atomically (so they can be shared by multiple threads) and never changed.
* Unfrozen values are those which are local to the module, and since modules execute single threaded, these can be non-atomically ref-counted and mutated.

Once we finish executing a module, we freeze all its values, and then reuse them freely.

## Thaw-on-write

It's not uncommon to return list literals from functions, for example:

```python
def my_list(x):
   return ([1,2,3], x)
```

This code returns the unfrozen list `[1,2,3]`. But while the list is unfrozen, and could be mutated by the caller, it probably won't be. To optimise this pattern, we can construct a frozen list when compiling `my_list`, and insert a shared reference to it in the result. If anyone tries to mutate the list, we explicitly unfreeze the list by copying it into a mutable variant (known as thawing the value).

## Immutable containers of mutable data

There are some data types, e.g. functions and tuples, which are themselves immutable but contain mutable data. Importantly, all types that can be invoked as functions (e.g. `lambda`, `def`, `a.b()`) fall into this category. These types can be non-atomically ref-counted, but can't be mutated.

## Implementation in Rust

Putting all these ideas together, we end up with:

```rust
enum FrozenValue {
    None(NoneType),
    Bool(bool),
    Int(i64),
    Obj(Arc<dyn StarlarkValue>),
}

enum Value {
    Immutable(FrozenValue),
    Pseudo(Rc<dyn ComplexValue>)
    Mutable(Rc<RefCell<Mutable>>),
}

enum Mutable {
    Mutable(Box<dyn ComplexValue>),
    ThawOnWrite(Arc<dyn StarlarkValue>),
}
```

We have the traits `dyn SimpleValue` `and dyn ComplexValue`, both of which let you convert to the other and have shared general value-like methods on. We have four types of value:

* `Immutable` values, which are immutable.
* `Pseudo` values, which are immutable containers of mutable values.
* `Mutable`/`Mutable` values, which are mutable.
* `Mutable`/`ThawOnWrite` values, which are immutable now, but can be replaced with `Mutable`/`Mutable` if needed.

We have two root types, `FrozenValue` and `Value`, corresponding respectively to values which we imported, and values which are defined locally.
