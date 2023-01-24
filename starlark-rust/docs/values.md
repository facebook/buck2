# Value Representation

:::warning
Some of the information in this page is outdated. However, the explanation of the problem, and thought process behind it, remains useful. Of particular note is that a garbage collected heap is now used for `Value`.
:::

This page explains how values are represented in the Starlark interpreter, ignoring some incidental details.

Importantly, in Starlark, any identifiers from modules that you import are 'frozen', which means that, if you have a module that defines a list, then once you have imported the module, the list is now immutable. This design means that you can safely share imports with multiple users, without any expensive copying, and use the imports in parallel.

## Frozen vs unfrozen values

Values that are frozen are segregated from those that are not:

* Frozen values are those you import, and (assuming no GC) are to be ref-counted atomically (so they can be shared by multiple threads) and never changed.
* Unfrozen values are those which are local to the module, and, since modules execute single threaded, can be non-atomically ref-counted and mutated.

Once a module has finished executing, it's values are frozen and can be reused freely.

## Thaw-on-write

It's not uncommon to return list literals from functions.

For example:

```python
def my_list(x):
   return ([1,2,3], x)
```

This above code returns the unfrozen list `[1,2,3]`. But while the list is unfrozen, and could be mutated by the caller, it probably won't be. To optimise this pattern, construct a frozen list when compiling `my_list` and insert a shared reference to it in the result. If anyone tries to mutate the list, it's explicitly unfrozen by copying it into a mutable variant (known as thawing the value).

## Immutable containers of mutable data

There are some data types (such as functions and tuples) that are themselves immutable but contain mutable data. Importantly, all types that can be invoked as functions (for example, `lambda`, `def`, and `a.b()`) fall into this category. These types can be non-atomically ref-counted but can't be mutated.

## Implementation in Rust

Putting all these above concepts together, results in the following:

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

In the above code, both of the traits `dyn SimpleValue` `and dyn ComplexValue` enable you to convert to the other and have shared general value-like methods.

There are four types of value:

* `Immutable`
* `Pseudo` - immutable containers of mutable values.
* `Mutable`/`Mutable`
* `Mutable`/`ThawOnWrite` - immutable now but can be replaced with `Mutable`/`Mutable` if needed.

There are two root types:

* `FrozenValue` - imported.
* `Value` - defined locally.
