# Heaps and Heap References

## Heaps

In Starlark, there are three interesting heap-related points of interest:

- A `Heap` has `Value`'s allocated on it and cannot be cloned or shared.
- A `FrozenHeap` has `FrozenValue`'s allocated on it and cannot be cloned or
  shared.
- A `FrozenHeapRef` is a `FrozenHeap` that is now read-only and can now be
  cloned and shared.

A `FrozenHeapRef` keeps a heap alive. While you have a `FrozenValue`, it is
important that you have either the `FrozenHeap` itself, or more usually, a
`FrozenHeapRef` to it. A `FrozenHeap` may contains a set of `FrozenHeapRef`'s to
keep the `FrozenHeap`s it references alive.

## Heap Containers

Heaps are included in other data types:

- A `Module` contains a `Heap` (where normal values are allocated) and a
  `FrozenHeap` (stores references to other frozen heaps and has compilation
  constants allocated on it). The `Heap` portion is garbage collected. At the
  end, when you call `freeze`, `Value`'s referenced by name in the `Module` are
  moved to the `FrozenHeap` and then then `FrozenHeap` is sealed to produce a
  `FrozenHeapRef`.
- A `FrozenModule` contains a `FrozenHeapRef`.
- A `GlobalsBuilder` contains a `FrozenHeap` onto which values are allocated.
- A `Globals` contains a `FrozenHeapRef`.

## Heap References

It is important that when a `FrozenValue` X is referenced by a `Value` or
`FrozenValue` (for example, included in a list), the heap where X originates is
added as a reference to the heap where the new value is being created.

As a concrete example in pseudo-code:

```rust
let h1 = FrozenHeap::new();
let s = "test".alloc(h1);
let h1 : FrozenHeapRef = h1.into_ref();

let h2 = Heap::new();
h2.add_reference(h1);
vec![s].alloc(h2);
```

In the above code, the following steps are taken:

1. Create a `FrozenHeap` then allocate something in it.
1. Turn the heap into a reference.
1. Use the allocated value `s` from `h1` when constructing a value in `h2`.
1. For that to be legal, and for the heap `h1` to not disappear while it is
   being allocated, it is important to call `add_reference`.

Note that this API can only point at a `FrozenValue` from another heap, and only
after that heap has been turned into a reference, so it will not be allocated in
anymore. These restrictions are deliberate and mean that most programs only have
one 'active heap' at a time.

Following are some places where heap references are added by Starlark:

- Before evaluation is started, a reference is added to the `Globals` from the
  `Module`, so it can access the global functions.
- When evaluating a `load` statement, a reference is added to the `FrozenModule`
  that is being loaded.
- When freezing a module, the `FrozenHeap`, in the `Module`, is moved to the
  `FrozenModule`, preserving the references that were added.

## `OwnedFrozen`

When you get a value from a `FrozenModule`, it will be an
`OwnedFrozen<Value<'static>>`. This structure is a pair of a `FrozenHeapRef` and
a value, where the ref keeps the value alive. You can move that `OwnedFrozen`
into the value of a module with code such as:

```rust
fn move<'v>(from: &FrozenModule, to: &Module<'v>) {
    let x: OwnedFrozen<Value<'static>> = from.get_owned("value").unwrap();
    let v: Value<'v> = x.add_to_heap(to.heap());
    to.set("value", v);
}
```

The `'static` in the type is a placeholder, not a claim that the value lives
forever: the accessors hand the value back at whatever lifetime is being used to
brand the heap you are working with, and never at `'static`. See the `branding`
module for what that lifetime means.

In general, you use an `OwnedFrozen` in one of three ways:

- **Move it into a heap** - `add_to_heap` (or `add_to_frozen_heap`) adds the
  owning heap as a reference of the heap you pass, and hands the value back
  branded for that heap. This is what you want nearly all of the time.
- **Look at it in place** - `by_ref` runs a closure on the value at an
  unnameable brand, so nothing derived from it can escape the closure.
- **Borrow the owner instead of sharing it** - `as_ref` produces an
  `OwnedFrozenRef`, which uses the borrow of the `OwnedFrozen` as the brand
  rather than duping the heap `Arc`. `FrozenModule` offers `get_option_ref` for
  the same reason.
