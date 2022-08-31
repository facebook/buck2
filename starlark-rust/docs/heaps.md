# Heaps and heap references

## The Heaps

In Starlark there are three interesting heap-like things:

* A `Heap` has `Value`'s allocated on it and cannot be cloned or shared.
* A `FrozenHeap` has `FrozenValue`'s allocated on it and cannot be cloned or shared.
* A `FrozenHeapRef` is a `FrozenHeap` that is now read-only and can now be cloned and shared.

A `FrozenHeapRef` keeps a heap alive. A `FrozenHeap` contains a set of `FrozenHeapRef`'s. While you have a `FrozenValue` it is important that you have either the `FrozenHeap` itself, or more usually, a `FrozenHeapRef` to it.

## Heap Containers

Heaps are included in other data types:

* A `Module` contains a `Heap` (where normal values are allocated) and a `FrozenHeap` (stores references to other frozen heaps and has compilation constants allocated on it). The `Heap` portion is garbage collected. At the end, when you call `freeze`, `Value`'s referenced by name in the `Module` are moved to the `FrozenHeap` and then then `FrozenHeap` is sealed to produce a `FrozenHeapRef`.
* A `FrozenModule` contains a `FrozenHeapRef`.
* A `GlobalsBuilder` contains a `FrozenHeap` which values are allocated on.
* A `Globals` contains a `FrozenHeapRef`.

## Heap References

It is important that when a `FrozenValue` X is referenced by a `Value` or `FrozenValue` (e.g. included in a list) that the heap where X originates is added as a reference to the heap where the new value is being created. As a concrete example in pseudo-code:

```rust
let h1 = FrozenHeap::new();
let s = "test".alloc(h1);
let h1 : FrozenHeapRef = h1.into_ref();

let h2 = Heap::new();
h2.add_reference(h1);
vec![s].alloc(h2);
```

Here we create a `FrozenHeap`, allocate something in it, and turn the heap into a reference. We then use the allocated value `s` from `h1` when constructing a value in `h2`. For that to be legal, and for the heap `h1` to not disappear while it is being allocated, it is important to call `add_reference`.

Note that this API means we can only point at a `FrozenValue` from another heap, and only after that heap has been turned into a reference, so it will not be allocated in anymore. These restrictions are deliberate and mean that most programs only have one "active heap" at a time.

Some places heap references are added by Starlark:

* Before we start evaluating, we add a reference to the `Globals` from the `Module`, so it can access the global functions.
* When evaluating a `load` statement we add a reference to the `FrozenModule` that is being loaded.
* When freezing a module we move the `FrozenHeap` in the `Module` to the `FrozenModule`, preserving the references that were added.

## `OwnedFrozenValue`

When you get a value from a `FrozenModule` it will be a `OwnedFrozenValue`. This structure is a pair of a `FrozenHeapRef` and a `FrozenValue`, where the ref keeps the value alive. You can move that `OwnedFrozenValue` into the value of a module with code such as:

```rust
fn move<'v>(from: &FrozenModule, to: &'v Module) {
    let x : OwnedFrozenValue = from.get("value").unwrap();
    let v : Value<'v> = x.owned_value(&to);
    to.set("value", v);
}
```

In general you can use the `OwnedFrozenValue` in one of three ways:

* Operate on it directly, with methods like `unpack_int` or `to_str`.
* Extract it safely, using methods like `owned_frozen_value`, which takes a `FrozenHeap` to which the heap reference is added, and returns a naked `FrozenValue`. After that, it is now safe for the `FrozenHeap` you passed in to use the `FrozenValue`. With `owned_value` there is lifetime checking that the right heap is passed, but with `FrozenValue`, there isn't. Be careful to pass the right heap, although given most programs only have one active heap at a time, it should mostly work out.
* Extract it unsafely, using methods `unchecked_frozen_value`, which gives you the underlying `FrozenValue` without adding any references. Be careful to make sure there is a good reason the `FrozenValue` remains valid.
