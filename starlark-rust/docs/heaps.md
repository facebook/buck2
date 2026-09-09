# Heaps and Heap References

Starlark values live in heaps, and a value is only usable while its heap is
alive. Rather than reference counting every value, Starlark tracks heaps, and
uses the lifetime parameter on `Value<'v>` to tie each value to the heap it
lives in. This page describes the heap types, how heaps keep each other alive,
and the APIs for moving values between them. The `branding` module in the
source (`values/layout/heap/branding.rs`) explains the lifetime discipline
itself.

## Heaps

There are three kinds of heap:

- A `Heap<'v>` is an unfrozen heap: values allocated on it are mutable and
  garbage collected. Each `Module` has one. It is only ever reached through a
  closure, `Heap::temp(|heap| ..)` or `Module::with_temp_heap(|module| ..)`, and
  `'v` names that closure; the handle is `Copy`.
- An `OwnedFrozenHeap` is a frozen heap under construction. It is owned, single
  threaded, and only allocates through the `FrozenHeap<'fh>` handle it hands out
  inside a closure: `OwnedFrozenHeap::with(|fh| ..)`, or `FrozenHeap::temp` for a
  throwaway heap. Values allocated through the handle are immutable and branded
  `'fh`, so they cannot leave the closure on their own.
- An `OwnedFrozen<()>` is a sealed frozen heap: read only, `Clone`,
  `Send + Sync`, and the thing that keeps a frozen heap's memory alive.
  `OwnedFrozenHeap::seal` produces one. An `OwnedFrozen<T>` is a sealed heap
  together with a `T` that it keeps alive.

The lifetimes `'v`, `'fh` and `'fv` do not measure how long anything lives.
They identify a heap: two values with the same brand live in the same heap, or
in heaps that heap keeps alive. A value can only be given a brand by the heap
that owns it, or by one of the APIs below that record the dependency between
the heaps.

## Getting values out of a frozen heap

Values allocated through a `FrozenHeap<'fh>` handle can only leave the closure
paired with the sealed heap that owns them, as an `OwnedFrozen<T>`:

```rust
let list: OwnedFrozen<Value<'static>> =
    OwnedFrozen::build(FrozenHeapName::user("example"), |fh| {
        fh.alloc(AllocList([fh.alloc("a"), fh.alloc("b")]))
    });
```

`OwnedFrozen::build` allocates on a fresh heap and seals it;
`OwnedFrozenHeap::seal_with` does the same on a heap you already hold. The
closure can only return values at the handle's brand, so the value and its
owner are paired by construction.

The `'static` in `OwnedFrozen<Value<'static>>` is a placeholder for the brand
of the sealed heap, which has no name. None of the accessors hand the value
back at `'static`:

- **Move it into a heap** - `add_to_heap(heap)` (or `add_to_frozen_heap`)
  records the owning heap as a reference of `heap` and hands the value back
  branded for `heap`. This is what you want nearly all of the time.
- **Look at it in place** - `by_ref` runs a closure on the value at a brand
  private to the closure, so nothing derived from it can escape;
  `by_ref_with_reconstructor` also provides an `OwnedFrozenReconstructor`,
  which can re-pair derived values with the owner or mint a `HeapEdge` (see
  below).
- **Transform it** - `map`, `try_map` and `maybe_map` produce an
  `OwnedFrozen<U>` from the same heap.
- **Borrow the owner instead of sharing it** - `as_ref` produces an
  `OwnedFrozenRef<'f, T>`, which uses the borrow of the `OwnedFrozen` as the
  brand and hands out the value directly through `value()`. It is `Copy` when
  `T` is and avoids touching the heap's reference count.

## Heap references

A heap can depend on sealed frozen heaps. `Heap::add_reference` and
`FrozenHeap::add_reference` take an `OwnedFrozenRef<'_, ()>`, the borrowed form
of a sealed heap, and keep that heap alive for as long as the referencing heap
is. The invariant everything relies on is:

> A value in heap A may point at a value in heap B only if A is B, or A
> references B (directly or through other heaps).

The safe APIs maintain this without the caller thinking about it. Every way of
obtaining a `Value<'v>` from a sealed heap adds the reference as a side effect:
`add_to_heap` does, and so does minting a `HeapEdge` from a reconstructor. The
three places that change a brand without an edge are listed at the end of this
page.

## Heap edges

A `HeapEdge<'v, 'dep>` is a witness that the heap `'v` keeps the heap `'dep`
alive. Its `rebrand` (and `rebrand_ref`) convert anything branded `'dep` to the
`'v` brand, including compound types: a `ValueTyped<'dep, Tuple<'dep>>`
becomes a `ValueTyped<'v, Tuple<'v>>`. Edges are `Copy` and zero sized. There
are three ways to get one:

- **The module edge.** `Module::frozen_heap(|fh, edge| ..)` (also on
  `Evaluator`) opens a scope on the module's own frozen heap and provides the
  edge from the module's value heap to it. Anything the closure allocates
  through `fh` reaches the module's `'v` through `edge.rebrand`. The compiler
  runs inside such a scope, which is where bytecode, constants and interned
  names live.
- **Reconstructor edges.** Inside `by_ref_with_reconstructor`, the
  `OwnedFrozenReconstructor` mints `edge(heap)` or `frozen_edge(fh)`, adding
  the owner as a reference of the given heap. This is how the compiler reads
  a `Globals` while allocating into the module's frozen heap.
- **The immortal edge.** `HeapEdge::immortal()` is the edge from every heap to
  the `'static` brand. Nothing needs to keep `'static` data alive, so it can be
  minted anywhere. Statics are brought to a brand by spelling `at()`:
  `AllocStaticSimple::at` and `ValueTyped::<'static, _>::at` (which covers
  `const_frozen_string!`) are this edge behind a name.

`HeapEdge::unchecked_new` is `unsafe` and crate-private; the minters above
(plus `HeapEdge::identity`, the edge from a heap to itself) are its only
callers.

## Heap containers

- A `Module<'v>` owns a `Heap<'v>` and an `OwnedFrozenHeap`, together, in a
  `ModuleHeaps`. Values are allocated on the heap; the frozen heap holds the
  compiler's products and anything a user allocates through
  `Module::frozen_heap`. The frozen heap is sealed when the module is frozen
  (whether freezing succeeds, fails or panics), and also when an unfrozen
  module is dropped; in both cases the sealed heap is added to the value heap's
  references, so `'v` values that came out of the frozen heap stay valid as
  long as the value heap does.
- A `FrozenModule` is an owned carrier: an `OwnedFrozen` of the module's data
  (its slots and extra value) allocated on the sealed heap. Every accessor is a
  projection of that owner: `get_owned` returns an
  `OwnedFrozen<Value<'static>>`, `get_option_ref` an `OwnedFrozenRef`, and
  `frozen_heap` the bare `OwnedFrozenRef<'_, ()>`.
- A `GlobalsBuilder` owns an `OwnedFrozenHeap`, onto which values are allocated
  as it is built. `Globals` is an owned carrier of the resulting table; entries
  are reached as `OwnedFrozenRef`s through `get_ref` and `iter`.
- A `MethodsBuilder` owns an `OwnedFrozenHeap`. `Methods` holds the sealed heap
  and its members at the `'static` brand. Methods tables are only ever reached
  as `&'static Methods` through `StarlarkValue::get_methods`, so their members
  are immortal and the `'static` brand is honest; they come to a brand through
  the immortal edge.
- Statics: `AllocStaticSimple`, the `static_starlark_value!` family and
  `const_frozen_string!` are `Value<'static>`s. They are in no heap and need no
  reference; use `at()`.

## Where Starlark adds references

- **Compiling a module** adds the `Globals` heap as a reference of the module's
  frozen heap, through a reconstructor edge, so the compiled code can name
  globals directly.
- **`load()`** and `Module::import_public_symbols` add the loaded
  `FrozenModule`'s heap as a reference of the loading module's frozen heap
  (`add_to_frozen_heap` on the looked-up slot), and the value reaches the value
  heap through the module edge.
- **Freezing** seals the module's frozen heap into the `FrozenModule`. The
  values named by the module are moved into the frozen heap, and when it is
  sealed, `ModuleHeaps` copies the value heap's references into it, so
  everything the module could reach stays reachable from its frozen form.
- **Dropping** an unfrozen module seals its frozen heap into the value heap's
  references.

## A worked example

Moving a value from a frozen module into another module:

```rust
fn copy<'v>(from: &FrozenModule, to: &Module<'v>) -> anyhow::Result<()> {
    let x: OwnedFrozen<Value<'static>> = from.get_owned("value")?;
    let v: Value<'v> = x.add_to_heap(to.heap());
    to.set("value", v);
    Ok(())
}
```

`add_to_heap` records `from`'s heap as a reference of `to`'s value heap and
hands the value back at `'v`. When `to` is frozen, the reference is copied into
its sealed heap, so the resulting `FrozenModule` keeps `from`'s heap alive too.

## What is trusted

Three brand changes rest on a contract rather than on an edge; the `branding`
module documents each in full.

- `OptCtx::demote`: the optimizer folds frozen values it observed at a module's
  value heap into IR allocated at the module's frozen heap. The contract is that
  the two heaps are one `ModuleHeaps`'s.
- `StarlarkDeserializeContext::deserialize_value`: a value being paged in
  carries no brand; the framework re-brands it at the heap being paged in when
  the owner is reached, so `StarlarkDeserialize` impls must keep the result
  only inside the value they are deserializing.
- `Freezer::freeze` on a value that is already frozen hands it back at the
  freezer's brand without a copy; `Freezer::new` requires the target heap to
  inherit the references of the heap being frozen, or to be scoped within it.

Everything else that hands out a brand records the dependency it certifies.
