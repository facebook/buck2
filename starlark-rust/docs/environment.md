# Environments

:::warning
Some of the information within this page is outdated. However, the explanation of the problem, and thought process behind it, remains useful. The storage of values is similar but implemented using different types.
:::

Starlark (with a nested `def`) has a series of environments that may be active during an evaluation, as illustrated in the following example:

```python
x = []
def foo():
    y = True
    def bar():
        z = 1
        list.append(x, 1)
```

The above example features the following environments:

* Global environment - defining things like `list.append`
* Module environment - defining `x`
* Environment of `foo` - defining `y`
* Environment of `bar` - defining `z`

A scope can *access* variables defined above it, and often *mutate* them, but not *assign* them.

To unpack that:

* From the statements inside `bar`, you can access `list.append`, `x`, `y`, and `z`.
* From inside `bar`, you can mutate the variables to be accessed with statements like `list.append(x, 1)` (which may also be termed `x.append(1)`).
  * However, before this module is imported by another module, all of its exports become *frozen*, which means it isn't possible to mutate a global list, and if `foo` is called from a different module, then `x` can't be modified.
* If `bar` does `x = 1` that defines a local variable `x` in the function `bar`, shadowing the global `x`. As a consequence, you cannot assign to variables defined in an outer scope.

Note that assignment *after*, or even *in* non-executed conditional branches, introduces a local variable.

For example:

```python
x = 1
def f():
    print(x)
    if False:
        x = 2
```

In the above code, on executing `f()`, it would complain that `x` is referenced before assignment, as the assignment `x = 2` makes `x` a local variable.

The rest of this document outlines the various types of environments, how they are accessed,  and how they are updated.

## Global Environment

The global environment is always frozen and consists of *functions* and *type-values*. All things in the global environment are accessed by name.

Type-values are things like `list.append`, which is used when you do either `list.append(xs, 1)` or `xs.append(1)`, assuming `xs` is of type `list`. The available methods for a type can be queried (for example, `dir(list)`).

There are also global functions, such as `len`, `range`, and `str`.

## Slots

To optimise evaluation, all variables are accessed by integers, which are known as 'slots'. Many variables can be converted to slots statically during compilation, and those which can't have their slot looked up by name at runtime.

The `Slots` data type is defined as:

```rust
enum Slots {
    Frozen(FrozenSlots),
    Slots(Rc<RefCell<Vec<Option<Value>>>>),
}

struct FrozenSlots(Arc<Vec<Option<FrozenValue>>>);
```

As featured in the above code:

* A set of slots are either `Frozen`, which came from another module behind `Arc` or just normal `Slots`, which can be manipulated by the current scope (behind a `Rc`/`RefCell` for single-threaded use and mutation).
* `Vec` is accessed by the slot index.
* `Option` refers to whether the slot has been assigned yet (to detect variables referenced before assignment).

## Module Environment

The module environment is where the module executes, namely where `x` is defined above. The module environment can have values added in the following standards-conforming ways:

* Assignment statements (such as `x = 1` or `x += 1`).
* `For` loops (such as the `x` in `for x in []:`).
* Via the `load("a.bzl", "foo")`, which imports `foo` frozen.
* Via `def foo():`, which defines `foo` in the module environment. Whether a `def` is frozen or not, when it's executed, its local variables are not frozen.

In addition, two non-standards-conforming ways of defining variables are supported:

* Some modules can be injected as bindings in advance. Given a module `foo` that is injected, all the bindings of `foo` will be inserted in this module as frozen.
* The function `load_symbols` injects a dictionary of bindings into the module environment.

Note that a module has a fixed set of variables (from the standards-conforming ways), a pre-execution set (from the injections) and yet more variables at runtime (via `load_symbols`). To support that structure, the mapping from name to slot index is tracked in a struct:

```rust
enum Names {
    Frozen(FrozenNames),
    Names(Rc<RefCell<HashMap<String, usize>>>),
}
struct FrozenNames(Arc<HashMap<String, usize>>);
```

Each name is given an entry in the map with an increasing slot index. A name will only be assigned a slot once, reusing it thereafter. A corresponding `Slots` data type provides the values associated with those names.

Importantly, the `Slots` can be extended at runtime by the `load_symbols` function. As with `Slots`, you can either share things behind an `Arc` or mutate them behind an `Rc`/`RefCell`.

## Function Environment

A function can have variables introduced via assignments, `for` loops, and parameters. No additional variables can be discovered at runtime, so all names can be erased at compile time.

A function can also access variables from the functions it is statically nested within, and from the variables at the root of the module. To support this structure, at runtime we pass around the context, defined as:

```rust
struct Context {
    names: Names,
    slots: Vec<Slots>,
}
```

The above code contains the mapping of names for the module and the slots for the module and each function.

When executed, the inner-most `Slots` (at the end of `slots:`) will never be frozen, as that represents the local variables: but any other may be.

When a function value is captured in a frozen module, use `FrozenContext`:

```rust
struct FrozenContext {
    names: FrozenNames,
    slots: Vec<FrozenSlots>,
}

## List comprehension environments

A list comprehension can be defined as:

```python
[x for x in [1,2,3]]
```

In the above code:

* The statement defines a variable `x` that is immediately initialised and shadows any other variables `x` in scope.
* The variable `x` cannot be assigned to, other than in the list comprehension, as it only lives inside the comprehension and the comprehension does not permit assignment statements (only expressions). Such names are not available at the top-level, even when defined in the root of a module.

List comprehensions are implemented by adding additional entries into the `Slots` data type. Even when added at the root of a module, such names are not added to `Names`.

## Optimisations

There are a number of optimisations made to the scheme:

* When freezing a `Names` or `Slots` structure, it's important to only freeze a particular mutable variant once, or you duplicate memory unnecessarily. Therefore, the `Slots` to be `Rc<RefCell<(_, Option<FrozenSlots>)>>` are augmented, and, similarly, the `Names`.
  * When `freeze` is called, the original value is consumed, and the `Some` variant is added.
  * **Note**: it is unsafe to ever access the slots after the `freeze`.
* Programs can only assign to the inner-most `Slots`, and that slots must always be mutable. Therefore, define a local `Slots` that is always mutable, and a separate AST node for referring to it.
  * For modules, it is important that this mutable local `Slots` is *also* in scope since the scope is used to retrieve unknown variables.
