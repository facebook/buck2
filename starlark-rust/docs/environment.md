# Environments

_**Warning**: Some of the information in this document is outdated, but the explanation of the problem, and thought process behind it, remains useful. The storage of values is similar, but implemented using different types._

Starlark (with nested `def`) has a series of environments that may be active during an evaluation. For example, given:

```python
x = []
def foo():
    y = True
    def bar():
        z = 1
        list.append(x, 1)
```

There is the global environment (defining things like `list.append`), the module environment (defining `x`), the environment of `foo` (defining `y`) and the environment of `bar` (defining `z`). A scope can _access_ variables defined above it, and often _mutate_ them, but not _assign_ them. To unpack that:

* From the statements inside `bar` we can access `list.append`, `x`, `y` and `z`.
* From inside `bar` we can mutate the variables we can access, with statements like `list.append(x, 1)` (can also be written `x.append(1)`). However, before this module is imported by another module, all of its exports become _frozen_. That means it isn't possible to mutate a global list, and if `foo` is called from a different module, we can't modify `x`.
* If `bar` does `x = 1` that defines a local variable `x` in the function `bar`, shadowing the global `x`. As a consequence, we cannot assign to variables defined in an outer scope.

Note that assignment _after_, or even in non-executed conditional branches, introduces a local variable. For example:

```python
x = 1
def f():
    print(x)
    if False:
        x = 2
```

On executing `f()` would complain that `x` is referenced before assignment, as the assignment `x = 2` makes `x` a local variable.

In the rest of this document we outline the various types of environment, how they are accessed and how they are updated.

## Global Environment

The global enviroment is always frozen and consists of _functions_ and _type-values_. All things in the global environment are accessed by name.

Type-values are things like `list.append`, which is used when you do either `list.append(xs, 1)` or `xs.append(1)`, assuming `xs` is of type `list`. The available methods for a type can be queried, e.g. using `dir(list)`.

There are also global functions, e.g. `len`, `range`, `str`.

## Slots

To optimise evaluation, we access all variables by integers which we refer to as _slots_. Many variables can be converted to slots statically during compilation, and those which can't have their slot looked up by name at runtime. The `Slots` data type is defined as:

```rust
enum Slots {
    Frozen(FrozenSlots),
    Slots(Rc<RefCell<Vec<Option<Value>>>>),
}

struct FrozenSlots(Arc<Vec<Option<FrozenValue>>>);
```

A set of slots are either frozen (came from another module, behind `Arc`), or normal slots which can be manipulated by the current scope (behind `Rc`/`RefCell` for single-threaded use and mutation). The `Vec` is accessed by the slot index, and the `Option` refers to whether the slot has been assigned yet (to detect variables referenced before assignment).

## Module Environment

The module environment is where the module executes, namely where `x` is defined above. The module environment can have values added in the following standards-conforming ways:

* Assignment statements, e.g. `x = 1` or `x += 1`.
* For loops, e.g. the `x` in `for x in []:`.
* Via the `load("a.bzl", "foo")`, which imports `foo` frozen.
* Via `def foo():`, which defines `foo` in the module environment. Whether a `def` is frozen or not, when executing, its local variables are not frozen.

In addition, we support two non-standards-conforming ways of defining variables:

* Some modules can be injected as bindings in advance. Given a module `foo` that is injected, all the bindings of `foo` will be inserted in this module, frozen.
* The function `load_symbols` injects a dictionary of bindings into the module environment.

Note that a module has a fixed set of variables (from the standards-conforming ways), a pre-execution set (from the injections) and yet more variables at runtime (via `load_symbols`). To support that structure we keep track at runtime of the mapping from name to slot index:

```rust
enum Names {
    Frozen(FrozenNames),
    Names(Rc<RefCell<HashMap<String, usize>>>),
}
struct FrozenNames(Arc<HashMap<String, usize>>);
```

Each name is given an entry in the map with an increasing slot index. A name will only be assigned a slot once, reusing it thereafter. A corresponding `Slots` data type provides the values associated with those names. Importantly, the `Slots` can be extended at runtime by the `load_symbols` function. Like with `Slots`, we can either share things behind an `Arc` or mutate them behind an `Rc`/`RefCell`.

## Function Environment

A function can have variables introduced via assignments and `for` loops, and also via parameters. No additional variables can be discovered at runtime, so all names can be erased at compile time. A function can also access variables from the functions it is statically nested within, and from the variables at the root of the module. To support this structure, at runtime we pass around the context, defined as:


```rust
struct Context {
    names: Names,
    slots: Vec<Slots>,
}
```

Here we have the mapping of names for the module, and the slots for the module and each function. When executing, the inner-most `Slots` (at the end of `slots`) will never be frozen, as that represents the local variables -- but any other may be. When we capture a function value in a frozen module, we switch to a `FrozenContext`:

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

This statement defines a variable `x` that is immediately initialised and shadows any other variables `x` in scope. The variable `x` cannot be assigned to other than in the list comprehension, as it only lives inside the comprehension, and the comprehension does not permit assignment statements (only expressions). Such names are not available at the top-level, even when defined in the root of a module.

We implement list comprehensions by adding additional entries into the `Slots` data type. Even when added at the root of a module, such names are not added to `Names`.

## Optimisations

There are a number of optimisations we make to the scheme:

* When freezing a `Names` or `Slots` structure it's important to only freeze a particular mutable variant once, or you duplicate memory unnecessarily. Therefore, we augment the slots to be `Rc<RefCell<(_, Option<FrozenSlots>)>>` and similarly for names. When `freeze` is called the original value is consumed and the `Some` variant is added. It is unsafe to ever access the slots after the `freeze`.

* Programs can only assign to the inner-most `Slots`, and that slots must always be mutable. Therefore we define a local slots that is always mutable, and a separate AST node for refering to it. For modules, it is important that this mutable local `Slots` is _also_ in the scope, since the scope is used to retrieve unknown variables.
