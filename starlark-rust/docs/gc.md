# A moving Garbage Collector

In this document we describe a two-space garbage collector that can deal with cycles. In Starlark, we use this pattern both when doing a real garbage collection, and when freezing. In both cases, we start out with a memory block which has pointers referring to things inside it, and end up with a new memory block with equivalent pointers inside it. However, only pointers reachable from outside the original memory block are available in the new memory block. The garbage collector can deal with cyclic data structures and the time spent is proportional to the amount of live data in the heap (memory that is dropped is not even visited).

## A worked example

Given a heap with the layout:

```
X := Data("world")
Y := Data("hello", X, Y)
Z := Data("universe")
```

All of `X`, `Y` and `Z` are memory locations. The `Y` memory location has both some data of its own (i.e. `"hello"`) and two pointers (`X` and `Y` itself).

The pointers from outside the heap into the heap are known as _roots_, and in our example let's assume that `Y` is the only root. Because `Y` is used from outside, we must move `Y` to the new memory block. Consequently, the data `X` needs to be copied, but `Z` can be dropped.

Step 1: To copy `Y` we allocate a value in the new heap `A` with a sentinel value in it (we call that sentinel `Blackhole`). We turn `Y` into a `Forward(A)` pointer, so that if anyone else in this cycle tries to collect `Y` they immediately "forward" to the new value. And we grab the data from `Y` so we can traverse its pointers. That gives us:

```
X := Data("world")
Y := Forward(A)
Z := Data("universe")

A := Blackhole
```

With `Data("hello", X, Y)` the current item we are processing.

Step 2: We now walk the pointers of the current value, performing a garbage collection on each of them. To copy `Y` we observe that `Y` points at a `Forward(A)` node, so don't need to do anything. To copy `X` we follow the process starting at Step 1, but for `X` (which ends up at `B`). After performing that move we are left with:

```
X := Forward(B)
Y := Forward(A)
Z := Data("universe")

A := Blackhole
B := Data("world")
```

Step 3: We now replace all our pointers with the forwarded value, and write it back over the `Blackhole` in `A`. That gives us:

```
X := Forward(B)
Y := Forward(A)
Z := Data("universe")

A := Data("hello", B, A)
B := Data("world")
```

Step 4: We now adjust the any roots pointing at `Y` to point at `A` and throw away the original heap, leaving us with:

```
A := Data("hello", B, A)
B := Data("world")
```

We have successfully garbage collected a cyclic data structure, preserving the cycles, and getting rid of the unused data.
