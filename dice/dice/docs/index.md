# DICE

DICE is a dynamic incremental computation engine that supports parallel computation, inspired by
[Adapton](https://docs.rs/adapton/latest/adapton/) and [Salsa](https://github.com/salsa-rs/salsa),

DICE is the core computation engine that powers the incremental graph transformations of [buck2](https://github.com/facebook/buck2).
It is intended to offer a generic computation API that can be used beyond just Buck, so that any kind of incremental computation can run on DICE.
All computations are executed in parallel on DICE via tokio executors. Duplicate requests to the same computations are deduplicated.

DICE is currently still experimental and largely being rewritten.

## Features
- [Incrementality](incrementality.md) - Incrementality behaviour of DICE
- [Parallelism](parallelism.md) - Parallelism and Behaviour of Computations
- [Cancellations](cancellations.md) - Cancelling of a currently running computation
- [Transient Errors](transients.md) - Transient Error Handling
- Cycle Detection // TODO

## Using DICE
- [Writing Computations](writing_computations.md) - How to write computations that are incremental

## Benchmarking DICE
// TODO


## Debugging the Graph
// TODO
