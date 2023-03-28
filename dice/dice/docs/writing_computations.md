# Writing a Computation
Computations are written by declaring a struct that implements the `Key` trait.
In this trait, you will declare a `compute` function that is the calculation to perform when not cached.
This method will receive a context `DiceComputations`, which is where you can request for further keys' values. These
keys will be recorded as dependencies of the current computation.

```rust
#[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash)]
struct MyKey(String);

#[async_trait]
impl dice::api::Key for MyKey {
    type Value = MyOutputType;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        // ...
        let dep_values = ctx.compute(/* any dependent keys */).await;
        // ...

        MyOutputType
    }

    /* ... */
}
```

Additionally, there are methods `equality` and `validity` that one can implement for the `Key` trait to configure
the behaviour of [transients](transients.md) and equals for the output of the computation. Equals allows DICE to resurrect
nodes that depends on values that were invalidated, but end up recomputing to the "same" value.

## Injected Keys
Injected Keys are a special type of Keys that are NOT computed. They must have their value explicitly set when informing
DICE of updated values via `TransactionUpdater::changed_to`, and all future requests will yield such value until updated
again.

Computations are written by declaring a struct that implements the `InjectedKey` trait.
They have no compute function, but offers `equality` as well.

```rust
struct MyInjectedKey;

#[async_trait]
impl dice::api::Key for MyInjectedKey {
    type Value = MyOutputType;

    fn equality(x: Self::Value, y: Self::Value) -> bool {
        x == y
    }
}
```
