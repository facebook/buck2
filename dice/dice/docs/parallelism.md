# Parallelism and Computation Behaviour

Every computation in DICE is automatically spawned asynchronously in Rust via tokio to be computed in parallel.
They behave like standard Rust futures, suspending when users `await` dependent computations and resuming when the
dependent futures are ready.

The same identical computation is always deduplicated, so concurrent requests to the same exact key will only be
executed once if not cached. Additionally, for normal computations, we guarantee that the same instance of the computed
value is returned to all requests if the value is considered equal based on the Key's equality.
