# Cancellations

DICE supports cancellations of computations you have requested.
Since each computation is returned as a future, dropping the future will notify DICE that the computation is no longer
needed, and can be cancelled if appropriate. Note that dropping the future does not guarantee that computation is
canceled because of multi-tenancy, where if any other request depends on the same computation, the computation will
continue to run to completion for the other request.

## How It Works
DICE tracks all currently running computations in a map of [`WeakShared`](https://docs.rs/futures/0.3.17/futures/future/struct.WeakShared.html)
`DiceTask`s. This core map is what allows concurrent requests share work when they request for the same computation.
When a request requires a computation that is currently running in the map, it will attempt to acquire a [`Shared`](https://docs.rs/futures/0.3.17/futures/future/struct.Shared.html)
version of the `WeakShared`. If the `WeakShared` was already dropped, acquiring a `Shared` will fail, causing the
request to spawn a new `DiceTask`, holding onto a `Shared` and inserting its corresponding `WeakShared` into the map.


When a request is canceled by dropping its future, the `Shared` it holds will be dropped.
When there are no strong references (i.e `Shared` versions) of the `WeakShared`, the `DiceTask` will be dropped,
which triggers the spawned task to be aborted.
By having only active requests hold onto the `Shared`, and the map itself holding only a `WeakShared`, we can guarantee
that the futures are never canceled if there is a request actively depending on it, and that the future will be canceled
once there are no active requests for it.
