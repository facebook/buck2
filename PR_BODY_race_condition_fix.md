## Summary

Fixes a TOCTOU race in `host_sharing::NamedSemaphores::get` that breaks the `OnePerToken` / `OnePerTokens` exclusivity guarantee used by `HostSharingBroker`.

The previous implementation did a non-atomic `get` followed by `insert` on the underlying `DashMap`. Two concurrent callers for the same name could each observe `None`, each construct a fresh `SharedSemaphore`, and each `insert` it — the second `insert` silently overwrites the first. The two callers then hold different semaphores under the same name, and the "only one in-flight `acquire` per name" contract is violated. Visible failures surface upstream as flaky tests, "address already in use", or corrupted shared fixtures.

Closes #1302

## The race

Two threads `A` and `B` call `NamedSemaphores::get("foo")` with no entry yet:

1. `A` looks up `"foo"` → `None`.
2. `B` looks up `"foo"` → `None`.
3. `A` constructs `Sem_A` and inserts `("foo", Sem_A)`. Returns `Sem_A`.
4. `B` constructs `Sem_B` and inserts `("foo", Sem_B)` (overwriting). Returns `Sem_B`.

`A` holds `Sem_A`; the map holds `Sem_B`. Both can `acquire(1)` at the same time — they are independent objects, each with one permit. Any later caller looks up `"foo"`, gets `Sem_B`, and observes its permit as available even while `A` is "holding the lock".

## Fix

Use `DashMap::entry(...).or_insert_with(...)` so the create-if-missing step is atomic per shard. The closure runs at most once per name across all threads, so all callers observe the same underlying `SharedSemaphore`.

The read-only fast path (`if let Some(...) = self.buckets.get(...)`) is preserved so the common case — entry already present — still avoids the write-shard lock. No public API change.

## Test plan

- [ ] `cargo test -p host_sharing` passes locally, including the new `test_get_is_atomic_under_contention`.
- [ ] (Optional verification) Temporarily reverting just the body of `get` to the previous `get`-then-`insert` form makes `test_get_is_atomic_under_contention` fail within a few iterations, confirming the test catches the original bug.

The new regression test races 32 threads through a `Barrier` on the same name and asserts that all returned clones share one underlying semaphore. It is deterministic on the fixed code and probabilistic on the racy code; the outer 32× repetition makes it reliable on a typical multi-core dev machine while keeping wall-clock cost in the tens of milliseconds.

## Related (separate, not addressed here)

`acquire_from_permits_and_identifiers` in `host_sharing/src/host_sharing.rs` will self-deadlock if a caller passes an `OnePerTokens` with duplicate names, because `SortedVec` does not deduplicate. This is a related but distinct bug; happy to file a follow-up issue and PR if useful.
host_sharing: fix TOCTOU race in NamedSemaphores::get

#1302
