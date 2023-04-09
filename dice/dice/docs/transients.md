# Transient Errors

DICE has a concept of "transient" errors, which are errors that are non-deterministic and should be retried instead of
cached.
These are indicated by `Key::validity(Key::Value)` returning `false`.

When DICE encounters a "transient value", the value is reused for the ongoing computation transaction. That is, all
active requests of the same transaction will see the same instance of the transient value. However, this value will
not be cached such that upon obtaining a new transaction with or without committing any changes to the graph, the value
will be recomputed (once). If the recompute still results in a transient, then the value is still not cached and the
same behaviour occurs on the next fresh transaction. If the recompute results in a non-transient value, then the value
will be cached, and the next transaction will reuse the cached value if there are no changes that invalidate the value.
