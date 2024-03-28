---
id: deferred_materialization
title: Deferred Materialization
---

When using [Remote Execution](../remote_execution.md), Buck2 can optionally
operate with Deferred Materialization, which means that Buck2 will avoid
downloading outputs until they are required by a local action.

This can provide very substantial performance savings on builds that execute
primarily on Remote Execution, since those builds become able to proceed without
ever downloading any intermediary outputs.

At Meta, despite very fast networks being used internally, this was was observed
to make real-world builds finish approximately 2.5 times faster.

## Pitfalls

Buck2's deferred materialization makes assumptions about your Remote Execution
backend. In particular, it expects that the TTL returned from action cache
entries by your Remote Execution backend always exceeds the TTL of all output
artifacts it references.

Nonetheless, artifacts may also eventually expire from your Remote Execution
backend. When that happens, builds using Deferred Materialization may fail if
those artifacts are needed locally.

A kill is necessary to recover from those builds. However, the
[Restarter](restarter.md) can be used to mitigate this issue by restarting Buck
when it encounters an expired artifact.

<OssOnly>
At Meta, artifacts get periodically refreshed, but open source RE backends do not expose the TTL of artifacts, so this feature does not work outside of Meta.
</OssOnly>

## Enabling Deferred Materialization

To enable deferred materialization, add this to your Buckconfig:

```
[buck2]
materializations = deferred
```

## On-disk state

Buck2 can also optionally track its state on disk in a SQLite database. This
allows Buck2 to remember what files are on disk across restarts.

This can allow Buck2 to avoid re-downloading outputs from your Remote Execution
backend if they are already on disk.

To enable, add this to your Buckconfig:

```
[buck2]
sqlite_materializer_state = true
```

## Deferring Write Actions

To further speedup builds, Buck2 can also be instructed to not execute any
writes on the critical path for a build.

To enable, add this to your Buckconfig:

```
[buck2]
defer_write_actions = true
```

This mechanism is recommended if you're using the On-disk State, since it means
Buck can omit writes entirely if the same content is already on disk.

## `buck2 clean --stale`

When enabling the on-disk state, Buck2 can also optionally delete only artifacts
that were not used recently. This also requires enabling deferred write actions.

You can use this mechanism via `buck2 clean --stale`.
