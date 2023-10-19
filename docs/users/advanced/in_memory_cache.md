---
id: in_memory_cache
title: In Memory Cache
---

Buck2 can maintain an in-memory cache of actions it executed. This allows
actions to skip re-running even when they are (transitively) affected by file
changes.

## Enabling the in-memory cache

This feature requires enabling
[Deferred Materialization](deferred_materialization.md) first. This is necessary
so that Buck2 knows what's on disk. This requirement might go away once we
decouple keeping track of what's on disk and deferred materialization.

Once done, to enable, add this to your Buckconfig:

```
[buck2]
hash_all_commands = true
```
