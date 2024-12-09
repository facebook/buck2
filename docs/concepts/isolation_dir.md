---
id: isolation_dir
title: Isolation dir
---

Every `buck2` daemon has an "isolation directory" which acts as an isolation
mechanism. This enables the ability to have multiple independent daemons, each
with a different isolation dir.

Different daemons enable the concurrent execution of commands, as a **single
daemon can generally execute only a single command at a time**. Most buck2
commands only act in their own specified isolation dir. For example,
`buck2 kill` will only kill the daemon associated to the default isolation-dir
(`v2`). Exceptions to this rule include `buck2 killall` command, which will kill
all buck2 processes on the machine.

WARNING: Isolation dirs have a very important consequence: `buck2` invocations
with _different_ isolation dirs **never share _any_ cached artifacts**.

## Isolation dir uses

Isolation dirs are also relied on for developer environment tooling (e.g. LSPs)
that have to run in the background without affecting the users manually
triggered builds.

Also isolation dirs are used to allow for
[recursive invocations](../users/faq/common_issues.md#are-multiple-concurrent-commands-supported).

## How to set the isolation dir for a command

The default isolation dir is `v2`. Thus any `buck2` command without an explicit
isolation dir is equivalent to

```sh
$ buck2 --isolation-dir v2 $ARGS
```

The isolation dir can also be set via the `BUCK_ISOLATION_DIR` env var.

NOTE: `--isolation-dir` arg must _always_ appear immediately after `buck2`. For
example, `buck2 build --isolation-dir v2 $ARGS` is an invalid command.

## Isolation dir buck-out path

The isolation dir specifies the root directory in [`buck-out`](./buck_out.md)
which will be used to isolate a daemon. With the default `v2` isolation dir,
`buck2` will be using the `$PROJECT_ROOT/buck-out/v2` directory for all target
outputs and internal metadata. `$PROJECT_ROOT` refers to the directory where the
[project](./glossary.md#project) lives.
