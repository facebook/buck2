---
id: daemon
title: Daemon (buckd)
---

The first time that a Buck2 command is run, Buck2 starts a daemon process for the current project. For subsequent commands, Buck2 checks for the running daemon process and, if found, uses the daemon to execute the command. Using the Buck2 daemon can save significant time as it enables Buck to share cache between Buck2 invocations.

By default, there is 1 daemon per [project](./glossary.md#project) root, you can run multiple daemons in the same project by specifying an [isolation dir](./glossary.md#isolation-dir).

While it runs, the Buck daemon process monitors the project's file system for changes. The Buck daemon excludes from monitoring any subtrees of the project file system that are specified in the `[project].ignore` setting of `.buckconfig` (for details, see the still-relevant [[project].ignore](../legacy/files-and-directories/dot-buckconfig.md#ignore) section of the '.buckconfig' legacy document).

## Killing or disabling the Buck daemon

The Buck daemon process is killed if `buck2 clean` or `buck2 kill`commands are run. Note that they won't kill the daemon associated with custom isolation dirs. To do that, run using the `--isolation-dir` option (`buck2 --isolation-dir <dir> <command>`)

<FbInternalOnly>

The Daemon is also killed when:

* The `buck2 killall` command is run.
* A new buck2 version is available.

</FbInternalOnly>
