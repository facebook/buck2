# FAQ

In addition to the [migration guide](migration_guide.md), this document answers some common issues users have. For issues not covered, ask in the [Buck2 Users group][buck2_users]

## Where is my output file?

To find where the output of a target is, do `buck2 build mytarget --show-output`. This path will be relative to the root of the repo (e.g. `~/fbsource/...`) whereas in Buck1 it would have been relative to the enclosing cell (e.g. `~/fbsource/fbcode/...`).

## My build is failing with a duplicate symbol error when I link in `@mode/opt`

This is a bug in your code, you need to resolve the duplicate symbol error. This is usually done by renaming one of the symbols.

See those posts for more information:

- https://fb.workplace.com/groups/buck2users/permalink/3186784961577863/
- https://fb.workplace.com/groups/buck2users/posts/3183946861861673/

The [build migration section](migration_guide.md#other-changes) explains this in more detail.

## Why is my build  / test failing?

If your build fails, first ascertain that the failure is related to Buck2 before reporting in the Buck2 Users group by building with Buck1 and seeing if that works. If your build is also broken in Buck1, seek support from your team mates or in project / language specific groups.

If your build succeeds using Buck1, then the error might be transient. Retry your build. If it passes, then look for mentions of ongoing SEVs in the [Buck2 Users group][buck2_users]. If there is none, you may want to report your issue to ensure we're aware.

If the issue is not transient, then the root cause is likely compatibility with Buck2. The [build migration section](migration_guide.md#build-migration) explains common problems and solutions.

If you still don't have an answer to your question, post in the [Buck2 Users group][buck2_users]. Make sure to include the source control revision you're building from, the command you're running, and the error you are seeing.

We also appreciate it if you can tell us what work this is blocking you from completing so that we can prioritize accordingly.

## Why is Buck2 hanging?

If Buck2 seems to be doing nothing, that might be a cycle in your dependencies, which (unfortunately) currently causes Buck2 to hang. You can confirm this by running Buck1, which will report cycles properly. Alternatively, if Buck1 isn't an option, you can set the environment variable `export DICE_DETECT_CYCLES_UNSTABLE=enabled` and restart Buck2 with `buck2 kill` - this will impose a significant performance penalty but detect cycles. This state of affairs sucks, and we're working on it in [T67259280](https://www.internalfb.com/tasks/?t=67259280).

## Why are things not rebuilding?

When Buck2 starts building it lists files that have changed:

```shell
File changed: fbsource//tools/build_defs/structs.bzl
```

If you are changing files, but no files are reported as having changed, then Watchman is failing to notify Buck2. You can run `watchman shutdown-server`, which may remedy the issue. If not, run `watchman-diag tool | pastry` and see if you have lots of "re-crawling" happening. If so, report to the [Watchman User's group](https://fb.workplace.com/groups/watchman.users) for further help.

The source control team [recommend using EdenFS](https://www.internalfb.com/intern/wiki/Watchman/Troubleshooting/migration/) to get the most reliable Watchman experience.

## How do I get the commands Buck2 executed so I can reproduce them in isolation?

See [Using `buck2 log what-ran` to find commands that Buck2 ran](developers/what-ran.md)

## What does `Transport endpoint is not connected` mean?

Usually means that your Eden mount is not working. Try `eden restart` to fix it.

## What does `Text file busy` mean?

This error message is the result of a race condition where an executable file is open for writing while another process is spawned. To fix, simply rerun the command. A proper fix is being worked on as part of [T107518211](https://www.internalfb.com/tasks/?t=107518211).

## How can I tell if an `fbpkg` has been built with `buck1` or `buck2`?

The build system that was used is embedded in the build info. You can look at it using the [standard fbcode mechanisms of inspecting a binary's build info at runtime](https://fburl.com/code/og05da8n), or you can inspect your fbpkg's binary manually by reading the `fb_build_info` ELF section:

```shell
$ fbpkg fetch cerberus.service
2022-04-15 12:58:30,493 fbpkg.fetch INFO: completed download of cerberus.
All packages downloaded successfully
$ objcopy cerberus_server /dev/null --dump-section fb_build_info=/dev/stdout | jq
{
  "build_identifier": "",
  "build_mode": "opt",
  "build_source": "Unknown",
  "build_tool": "buck",
  "clowntown_revision": "",
  "compiler": "clang",
  "epochtime": 1648230730,
  "fdo_profile": "",
  "host": "twshared34352.04.prn6.facebook.com",
  "package_name": "cerberus.service",
  "package_release": "",
  "package_version": "c04314c14e3930328bbca1a36b7cf38a",
  "path": "/data/sandcastle/boxes/eden-trunk-hg-fbcode-fbsource/fbcode",
  "platform": "platform009",
  "revision": "8430fe390389eeb3cbb227cc0957431cdb847780",
  "revision_epochtime": 1648221050,
  "rule": "fbcode:cerberus/service:cerberus_server",
  "rule_type": "cpp_binary",
  "time": "Fri Mar 25 10:52:10 2022",
  "time_iso8601": "2022-03-25T10:52:10Z",
  "upstream_revision": "8430fe390389eeb3cbb227cc0957431cdb847780",
  "upstream_revision_epochtime": 1648221050,
  "user": "twsvcscm"
}
```

## Are multiple concurrent builds supported?

Not yet. While building targets in parallel will sometimes work, it isn't yet supported or recommended.

  [buck2_users]: https://fb.workplace.com/groups/buck2users
