# Getting Help

In addition to the [migration guide](migration_guide.md), this document answers some common issues users have. For issues not covered, ask in the [Buck2 Users group][buck2_users]

## Why is my build  / test failing?

If your build fails, first ascertain that the failure is related to Buck2 before reporting in the Buck2 Users group by building with Buck1 and seeing if that works. If your build is also broken in Buck1, seek support from your team mates or in project / language specific groups.

If your build succeeds using Buck1, then the error might be transient. Retry your build. If it passes, then look for mentions of ongoing SEVs in the [Buck2 Users group][buck2_users]. If there is none, you may want to report your issue to ensure we're aware.

If the issue is not transient, then the root cause is likely compatibility with Buck2. The [build migration section](migration_guide.md#build-migration) explains common problems and solutions.

If you still don't have an answer to your question, post in the [Buck2 Users group][buck2_users]. Make sure to include the source control revision you're building from, the command you're running, and the error you are seeing.

We also appreciate it if you can tell us what work this is blocking you from completing so that we can prioritize accordingly.

  [buck2_users]: https://fb.workplace.com/groups/buck2users
