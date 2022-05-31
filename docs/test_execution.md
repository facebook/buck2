# Test Execution

Test execution in Buck2 is a collaboration with Tpx.

Tpx is Meta's internal test runner. It has a large number of responsibilities
when used with Buck2.

Those responsibilities can be grouped as follows:

Tpx does translation:

- It understands the output formats of various supported test frameworks. This
  is used to identify test cases and collect test results.
- It understands, to an extent, the input formats as well. For example, given a
  test case, Tpx can identify what command needs to run to execute just that
  test.

It also does orchestration:

- It interacts with Test Infra to discover what tests should run, under a
  number of configurations.
- It coordinates the execution of tests. For example, it may request retries,
  or choose to bundle multiple tests in a single execution (or not).
- It reports test results to Test Infra as well.

In Buck2, the way rules interact with Tpx is via a provider called
`ExternalRunnerTestInfo`.


## Anatomy of a test run

When a user runs `buck2 test $targets`, Buck2 will:

- Identify all matching targets that have a `ExternalRunnerTestInfo`
- Build all the artifacts referenced by those targets (this will likely change
  eventually to build them only if they are used).
- Notify Tpx that those tests exist. At this point, Tpx receives a subset of
  `ExternalRunnerTestInfo`.
- Tpx requests command execution from Buck2 to list and execute tests.
- When it receives command results from Buck2, Tpx may fire off events that the
  end-user will see (such as test results), log to Test Infra, request further
  executions, etc.

Note that if more than one target is being built, test building and execution
will proceed concurrently.


## Information available on `ExternalRunnerTestInfo`

As noted, rules communicate their testing capabilities via
`ExternalRunnerTestInfo`. There are a number of fields available on
`ExternalRunnerTestInfo` to control how a given target is tested.

### Fields exposed to Tpx

Here is what is available in `ExternalRunnerTestInfo` that Tpx gets to interact
with.

- `type`: a string key that defines the type of test this is. Tpx uses this
  internally to choose a translator. Examples include `gtest`, `apple_test`,
  `custom`. Note that Tpx also allows labels to influence the orchestrator
  selection.
- `command` and `env` are respectively a list and a key-value mapping of
  arguments. Those are the inputs to translation in Tpx. They are only
  sometimes visible to Tpx. See the discussion of test arguments and handles
  below for more details.
- `labels` a set of string labels to pass to Tpx. Those have no meaning to
  Buck2 but some labels have impact on translation in Tpx.
- `contacts` a list of contacts for the tests. Those are usually oncalls.
- `executor_overrides`. This is a key-value mapping of executor configurations
  that Tpx can use when requesting execution from Buck2.

### Fields pertinent for remote execution

For compatibility with remote execution, there are two fields that rules should
set in their `ExternalRunnerTestInfo` if they should be run(nable) on RE:

- `use_project_relative_paths`. If this is `true` (the default is `false`),
  Buck2 will produce relative paths. If not, it'll produce absolute paths.
- `run_from_project_root`. If this is `true`, tests will run from the project
  root (i.e. their `cwd` will be the project root, which is the same as all
  other build commands).  If `false`, it'll be the cell root.

Note that passing `--unstable-force-tests-on-re` to `buck2 test` will override
those fields and set them to `true`, since they are a pre-requisite to run on
RE. In contrast, passing `--unstable-allow-tests-on-re` will only allow tests
that already set both those fields to `true` to execute on RE.

Also note that when `executor_overrides` are set, if an executor override is
used and results in execution on RE, it'll happen on RE unconditionally.
Therefore it is a good idea to set those fields if RE-only executor overrides
are provided.


## Verbatim arguments and handles

As noted earlier, Tpx only gets to interact with a subset of arguments provided
by rules in `ExternalRunnerTestInfo`. The reason for this is that Tpx doesn't
get to access e.g. artifacts that Buck2 knows about.

For example, consider this example:

```
binary = ctx.attrs.dep[RunInfo]
test_info = ExternalRunnerTestInfo(command = [binary, "run-tests"], ...)
```

When Buck2 actually runs this command, we'd expand `binary` to a path (and
possibly to more args). Buck2 would also account for any hidden arguments and
make those available where the command is executed. It is important for Buck2
to retain this capability when running with Tpx.

To that end, all non-trivial arguments present in `command` (and in the values
of `env`), such as `cmd_args` or `RunInfo` are exposed to Tpx as opaque
handles, and simple string arguments are passed as-is to Tpx.

This means that Tpx would see the command described above as:

```
[ArgHandle(index = 0), Verbatim("foobar")]
```

When requesting execution from Buck2, Tpx can use the `ArgHandle` and Buck2
will swap it back for the underlying value that was set on the provider.

This allows Tpx to introspect and modify parts of the command lines it
receives, as long as it doesn't need to access the actual text value of
non-verbatim arguments. Usually, this works out to be sufficient (or can be
made sufficient with a bit of refactoring in Tpx).


## Execution Configurations

By default, tests execute using the execution configuration of the associated
target. This is the execution configuration that would be used for run actions
(i.e.  `ctx.actions.run`) declared in the same target.

This is a default that actually makes little sense, but works out as long as
cross-compiling is not the norm.

That said, it's easy to see where this breaks down: for example, for iOS tests,
we need the execution platform for builds to be XCode (local or RE Mac), but to
do test listing we don't need XCode (and we'd rather do it on RE Linux where
capacity is cheaper), and to run tests we need a simulator.

To support this, `ExternalRunnerTestInfo` allows specifying override platforms.
Those are given a name, Tpx can request execution on those by passing this name
when it sends execution requests to Buck2:

```
ExternalRunnerTestInfo(
  executor_overrides = {
      "ios-simulator": CommandExecutorConfig(
          local_enabled = False,
          remote_enabled = True,
          remote_execution_properties = {
              "platform": "ios-simulator-pure-re",
              "subplatform": "iPhone 8.iOS 15.0",
              "xcode-version": "xcodestable",
          },
          remote_execution_use_case = "tpx-default",
      ),
      "static-listing": CommandExecutorConfig(local_enabled = True, remote_enabled = False),
  },
  ...
)
```

There is currently no way to modify the default execution platform for a test.
This will likely change eventually.


## Working Directory

As noted earlier, tests run from the cell root, unless `run_from_project_root`
is set. If you need to produce paths relative to the cell root for use by
tests, you can do this `relative_to(ctx.label.cell_root)` on `cmd_args`.
