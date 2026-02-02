---
id: local_resources
title: Local Resources For Tests Execution
---

Executing a test might require an external resource which is expensive to
create. For example running an iOS UI test requires an iOS simulator and it
takes relatively long time to setup it prior to test execution. When tests are
executed remotely resources initialization and allocation could be preemptively
managed by remote execution tier which is not the case for local execution. To
effectively manage such resources needed for local execution of tests there is a
separate Buck2 feature backed by `LocalResourceInfo` provider.

## `LocalResourceInfo` provider

This provider describes how to initialize and clean up a pool of homogeneous
local resources. Management of initialized resources is done by Buck2 itself
when it executes tests requiring such resources.

Fields:

- `setup` — command represented by `cmd_args` object which is executed to
  initialize a local resource. Running this command should write a JSON to
  stdout. This JSON represents a pool of local resources which are ready to be
  used.
- `resource_env_vars` — key-value mapping `{str: str}` from environment variable
  (appended to an execution command for test which is dependent on this local
  resource) to keys in JSON output of `setup` command.

Example JSON output of `setup` command:

```json
{
  "pid": 42,
  "resources": [{"socket_address": "foo:1"}, {"socket_address": "bar:2"}]
}
```

JSON keys:

- `pid` — an optional attribute which maps to a PID of a process that holds
  initialized local resources. If present, on non-Windows platforms the process
  will be sent `SIGTERM` when those resources are no longer needed. Signal
  should be handled to release any system resources related to local resources.
- `resources` — a list of resource instances, each is a mapping from a string
  alias (e.g. `socket_address`) to a value which represents resource. The number
  of concurrently running tests that require resources of the same type is
  limited by how many instances are in a list. String alias is mapped to an
  environment variable key (which will be added to a command requiring such
  resource) using a `resource_env_vars` field in `LocalResourceInfo` provider
  (see [example](#example-usage) below).

## Test Execution

For a general context on how tests are executed, see
[Test Execution](test_execution.md).

A decision whether certain local resource is required for specific test is made
by a test runner. List of required resources is then passed to Buck2 in
`required_local_resources` field of `ExecuteRequest2` test API protobuf message.

If resource is required for a certain test execution and test could potentially
be executed locally, `local_resources` field in test's `ExternalRunnerTestInfo`
provider is used to select appropriate `LocalResourceInfo` provider.

`ExternalRunnerTestInfo.local_resources` is a key-value mapping
`{str: ["label", None]}`. Keys represent resource types that match the values
passed from the test runner, and values are labels that should point to a target
exposing the `LocalResourceInfo` provider to be used for the initialization of
the resource of that type. If the value is `None`, it indicates that a resource
of that type will not be provided, even if the test runner requests it.

Before running a test, `setup` command from selected provider is executed and
its output is used to create a pool of resource instances. This pool is shared
across all tests pointing to the same configured target label containing
`LocalResourceInfo` provider (normally that means pool is shared for tests
requiring same resource type). A resource is acquired (with potential queuing)
from that pool prior single test is executed and is returned back to the pool
when test finished execution. After `buck2 test` command is finished, cleanup is
performed when SIGTERM is sent to each process holding a pool of resources.

## Example Usage

Define a target which has `LocalResourceInfo` provider:

```python
simulator(
  name = "my_resource",
  broker = ":broker",
)
```

where `broker` points to a runnable handling actual simulators.

Implementation of `simulator` rule would be:

```python
def _impl(ctx: AnalysisContext) -> ["provider"]:
  return [
    DefaultInfo(),
    LocalResourceInfo(
      setup = cmd_args([ctx.attrs.broker[RunInfo]]),
      resource_env_vars = { "IDB_COMPANION": "socket_address" },
    )
  ]
```

Running a `:broker` via `setup` command produces the following JSON:

```json
{
  "pid": 42,
  "resources": [{"socket_address": "foo:1"}, {"socket_address": "bar:2"}]
}
```

When Buck2 locally executes a test which requires this particular type of local
resource, it reserves one resource from the pool (e.g.
`{"socket_address": "bar:2"}`) and add environment variable representing this
resource to execution command (e.g. `IDB_COMPANION=bar:2`). In our examples
`"socket_address"` alias was substituted by `"IDB_COMPANION"` based on
`LocalResourceInfo.resource_env_vars` field.

The last part is to map a resource type to desired `LocalResourceInfo` provider.
Let's assume a test runner requires a resource of type "ios_simulator" for every
`apple_test` rule.

Pass `:my_resource` target as a dependency into `apple_test` rule:

```python
apple_test = rule(
    impl = apple_test_impl,
    attrs = {
        ...
        "_ios_simulator": attrs.default_only(attrs.dep(default = ":my_resource", providers = [LocalResourceInfo])),
        ...
    },
)
```

Actually map "ios_simulator" resource type to `:broker` target containing
`LocalResourceInfo` provider:

```python
def apple_test_impl(ctx: AnalysisContext) -> ["provider"]:
    ...
    return [
        ...
        ExternalRunnerTestInfo(
            ...
            local_resources = {
                "ios_simulator": ctx.attrs._ios_simulator,
            },
            ...
```
