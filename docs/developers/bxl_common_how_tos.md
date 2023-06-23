---
id: bxl_how_tos
title:  Common How-Tos
---

## Passing in and using CLI args

A BXL function can accept a `cli_args` attribute where args names and types are specified to use within your script, as shown in the following example:

Example:

```python
def _impl_example(ctx):
    # ...
    pass

example = bxl(
    impl = _impl_example,
    cli_args = {
        # cli args that you want to receive from the command line
        "bool_arg": cli_args.bool(),
        "list_type": cli_args.list(cli_args.int()),
        "optional": cli_args.option(cli_args.string()),
        "target": cli_args.target_label(),
    },
)
```

On the command line, you can invoke the arguments as follows:

```sh
buck2 bxl //myscript.bxl:example -- --bool_arg true --list_type 1 --list_type 2 --target //foo:bar
```

For BXL functions, to read the arguments, use them as attributes from the `cli_args` attribute on the BXL `ctx` object, as follows:

```python
def _impl_example(ctx):
    my_bool_arg = ctx.cli_args.bool_arg
```

## Running actions

You can create actions within BXL via the `actions_factory`. This is called once globally then used on demand:

```python
def _impl_example(ctx):
    actions = ctx.actions_factory # call once, reuse wherever needed
    output = actions.write("my_output", "out")
```

## Getting providers from an analysis

After calling `analysis()`, you can get the providers collection from `providers()`:

```python
def _impl_example(ctx):
    my_providers = ctx.analysis(my_target).providers()
```

## Get a specific provider from an analysis

After calling `analysis()`, you can also get the providers collection from `providers()` then grab whatever specific provider you need:

```python
def _impl_example(ctx):
    default_info = ctx.analysis(my_target).providers()[DefaultInfo]
    ctx.output.print(default_info)
```

## Build a subtarget

Once you have a provider, you can get its subtargets by using the `sub_targets` attribute on the struct to get a dict of provider labels to provider collections:

```python
def _impl_example(ctx):
    subtarget = ctx.analysis(my_target).providers()[DefaultInfo].sub_targets[“my_subtarget”]
    ctx.output.print(subtarget)
```

## Getting attributes or resolved attributes efficiently

If you need to use all of the attrs/resolved_attrs, then initializing the eager variant once would be best. If you only need a few of the attrs, then initializing the lazy variant is better. There’s not really a hard line, it depends on the target node, and which attrs you are looking for. If performance is key to your BXL script, the best way to determine this is to use the BXL profiler.

Regardless, if you use eager or lazy versions of getting attributes, you should cache the attrs object:

```python
def _impl_example(ctx):
    lazy = ctx.attrs_lazy() # call once and reuse wherever is necessary
    eager = ctx.attrs_eager() # call once and reuse wherever is necessary
```

## Inspecting a struct

You can use `dir(my_struct)` to inspect a struct. You can also use `getattr(my_struct, “my_attr”)` to grab individual attributes, which is equivalent to `my_struct.my_attr`.

These are available as part of the [Starlark language spec](https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#dir).

## Set addition/subtraction on a `target_set`

There are a few BXL actions that return a `target_set` (such as a cquery `eval()`). The `target_set` supports set subtraction and addition (you can use `-` and `+` directly in Starlark).

## Profiling, Testing, and Debugging a BXL script

You can use `buck2 bxl profiler`, with various measurements, to determine where the script is least efficient.

To time individual pieces of the script, you can use BXL’s timestamp methods:

```python
def _impl_example(_ctx):
    start = now() # call once and reuse wherever is necessary
    # do something time intensive here
    end1 = start.elapsed_millis()
    # do something else time intensive here
    end2 = start.elapsed_millis()
```

BXL does not have a debugger available nor a robust testing framework for mocking.

* **Debug** - the main method to debug a BXL script is with print statements (`print()` and `ctx.output.print()`).
* **Test** - the main method to test a BXL script is to actually invoke it with required inputs then verify the outputs.

## Telemetry

### Emitting events from your BXL script

In BXL, you can emit custom events via `ctx.instant_event()`, which takes in two named parameters:
* `id`: string, identifies your event. Helpful to identify your event when looking through event logs. Ids do not have to be unique in a single BXL script.
* `metadata`: dict, where keys are strings and values are strings, bools, or ints. You can put any metadata you wish here.

Example:

```python
def _impl(ctx):
  ctx.instant_event(id = "id1", metadata = {"foo": "bar"})

my_script = bxl(
  impl = _impl,
  cli_args = {},
)
```

Only instant events can be manually created within BXL at this time, which means that the event represents a single point in time. If you need something similar to spans (start and end events which encompass a range of time) for measuring the duration of a particular section (excluding actions - see below for more information), you could couple instant events with the global `now()` function to measure the duration yourself:

```python
def _impl(ctx):
  instant = now()

  # do something time intensive
  end = instant.elapsed_millis()
  ctx.instant_event(id = "id1", metadata = {"duration": end})

  # do something else time intensive
  end = instant.elapsed_millis()
  ctx.instant_event(id = "id2", metadata = {"duration": end})

my_script = bxl(
  impl = _impl,
  cli_args = {},
)
```

**Measuring time for actions and ensuring artifacts**

You cannot use `now()` to measure the time it takes to run actions and ensure artifacts because these processes occur asynchronously outside of the BXL script execution. For BXL user telemetry, we emit action events via the buck2 core automatically. Events around ensuring the artifacts are not emitted currently, but will be added soon.

### User event log

To write to your own event log when running BXL, you can run your BXL command with the `--user-event-log` flag to tell buck2 where to write the events to. Buck2 is aware of the following file extensions: `.json-lines`, `json-lines.zst`, `.json-lines.gz`, and will compress the files automatically for you depending on the extension. If the extension is not one of these, the logs will always be written in JSONL format, uncompressed.

Example:

```
buck2 bxl path//to/my_script/script.bxl:my_script --user-event-log my_file.json-lines.gz
```

When using this flag to write to a custom event log, it is up to you to clean up these log files. In addition, if the same filename is used with subsequent BXL invocations, events are always appended to the existing file contents, which is the same behavior as `buck2 <any command> --event-log <path>`. If you tell buck2 to write to a compressed file, you are responsible for decompressing them.

### Getting a user event log from a normal event log

`buck2 log show-user` can be used to convert a normal event log (regardless of encoding/compression) to a user event. Similar to `buck2 log show`, you can choose the most recent invocation, or the nth invocation, or provide a path to the normal user event log. Note that user event logs are not able to be passed into `buck2 log show` or `buck2 log show-user`.

### Event log output

The first line of your event log will always be the invocation record, which contains useful things like command line args used, working directory, etc. The subsequent lines are either instant events and/or action events, depending on your BXL script's contents.

**Instant event**

Sample:
```python
{
  "StarlarkUserEvent": {
    "id": "foo",
    "metadata": {
      "bool_value": true,
      "string_value": "str",
      "int_value": 123,
    }
  },
  "epoch_millis": 123456789 # when the event was emitted
}
```

**Action event**
```python
{
  "ActionExecutionEvent": {
    "kind": "Write", # kind of action, like write or run
    "name": { # name of the action, for user display. Unique within the execution of a particular target
      "category": "write", # category for the action
      "identifier": "my_output" # identifier for the action
    },
    "duration_millis": 0, # duration of the action in millis, excluding input materialization time
    "output_size": 10, # size in bytes of the action's outputs
    "input_materialization_duration_millis": 0 # how long it took to materialize any inputs to the action
  },
  "epoch_millis": 123456789 # when the event was emitted
}
```

**Ensure artifact event**
```python
{
  "BxlEnsureArtifactsEvent": {
    "duration_millis": 0, # duration of ensuring the artifact
  },
  "epoch_millis": 123456789 # when the event was emitted
}
```
