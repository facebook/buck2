# Incremental Actions

It's possible to make certain Buck2 actions to behave incrementally i.e. produce result for current invocation based on the result from the previous run. Incrementality could significantly improve performance of some actions such as packaging (e.g. Apple App Bundles) or linking (MSVC incremental linking).

There are several things which are essential in order to make action incremental:
1. Result from the previous run should be accessible
2. To understand which parts of the result need to be updated it should be easy to compare inputs from previous run with inputs from the current run and detect those changed

The only way to run user defined command in Buck2 is via `ctx.actions.run` and both problems are solved via its `metadata_env_var`, `metadata_path` and `no_outputs_cleanup` parameters.

When `no_outputs_cleanup` flag is turned on Buck2 won't perform any deletion of old outputs for the action. That means result from the previous run will be accessible but user script has to detect which parts of it should be deleted and perform manual cleanup.

When `metadata_env_var` and `metadata_path` parameters are there, Buck2 will create a JSON file on a disk before acually executing command. The file will contain a list of paths and hash digests for every command action input. All paths in the file are relative to Buck2 project root. Symlinks are not included in metadata, because it is possible for user script to resolve symlink and use resolved path to get destination hash digest from action metadata if it's needed. Example content of it:

```json
{
  "version": 1,
  "digests": [
    {
      "path": "buck-out/v2/gen/cell/configuration_hash/path/to/target/__target_name__/generated_file",
      "digest": "da39a3ee5e6b4b0d3255bfef95601890afd80709:10"
    },
    ...
  ]
}
```

User script which is run as a part of action execution is responsible for parsing this JSON file. There is a `version` field in it which is bumped every time there is a non-backwards compatible change to the format of the file. User script should verify that the provided data is of supported version and should be updated accordingly when the current version is newer than the supported one. Path of this JSON file is provided to the user script via an environment variable with key equal to `metadata_env_var`. User is able to specify part of the path relative to result directory via `metadata_path`.

For example, if some rule implementation has the following code:
```python
result = ctx.actions.declare_output("result")
command = cmd_args(["my_script.py", "--output", result.as_output()])
ctx.actions.run(
    command,
    category = "my_category",
    metadata_env_var = "ACTION_METADATA",
    metadata_path = "action_metadata.json",
    no_outputs_cleanup = True,
)
```
then `my_script.py` will be executed as:
```shell
ACTION_METADATA=project/relative/path/to/target/action_metadata.json my_script.py --output resolved/path/to/result
```
and `my_script.py` is responsible for reading `ACTION_METADATA` environment variable and parsing JSON file with action metadata.

Parsed metadata provides information about inputs for the current run, but the script needs somehow to obtain similar information about inputs from the previous run. Such information could just be another output of the user script (similarly to previous result it won't be deleted when `no_outputs_cleanup = True`). Format of such file is an implementation detail of the user script, but at the very least it should contain list of every source which was actually used to form the result and hash digests for such sources. So rule implementation would look something like:

```python
result = ctx.actions.declare_output("result")
state = ctx.actions.declare_output("incremental_state.json")
command = cmd_args(["my_script.py", "--output", result.as_output(), "--incremental-state", state.as_output()])
ctx.actions.run(
    command,
    category = "my_category",
    metadata_env_var = "ACTION_METADATA",
    metadata_path = "action_metadata.json",
    no_outputs_cleanup = True,
)
```

User script would then:
1. Parse `incremental_state.json` and delete it. Deletion prior amending the result is important so we do not end up in a situation where incremental state file is out of sync with the result when user script fails during changing the result. Such corrupted state might lead to subsequent incorrect builds reported as "successful".
2. Parse action metadata file, compute what is needed to update the result and amend it accordingly.
3. Calculate new state and write it into new `incremental_state.json`.
