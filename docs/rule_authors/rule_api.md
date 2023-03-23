---
id: rule_api
title: Rule APIs
---

When implementing a rule, you are given a value of type `context` and are expected to produce providers. This page details those providers and the attributes and APIs that they offer.

## Providers

* `DefaultInfo(default_outputs : ["artifact"], other_outputs : [["artifact", "cmd_args"]] = [], sub_targets : {str.type: ["provider"]} = {})` - the provider that is used for:
  * `buck2 build` - builds everything in `default_outputs` and `other_outputs`.
  * `$(location)` - uses the `default_outputs`.
  * `buck2 build my_target[foo]` - selects the `foo` value from `sub_targets`.
  * **Note**: if you use `cmd_args` in `other_outputs`, then it will expand to all the inputs referenced by the `cmd_args` you provide.
* `RunInfo(args)` - used for `buck2 run`, where `args` is anything that can be converted into `cmd_args`, including a command line itself.
* `ExternalRunnerTestInfo(...)` - for details, see [Test Execution](test_execution.md).

## Type `context`

The starting type, usually bound as `ctx`.

* `ctx.attrs` - returns the attributes of the target as a Starlark struct with a field for each attribute, which varies per rule.
* `ctx.actions` - returns `actions` allowing you to define actions.
* `ctx.label` - returns a `label` representing the target.

## Type `actions`

Most output filenames can either be artifacts created with `declare_output` or strings that are implicitly converted to output artifacts.

* `ctx.actions.declare_output([prefix], filename, dir = False)` - returns an `artifact` with the name `filename`, which when asked for its name, will return `filename` (which may include a directory portion).
  * `prefix` (optional) - provides a silent part of the filename, which can be used to disambiguate but whose presence will not be visible to anyone using the `artifact`. By default, outputs are considered files; pass `dir = True` to indicate it is a directory.
  * `declare_output` - mainly used to produce an unbound artifact for passing to `ctx.actions.run`.

* `ctx.actions.write(filename, content, is_executable : bool.type = false, allow_args : bool.type = false)` - returns an `artifact` whose contents are `content`.
  * `filename` - can be a string or an existing artifact created with `declare_output`.
  * `is_executable` (optional) - indicates whether the resulting file should be marked with executable permissions.
  * `allow_args` (optional) - must be set to `True` if you want to write parameter arguments to the file (in particular, macros that write to file).
    * If it is true, the result will be a pair of the `artifact` containing `content` and a list of `artifact` values that were written by macros, which should be used in hidden fields or similar.

* `ctx.actions.write_json(filename, content, with_inputs = False)` - returns an `artifact` whose contents are `content` written as a JSON value.
  * `filename` - can be a string, or an existing artifact created with `declare_output`.
  * `content` - must be composed of the basic json types (Boolean, number, string, list/tuple, dictionary) plus artifacts and command lines.
    * An artifact will be written as a string containing the path.
    * A command line will be written as a list of strings, unless `joined=True` is set, in which case it will be a string.
  * If you pass `with_inputs = True`, you'll get back a `cmd_args` that expands to the JSON file but carries all the underlying inputs as dependencies (so you don't have to use, for example, `hidden` for them to be added to an action that already receives the JSON file).

* `ctx.actions.copy_file(dest, src)` - copies the source `artifact` to the destination (which can be a string representing a filename or an output `artifact`) and returns the output `artifact`. The copy works for files or directories.

* `ctx.actions.symlink_file(dest, src)` - creates a symlink to the source `artifact` at the destination (which can be a string representing a filename or an output `artifact`) and returns the output `artifact`. The symlink works for files or directories.

* `ctx.actions.symlinked_dir(output, srcs : {str.type: "artifact"})` - returns an artifact that is a directory containing symlinks. The `srcs` must be a dictionary of path (as string, relative to the result directory) to bound `artifact,` which will be laid out in the directory.

* `ctx.actions.copied_dir(output, srcs : {str.type: "artifact"}, copy : bool.type = false)` - returns an artifact which is a directory containing copied files. The `srcs` must be a dictionary of path (as string, relative to the result directory) to the bound `artifact`, which will be laid out in the directory.

* `ctx.actions.download_file(output, url : str.type, sha1: str.type, is_executable : bool.type = false)` - downloads a URL to an output (filename as string or output `artifact`). The file at the URL must have the given `sha1` or the command will fail. The optional parameter `is_executable` indicates whether the resulting file should be marked with executable permissions.

* `ctx.actions.run(arguments, category : str.type, identifier : str.type = "", env : {str.type: str.type} = {}, local_only : bool.type = false, always_print_stderr : bool.type = false, weight : int.type = 1, metadata_env_var: str.type = None, metadata_path: str.type = None, no_outputs_cleanup: bool.type = false)` - runs a command.
  * `arguments` - must be of type `cmd_args`, or a type convertible to such (such as a list of strings and artifacts) and must contain at least one `.as_output()` artifact.
  * `category` and `identifier` - when used together, identify the action in Buck2's event stream, and must be unique for a given target.
  * `weight` is used to note how heavy the command is and will typically be set to a higher value to indicate that less such commands should be run in parallel (if running locally).
  * `no_outputs_cleanup` - if this flag is set then Buck2 won't clean the outputs of a previous build that might be present on a disk; in which case, command from `arguments` should be responsible for the cleanup (that is useful, for example, when an action is supporting incremental mode and its outputs are based on result from a previous build).
  * `metadata_env_var` and `metadata_path` - both should either be set or unset.
    * `metadata_path` defines a path relative to the result directory for a file with action metadata, which will be created right before the command will be run.
      * Metadata contains the path relative to the Buck2 project root and hash digest for every action input (this excludes symlinks as they could be resolved by a user script if needed). The resolved path relative to the Buck2 project for the metadata file will be passed to command from `arguments`, via the environment variable, with its name set by `metadata_env_var`.
    * Both `metadata_env_var` and `metadata_path` are useful when making actions behave in an incremental manner (for details, see [Incremental Actions](./incremental_actions.md))

* `ctx.actions.tset(type, value = None, children = None)` - creates a new transitive set (for details, see [Transitive Sets](./transitive_sets.md)).

* `ctx.actions.cas_artifact(output, digest : str.type, use_case: str.type, expires_after_timestamp: int.type, is_executable : bool.type = false)` - downloads a CAS artifact to an output.
  * `digest` - must look like `SHA1:SIZE`.
  * `use_case` - your RE use case.
  * `expires_after_timestamp` - must be a UNIX timestamp. Your digest's TTL must exceed this timestamp. Your build *will* break once the digest expires, so make sure the expiry is long enough (preferably, in years).
  * `is_executable` (optional) - indicates the resulting file should be marked with executable permissions.

## Type `cmd_args`

The `cmd_args` type is created by `cmd_args` and is consumed by `ctx.actions.run`. The type is a mutable collection of strings and `artifact` values. In general, command lines, artifacts, strings, `RunInfo` and lists thereof can be added to or used to construct a `cmd_args` value. All these methods operate mutably on `cmd` and return that value too.

* `cmd_args(*args, format: str.type = "", delimiter: str.type = None, prepend: str.type = None, quote: str.type = None)` - creates and returns a `cmd_args` type.
  * `*args` - a list of things to add to the command line, each of which must be coercible to a command line. Further items can be added with `cmd.add`.
  * `format` (optional) - a string that provides a format to apply to the argument. for example, `cmd_args(x, format="--args={}")` would prepend `--args=` before `x`, or if `x` was a list, before each element in `x`.
  * `delimiter` (optional) - added between arguments to join them together. For example,  `cmd_args(["--args=",x], delimiter="")` would produce a single argument to the underlying tool.
  * `prepend` (optional) - added as a separate argument before each argument.
  * `quote` (optional) - indicates whether quoting is to be applied to each argument. Note: the only current valid value is `"shell"`.

* `cmd.add(*args)` - a list of arguments to be added to the command line, as per `cmd_args`.

* `cmd.hidden(*args)` - things to add to the command line which do not show up but are added as dependencies.

* `cmd.ignore_artifacts()` - conceptually the opposite of `hidden()`. It causes none of the arguments of the command line to be added as dependencies.
  * Use this if you need the path to an artifact but *not* the artifact itself.
  * Note:  if you do find yourself needing any of the inputs referenced by this command, you will hit build errors due to missing dependencies.

* `cmd.relative_to(directory, parent : int.type = 0)` - complex magic. Before using this, please contact Meta's Buck2 team.

* `cmd.absolute_prefix(prefix : str.type)`- adds a prefix to the front of every artifact.

* `cmd.absolute_suffix(suffix : str.type)` - adds a suffix to the end of every artifact.

* `cmd.parent(count : int.type = 1)` - uses the parent of all given artifacts. Often used as `cmd_args(artifact, format="-L{}").parent()`.

* `cmd.replace_regex(pattern : str.type, replacement : str.type)` - replaces all parts matching `pattern` regular expression in each argument with `replacement` string. Several replacements can be added by multiple `replace_regex` calls.

* `cmd.copy()` - returns a copy of the `cmd_args` such that any modifications to the original or the returned value will not impact each other.

* `cmd.inputs` - returns a list of the artifacts that are inputs to this command line.

* `cmd.outputs` - returns a list of the artifacts that are outputs of this command line.

## Type `label`

A label represents a configured target.

For example, the label `fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)` has the following attributes:

* `package` gives back `buck2/hello`
* `name` gives back `world`
* `sub_target` gives back `None`
* `path` gives back `fbcode/buck2/hello`
* `cell` gives back `fbcode`
* `raw_target()` gives back `fbcode//buck2/hello:world` without the configuration

## Type `artifact`

An artifact, which has a location on disk. Some of that location is considered private, and some (the suffix) is available for use.

The examples below assume an artifact such as one created with `ctx.actions.declare_output("hello/world.txt")`. It has the following attributes:

* `basename` gives back `world.txt`
* `extension` gives back `.txt`
* `is_source` - `True` if the artifact is a source, otherwise `False`.
* `owner` gives back a `label` representing the rule that created it (if it is a build output)  or `None` (if it is a source).
* `as_output()` gives a value suitable for setting as an output to `ctx.actions.run`.
* `short_path` gives back `hello/world.txt`

### Projected artifacts

Artifacts can be *projected* via the `project()` method. Projecting an artifact yields a path within it.

For example, if artifact `foo` is a directory containing a file `bar`, then `foo.project("bar")` yields the file `bar`.

It is possible for projected artifacts to hide the prefix in order to have the short name of the resulting artifact only contain the projected path, by passing `hide_prefix = True` to `project()`.
