# Notes for developers

The [README](https://www.internalfb.com/code/fbsource/fbcode/buck2/README.md) is the first stop for developer information, including how to set up the project, build it and coding conventions. The expectation is that _every_ developer on Buck2 has read the README. This page includes tips and techniques for debugging, that are best-effort useful things.

## Buck2 commands

There are useful commands as they stand today, but the commands change regularly, so this section may be out of date.

* The main binary is `buck2`, a daemon which takes commands (e.g., `buck2 build ...`). To build a target from the `buck2` directory, `./buck2.sh build fbcode//one_world/cli/util/process_wrapper:process_wrapper`
* Evaluate interpretation speed of FB4a, `cargo build --bin=interpret --release && /usr/bin/time -lp $CARGO_TARGET_DIR/release/interpret --dir ~/fbsource fbsource//fbandroid/apps/fb4a: --stats`
* To run just one Rust test, do `cargo test -p starlark string_index` (where `starlark` is the project, and `string_index` is a substring of the test).
* To diagnose a stack overflow, `rust-lldb -- program args` then `run` (make it run) followed by `bt` (show the stack trace).
* To view the docs, do `cargo doc --open`, which will open a web browser. Often there are doc warnings, but we don't worry about those yet.

To test your macOS setup, you can try executing this command:

* `./buck2.sh build @fbsource//fbobjc/mode/buck2/linux fbsource//fbobjc/Libraries/FBLog:FBLog`

## Working with Starlark

* You might want to install the LSP support, which is described in [`starlark-rust/vscode/README.md`](https://www.internalfb.com/code/fbsource/fbcode/buck2/starlark-rust/vscode/README.md). Goto-definition doesn't work, but it has good lint support.
* There is an IDE debugger in prototype, but it's not really ready for mainstream use yet.
* For debugging, useful functions include `print` (write to stdout), `fail` (raise an error), `str` (convert a value to a string), `debug` (convert a value to a string including all details).
* There is a `breakpoint()` function which will open a text debugger, which can evaluate expressions and show the stack.
* Add `BUCK_LOG=info` to your env variables to print out all commands executed. See [here](https://docs.rs/tracing-subscriber/0.2.17/tracing_subscriber/filter/struct.EnvFilter.html) for the full power of the environment variable.
* If you want to run the Starlark linter without compiling it, run `~/fbsource/tools/lint/starlark/starlark_linter . --check` or similar.

## Running a Buck2 Version Bump

* The Buck2 version bump is a chronos job that runs once per day and updates metadata on which buck2 binary to fetch with all the landed development changes made since the last bump to all of Facebook, however, sometimes it is necessary to run a bump manually.
* To manually run a Buck2 bump, go to the chronos job page [here](https://www.internalfb.com/intern/chronos/search/?query=buck2_version_bump), click `Run Now` in the top right, and then `Run Job Instance` in the pop-up window.
* The bottom of the chronos job page will indicate the progress of the job. Further details can be accessed by clicking on the job's `Job instance ID`.
* Once the job finishes running any checkout later than the current master on which `buck2` is run will fetch the new buck2 binary and execute it.
* To disable/enable version bumps (e.g. during code freeze) go to the chronos job page and select "Disable job"/"Enable job" respectively.

## Working with Buck1

* To see the JSON encoded graph of a target run `buck query "deps('$TARGET')" --output-format json --output-attributes '.*'`
* To see the command line of a command, introduce an error in the file (e.g. syntax error for compiling, undefined symbol for linking) and run `buck build $TARGET -c modern_build_rule.strategy=remote -c remoteexecution.is_local_fallback_enabled=false -v 9 -c cache.key_seed=$RANDOM`
* To understand Starlark performance in Buck1 use `STARLARK_RT_STATS=~/stats.txt buck uquery //fbandroid/apps/fb4a:example_art_pgo`, which will produce a file `~/stats.txt`.

## Working with Remote Execution (RE)

There are two interesting RE values, namely a session (e.g. `reSessionID-9ddf9623-d94c-44cd-a3c5-9461ffdc2076`) and a command hash (e.g. `1c028ca8103c6e13348ae2b78b6941a41f836933:94`). You can interact with a command hash using the `frecli` tool (preinstalled on devservers). Some examples:

* `frecli exec action 1c028ca8103c6e13348ae2b78b6941a41f836933:94` runs the action remotely and tells me what files it required and produces.
* `frecli cas download-action 1c028ca8103c6e13348ae2b78b6941a41f836933:94 out` downloads the action to the `out` directory, where it can be executed (the output also says the command).

There are lots of other useful operations in `frecli`.

## Useful tools

* To observe the contents of a header map (`.hmap` files) do `/opt/chef/embedded/bin/ruby ~/fbsource/xplat/scripts/hmap myfile.hmap`.

## Debugging the Buck2 Daemon

You can attach to the Buck2 daemon using lldb to debug it interactively. For more information about the [daemon lifecycle, see the docs](https://www.internalfb.com/code/fbsource/fbcode/buck2/cli/daemon_lifecycle.md).

Cargo ships with `rust-lldb` which just invokes `lldb` and loads Rust-specific extensions to improve the debugging experience (e.g., formatting for variables).

Before executing `buck2.sh`, make sure that you've set the environment variable `DEBUG` to `1` (e.g., `export DEBUG=1`): this env variable is used by `buck2.sh` to determine the optimization level.

### Attaching to a Running Daemon

In this scenario, we want to debug the execution of a specific command. The first step would be to make sure there's an appropriate daemon which is running, so we can attach to it. For that, we can just execute any command that spawns a daemon (look for commands which implement the `StreamingCommand` trait).

For example:

1. Execute `./buck2.sh audit cell` which will spawn a buck2 daemon
2. Find the buck2 daemon PID using `./buck2.sh status` (or "Activity Monitor" or `ps x | grep "[b]uck2d"`). NOTE: Activity Monitor will _not_ display the customised `buck2d[REPO]` process name but instead will just show `buck2`.
3. Execute `rust-lldb`
4. Execute the following commands inside the `lldb` prompt:
    4.1 `attach PID`
    4.2 Set any breakpoints you would like.  E.g., `breakpoint set --file server.rs --line 1127`
    4.3 Resume the process execution using `process continue`
5. Execute the build command that you would like to debug.

#### Attaching to a Fresh Daemon

Sometimes you might want ot attach to the daemon very early in the startup cycle. The trick here is to suspend the daemon process until you resume it in `lldb`. Process suspension and resumption on Unix is achieved by sending the `SIGSTOP` and `SIGCONT` signals.

1. Find an appropriate place where you want to stop the daemon, so you can attach `lldb`.
2. Insert the following code `nix::sys::signal::kill(nix::unistd::Pid::this(), nix::sys::signal::Signal::SIGSTOP)?;` (remember to add `use nix;` at the top of the source file).
3. Run a command that will spawn a buck2 daemon (E.g., `./buck2.sh audit cell`). Note that the client command might fail depending on where you stop the daemon (e.g., if you stop it early enough, the client will never be able to spawn the daemon). That's okay - let the client fail.
4. Attach to the stopped daemon process as described in the section above.

#### Troubleshooting Debugging Info

The ability for `lldb` to show and print local variables depends on two aspects: availability of debug info and lack of optimizations. In `fbcode`, those settings are set by the mode files in `fbcode/mode` (e.g., `fbcode/mode/dev`), specifically via the `rust.rustc_flags` config key (look for `-Copt-level` and `-Cdebuginfo`). The source of truth is generated by `fbcode/tools/build/buck/gen_modes.py` (search for `RustMode` to find the source of truth for the relevant values).

Even in the dev mode (`fbcode/mode/dev`), optimizations are still enabled (level 1) which removes the ability for `lldb` to show useful information (e.g., printing local variables using `frame variable`). The reason to have the optimization level set to 1 in dev mode is that otherwise Rust code is so slow, that it becomes practically impossible to debug at an acceptable perf level.

There's an easy way to disable optimizations for a smaller set of source files: set the optimization flags on a per `rust_library()` level. If that level is not granular enough (e.g., too many source files which impacts performance), consider splitting into a smaller `rust_library()`.

To disable all optimizations for the source files in a specific `rust_library()`, add the following code to its definition:

```python
rustc_flags = [
    "-Copt-level=0",
],
```

By default, `-Cdebuginfo=2` would already have been applied via `rust.rustc_flags`, so it's not necessary but it's something you can also add if your symbols are still missing.

#### Troubleshooting No Breakpoints

If you cannot set a breakpoint and see a message similar to `Breakpoint 1: no locations (pending).`, that usually means that the debug symbols in the object files cannot be found. One common cause is that you've retrieved a cached version of the executable which points to Sandcastle object files.

When you attach to lldb, it will print out the path to the Buck2 binary (e.g., `Executable module set to "/Users/XXX/fbsource/fbcode/buck-out/opt/gen/03598924/buck2/cli/buck2#binary/buck2"`). To verify the paths to the object files which contain the DWARF data, execute the following command `dsymutil -s EXECUTABLE_PATH | grep N_OSO` and observe the paths - they should be local. For example, if they start with `/data/sandcastle/boxes`, that means your grabbed a (wrongly) cached executable.

To rectify the problem, make any change to the Rust code that will produce a different executable (e.g., change any strings, for example, `#[structopt()]` usages).

#### Attaching Using Visual Studio Code

Before you can debug using Visual Studio Code, make sure you've installed the CodeLLDB extension (consult main Buck2 [`README.md`](https://www.internalfb.com/code/fbsource/fbcode/buck2/README.md) for more details).

1. Modify any of the source files, so that a relink is triggered (e.g., `#[structopt()]` usages). See `Troubleshooting No Breakpoints` for rationale.
2. Add the relevant `opt-level` flags to the `rust_library` which contains the code you want to debug (see `Troubleshooting Debugging Info` for more info).
3. Set `remap_src_paths = no` in the `rust` section of `fbsource/tools/buckconfigs/fbsource-common.bcfg`
4. Execute `./buck2.sh audit cell` which will spawn a buck2 daemon
5. Go to the Debug tab (sidebar) in Visual Studio Code, select "Debug buck2d" from the dropdown and click the green play button next to it. Enter `buck2d` in the process filter and select the daemon process.
6. Wait for the debugger to attach, you will see a message `Attached to process XYZ` in the `DEBUG CONSOLE` tab.
7. The **first time** you set a breakpoint in **any specific file**, you need to do so manually. For example, execute `breakpoint set --file server.rs --line 1123` in the `DEBUG CONSOLE` prompt. You should see the breakpoint appear in the `BREAKPOINTS` section with a source path to `buck-out`. Subsequent breakpoints can be set by clicking on the file in the `BREAKPOINTS` section.
8. Execute the actual Buck2 command which you want to debug (e.g., `./buck2.sh build some//some:target`)

The reason that it's required to turn off remapping of source paths is that all source paths are relative to the `rust_library()` containing directory rather than a single root directory. This prevents the ability for CodeLLDB to find the sources for multiple packages.

If interested, you can also use the `relativePathBase` and `sourceMap` directives in `launch.json` (though this would only work for a single package).

## Working with Tpx

Buck2 spawns tpx to orchestrate test execution. Its output is silenced by default, but you can see it by setting `BUCK_LOG=buck2_build_api::test=debug`.

### Reproducing Build Failures Locally (macOS)

* Reproduce the error and note the action digest (e.g., `c2ca27584cec2cb4e1844376085b1942bbc1d9d3:94`)
* Execute `fbsource/xplat/remote_execution/dotslash/recli cas download-action DIGEST`
* The command will output a download directory, e.g., `/var/folders/l0/__fkmnl56v93dfsfn_25ky6m0000gn/T/recli/16e05f022eeb07fe12880d8d15c0bbec78c50727_234/hv6HQGjN97`
* `cd` into the download directory and execute the command output by `recli` in the earlier step
  * NB: The command printed by `recli` will _not_ wrap multiple arguments in quotes. For example, you might see `SRCS=x y z`, you will need to adjust the command, so that you execute `SRCS="x y z"` in your shell.

## DICE Cycle Detection

Currently, there is not yet default cycle detection build into Buck2. If you ever encounter deadlocks, you can explicltly enable cycle detection.

* The various binaries accept an optional `--detect_cycles` flag that you can set to `enabled`
* The dice daemon can be started with the environment variable `DICE_DETECT_CYCLES_UNSTABLE=enabled`

Note that this cycle detection control is unstable and will be removed in the future

## e2e Tests

* e2e tests use Buck1 to build and tpx to run tests. To pass any tpx argument to test_e2e.sh, you can run with `./test_e2e.sh <tpx argument>`.
  * To filter test, you can run with `./test_e2e.sh <test case name or filename>`. This filter allows partial match. For example, `./test_e2e.sh cxx` will run all tests with name `cxx`, which should just be `test_cxx` test case. `./test_e2e.sh build` will run all tests in test_build_isolated.py and test_build_inplace.py.
* To add a new buck command, you need to add that command as a function to [Buck](https://www.internalfb.com/code/fbsource/xplat/build_infra/buck_e2e/api/buck.py).
  * See example diff D28337167.
* If you find the Buck1 build to take a long time, consider rebasing to remote/fbcode/warm. This should have much better cache hit rate than other revisions.
* Tests can generate two versions:
  * A version that runs on the currently deployed buck2 binary (since the last VB).
  * A version that runs on the current buck2 repo.

    Typically, generating a test for both is reserved for rule tests, since rule code lives in repo.
    The flag that controls this behavior can be found [`here`](https://www.internalfb.com/code/fbsource/[0f186750c9350f343dbb40094cd32bb5e7712707]/fbcode/build_infra/buck_e2e.bzl?lines=65).

    If you create a new test which works for the current commit, but not for the current VB,
    you have two principal options for creating your test.
        - [Skip your test](https://fburl.com/code/r74x33bl) on the deployed repo.
          Then, when the next VB has landed, remove the skip decorator.
        - Do not add a test until the VB containing your change has landed.

    **Only skip the tests for VB if you are creating a new feature, new test, or your code will not be used in production.**
    In all other cases, you must fix the core feature to be backwards compatible with existing rules.

    The first option is preferred, but sometimes the second one is necessary.  If deployed buck2 is on fbcode TD and you add a new inplace test target using a new core feature, the test is going to fail TD.  The skip decorator does not help here.  See [this link](https://fb.prod.workplace.com/groups/buck2dev/permalink/3045792805708771/) for more information.

## Materializers

The materializer is the component of Buck2 that makes files created on RE available in `buck-out` on the local machine for utilization by actions or simply because the user requested them.

The materializer is configured using the `buck2.materializations` key in `.buckconfig`.

This is read at startup, so after changing it, the daemon needs to be restarted. Note that `-c` flags do not get passed to the daemon at startup, so you cannot use `-c` to set this: you _must_ update the config.

The configuration values we currently support are:

* `all`: materialize all files immediately as they become known to Buck.
* `deferred`: materialize files if they are requested by an action or were the top-level build target (i.e. the one the user typed in).
* `eden`: make files available in an Eden mount, and download them upon first access.

When using `deferred` materializations, the behavior for top-level build targets can be customized on a per-command basis via the `--materializations` flag when building.
Notably, `--materializations=none` won't download any of the outputs (but it'll still download intermediary files if they are necessary during the build).

## Oncall

### Oncall Tasks

* To see Oncall Hub page for Buck2, bunnylol `oncall buck2`.
* A list of currently open tasks is visible under the `Tasks` section.

### Version Bump

Buck Version Bump (VB) releases a new version of buck2 to developers by updating the version of a .buck2 file in a diff.
You can see the status of Buck2 VBs and any incoming VB diffs using bunnylol `bvb`.
The [chronos job for Buck2 VB in fbsource](https://www.internalfb.com/intern/chronos/job?smc=chronos_gp_admin_client&jobname=buck2_version_bump_fbsource) runs every weekday to trigger a new VB diff.
Currently, buck2 unit tests, e2e tests, and buck2-demo CI are the only tests running on VB diffs.
If any buck2 CI fails on the VB diff, you can use [this scuba query](https://fburl.com/scuba/phabricator_diff_land_issues/qj3soe10) to find any diff that bypassed a buck2 CI job in the last week.
This will usually be faster than a manual bisect to identify the root cause of the test failure.

Note that the BVB will only release binaries if they've been built. Use the BVB
UI (bunnylol `bvb` to find) to find them.

Note that builds only trigger on diffs in fbcode/buck2.

### Building Buck 2 manually

If Sandcastle cannot build Buck 2, you might have to do it yourself. On a devserver, run `buck2/scripts/buck2_upload.sh`.

On a laptop, you have some set up to do:

* Build the upload script: `buck build @mode/opt-mac buck2/deploy:buck2_upload --show-output`
* Install the Manifold CLI: `sudo feature install manifold`
* Then run the binary you build earlier: `buck-out/gen/.../buck2/deploy/buck2_upload.par --schedule_type master`

### Investigating Buck2 event ingress

Buck2, when configured to do so (i.e. operating within Facebook), echoes its event stream to the Scribe category `buck2_events` during
normal operation. This Scribe category is tailed by the Stylus tailer [build_infra/buck2_ingress](https://fburl.com/svc/pb828n8h). See
[`fbcode/buck2/facebook/ingress/README.md`](https://www.internalfb.com/code/fbsource/fbcode/buck2/facebook/ingress/README.md) for more
information on building, running, and debugging the ingress tailer.

Since Scribe requires that data flowing through it be UTF-8-encoded strings, Buck2 serializes its event stream as base64 prior to sending
it through scribe. The `buck2tail` binary (`fbcode/buck2/facebook/buck2tail`) can be used in a pipeline in order to deserialize and read
the event stream as JSON (e.g. `ptail -f buck2_builds | buck2tail`).
