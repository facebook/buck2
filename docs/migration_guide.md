# Migration Guide

Buck2 is supported in `fbcode` on Linux for existing Buck v1 projects using the languages C++, Rust, Python, OCaml and Go.
Keep in mind that these are early days and new issues may be uncovered as more projects make the jump.
Early adopters can use this guide to migrate from `buck` to `buck2` and we will work to resolve issues as they arise.
To try Buck2 simply type `buck2` instead of `buck`. For example, to build the Remote Execution `frecli` tool, before I would type:

```shell
buck build fbcode//remote_execution/rust/recli:recli
```

Whereas now I type:

```shell
buck2 build fbcode//remote_execution/rust/recli:recli
```

## Playbook

We recommend the following steps to migrate an `fbcode` project to Buck2:

1. `buck2 build @mode/<mode> <//your/target>` - if that fails read the [_Build Migration_](#build-migration) section of this document.
2. Set up a Contbuild using Buck2, without running tests.

    ```python
    "buck_version": "both"
    "buck2_overrides": {
      "owners": ["<your username - not oncall>"],
      "run_tests": False,
    },
    ```

    Alternatively, you may wish to create a separate Contbuild to build fewer targets, in which case use `"buck_version": "v2"`. See [_Contbuild integration_](#contbuild-integration) for more details.

3. Use the [self serve dashboard](https://fburl.com/unidash/bym1i8an) to compare build success and speed of Buck1 vs Buck2.
4. `buck2 test @mode/<mode> <//your/test>` - if that fails, read the [_Test Migration_](#test-migration) section of this document.
5. Use `buck2 run @mode/<mode> <//your/binary>` and convert any dev scripts to using Buck2 - if that fails, read the [_Workflow Migration_](#workflow-migration) section of this document.
6. Enable tests in Contbuild, by removing `"run_tests": False` from the config.
7. Verify proper packaging of your code with Buck2, read the [_Productionisation_](#productionisation) section of this document.
8. Write a workplace group on whatever group the developers of your project use suggesting they use Buck2.
9. Make Buck1 print out a "Try Buck2" recommendation by adding your project folder to [fbcode/.buckconfig](https://fburl.com/code/cwvrhc7g) (example [D36635354](https://www.internalfb.com/diff/D36635354)).
10. (Optional) tools and IDE migration:
     * setup pyre configuration to use buck2 ([examples](https://fburl.com/code/onkpc52p))
     * setup OD to use buck2 if you are using OD Flavor. Read the [_On Demand Migration_](#on-demand-migration) section for more details.
11. Move production to use fbpkgs built with Buck2.
12. Opt-in developers to GK [use_buck2_as_default](https://www.internalfb.com/intern/gatekeeper/projects/use_buck2_as_default/) or join [this group](https://fb.workplace.com/groups/984932495780948) to have `buck` command using `buck2` instead of `buck1`. Read the [_Development Opt-in GK_](#opt-in-gk-for-buck-using-buck2) section of this document. To use Buck1 either use `buck1` explicitly or set the environment variable `NO_BUCK2=1`.
13. When you are ready to make the migration permanent, remove the Buck1 Contbuild config.
14. Add your repo to this [SITEVAR](https://www.internalfb.com/intern/sv/BUCKV2_SUPPORTED_REPOS/) to enable BuckV2 in VS Code's C++ extension.

If you get stuck, or anything in this document is unclear, ask on [Buck2 users](https://fb.prod.workplace.com/groups/buck2users). Some migration steps may require a reasonable level of fluency with Buck or other fbcode infrastructure, so we are happy to help!

## Build Migration

The main reason that builds fail are due to Buck2 checking properties that are necessary for Buck1 to be correct, but that Buck1 doesn't check. In this section we'll go through some of the common causes, what the underlying issue is, and how to remedy them.

### Implicit Dependencies

Buck2 executes most commands in a sandbox (using remote execution), so dependencies that are not declared are not available. As a consequence, you may receive an error that the command didn't find a file. The solution is to fully specify all dependencies. In addition, there are three changes in how Buck2 looks for dependencies, relative to Buck1.

#### Fewer `glob` results

In Buck2 a `glob` will _only_ list files within the package. For a full explanation see [package boundary violations](https://fb.workplace.com/groups/buck2prototyping/posts/2631648140466003), and for lots of examples of how to fix the issue [see this post](https://fb.workplace.com/notes/4648083995302731). As a brief summary, given the folder structure:

```text
TARGETS
foo/
  a.h
bar/
  TARGETS
  b.h
```

If the outer `TARGETS` file (or `BUCK` file) has `glob(["**/*.h"])` in Buck1 it will include `a.h` and `b.h`, while in Buck2 it will only include `a.h`, because `b.h` is contained within the "package boundary" of `bar/TARGETS`. One common way these errors appear is as a missing C++ header error.

The simplest way to avoid such issues is to explicitly list out the files in question, avoiding the use of `glob`. Alternative approaches are to restructure the code, e.g. adding additional `cpp_library` targets (see [D30582787](https://www.internalfb.com/diff/D30582787)) or using `export_file`.

#### Directory/symlink dependencies

Buck2 does not support dependencies that point at symlinks or directories (the one exception is for symlinks that point at GVFS). There are two general approaches to solving the issue:

* For _symlink dependencies_, you should remove the symlink from the source repo. The way to share files in a way that Buck is aware of is by using `export_file` to export the file, then refer to the file by its target name.
* For _directory dependencies_ often replacing the directory `["foo"]` with `glob(["foo/**"])` - i.e. the list of files in `foo` - will give an equivalent build.

#### Changed working directory

In Buck1 most commands are run with a _cell relative_ working directory - e.g. in `fbsource/fbcode/remote_execution` the working directory would be `fbsource/fbcode`. In Buck2, that changes to be a _project relative_ working directory - e.g. the above example would be `fbsource`. Things like `#include` expressions where the header paths are not fully specified may work in Buck1 but fail in Buck2 - usually adding more header dependencies will solve the issue.

### Changes to `genrule`/`custom_rule`

With Buck2 there are two main changes to the `genrule` mechanism (often used via the `custom_rule` wrapper) and a few more minor ones:

1. Commands are run in a sandbox with no access to undeclared dependencies, network access or locally installed tools. These restrictions increase cachability and reproducibility, so are strongly encouraged. Where meeting these restrictions is not feasible (e.g. use of `hg`), rules can be opted into local execution by adding `labels = ["my_reason_for_running_locally"]` to the `genrule` and adding the exception to [genrule.bzl](https://www.internalfb.com/code/fbsource/fbcode/buck2/prelude/genrule.bzl) under `_LOCAL_LABELS`.
2. Commands are run with relative paths for things like `$SRC` and the results of `$(location)` expansion. Where these restrictions cause problems, rules can be opted to running with paths relative to the project root (instead of relative to `$SRCDIR`) by adding `labels = ["my_reason_for_running_relative_to_root"]` to the `genrule` and adding the exception to [genrule.bzl](https://www.internalfb.com/code/fbsource/fbcode/buck2/prelude/genrule.bzl) under `_BUILD_ROOT_LABELS`.
3. The `$GEN_DIR` environment variable is not available. This environment variable allows `custom_rule` with [`strict=False`](https://fb.workplace.com/groups/fbcode/permalink/1188158247887679/) to access the `fbcode` directory, which does not work with Buck2. If you see your build mention `GEN_DIR_DEPRECATED` in its error message, you're affected by this.
4. The algorithm for generating file names when a list is used as the `srcs` attribute is significantly simpler, which may sometimes result in name clash errors (you will see `genrule srcs include duplicative name`). If you see such clashes, use the dictionary `{name: artifact}` to provide explicit names.
5. In Buck1, symlinks in `genrule` outputs are _followed_, but in Buck2, they are cached as symlinks, which may require fixes (e.g. [D29739337](https://www.internalfb.com/diff/D29739337), [D29766735](https://www.internalfb.com/diff/D29766735)).

Some common changes that resolve the issues above include:

* Rules which use Protobuf codegen should move to using `protobuf_library` (e.g. [D29629913](https://www.internalfb.com/diff/D29629913), [D29629514](https://www.internalfb.com/diff/D29629514)).
* Rules using `dewey_cat` should be ported to [`manifold_get`](https://www.internalfb.com/intern/wiki/DeweyGuide/Migration_Guide/To_Manifold/#downloading-via-buck-in).
* If your rule uses DotSlash binaries these require network access and must currently be run locally, although changes are afoot to enable them to allow DotSlash within the sandbox in future.
* The use of relative paths means that `cd` within the command can invalidate these paths causing missing path issues (e.g. [D29793894](https://www.internalfb.com/diff/D29793894)). Using `realpath` before the `cd` can solve these issues.

Usually it is possible to solve these issues in a way compatible with both Buck1 and Buck2, but if not, you can use the `is_buck2()` funbction which returns `False` in v1 and `True` in Buck2:

```python
load("@fbsource//tools/build_defs/buck2:is_buck2.bzl", "is_buck2")
```

Don't use `is_buck2()` to hide targets or change deps since that will create lots of differences between buck and buck2 query. See [this post](https://fb.prod.workplace.com/groups/buck2prototyping/permalink/2812135695750579/) for more details.

### Platform vs Host differences

In `fbcode` with Buck1 there is a single platform (e.g. platform-009, platform-010) that is used for all targets in the build. In Buck2, there is a separate host platform (the platform the build is running on) vs target platform (the platform you want to run the result on). The target requested on the command line will be built using whatever mode is chosen either on the command line with `-c fbcode.platform=platform009` or in the [repo configuration](https://www.internalfb.com/code/fbsource/fbcode/PLATFORM.bzl), which is the target platform. As soon as a dependency makes a _host transition_ it will switch to the host platform, which currently is platform-010. A host transition happens when you follow a host-dependency (typically for toolchains) or you use `$(exe my_dependency)`, as these are usually things that have to be run while the build is happening, and thus must be on a platform compatible with the build. If the `$(exe my_dependency)` dependency should actually be built with the target platform, use `$(exe_target my_dependency)` instead, which will stick to the same platform as the target.

Unfortunately, because `fbcode` with Buck1 doesn't use separate host and target platforms, some targets will not use the proper `$(exe_target ...)` macro and need to be adjusted. Once these details are correct throughout `fbcode` we'll be able to enable new experiences, like cross-compiling and greater `xplat` compatibilty.

### Other changes

* The `export_file` rule in Buck v1 sometimes uses the basename of the exported files and sometimes the short name -- it depends on how the exported file is consumed. For example, exporting `foo/bar.txt` will give the name `bar.txt` in a Buck1 `genrule` and `foo/bar.txt` in Buck2. There are three solutions: 1) use `export_file` on a `src` with no slashes; 2) use `mode = "copy"` in `export_file` and an explicit `out` attribute; 3) for `genrule` use an explicit `srcs` dictionary, e.g. change `srcs = [":foo"]` to `srcs = {"foo": ":foo"}`.
* In Buck v1, if you are using user providers, a value not present is considered missing (`hasattr` returns `False`), whereas in Buck2 that field is set to `None` (`hasattr` returns `True`).
* There are no "flavors" in Buck2, where a flavor is a target modified by appending `#something`. For example, Rust binaries could be checked (not compiled, just type checked) with `library#check`, whereas in Buck2 that is `library[check]` (using the named targets syntax). Where libraries are depended upon using flavours for stripping or platform transition those should use [configuration mechanisms](configurations.md) instead.
* The C++ link order in Buck v1 and Buck2 is unspecified, but for some C++ projects there are duplicate symbols defined, where v1 happens to pick a link order that works but v2 doesn't. In such cases the duplicate symbols should be fixed properly so either link order works, usually by renaming one symbol.
* When run on Remote Execution, input files will be hard links, and identical inputs will link to the same underlying file. Most tools are oblivious to hard links, but some such as `tar` may require flags such as `--hard-dereference`, and `cp` may require `--dereference`.
* FBCode CI code coverage (diff and full) is currenly disabled for buck2 and planned to be supported in H2 2022. (T122036419)
* Python needed_coverage is currently not collecting data and silently failing and planned to be supported in Q3 2022. (T122359235)
* FBCode multisect logic is currently not supported and planned to be supported in Q3 2022. (T122360065)

### Historical notes

These are issues that we think we have fixed throughout the repo, so should no longer be encountered.

* Targets files must use Starlark, not Python - e.g. no `f"strings"`, float, globals etc. In particular `type(x)` in Starlark returns a string, so `type(x) == type("")` is the recommended compatible way to work with Starlark and Python.
* We are more strict about parsing macros, e.g. `$(location target` without a closing bracket used to be accepted, but isn't anymore.
* If you depend on a source file with a `../` prefix it will be an error.

### Keep going mode

In order to quickly get as many build errors as possible one workflow is to use the keep-going feature, which produces all errors. The recipe is:

* `buck2 kill` - first kill the daemon.
* `BUCK2_KEEP_GOING=my_output_file.txt buck2 build my_target` - run the command of interest.
* `buck2 kill` - stop the daemon, which will be in an undefined state after a keep-going run.

Afterwards you can consult `my_output_file.txt` for all the errors that occured. Note that `my_output_file.txt` will be relative to the root of the project, not where you ran `buck2` from.

The keep-going mode is considered experimental and is expected to change in future.

## Run Migration

There are a few differences between Buck 1 and Buck 2 that can cause differences when running code - both tests (see below) and normal execution.

* C++ files are compiled relative to the project root (e.g. `fbsource`) rather than the cell root (e.g. `fbcode`). As a consequence, the `__FILE__` preprocessor macro will have a leading `fbcode/` prefix when building with Buck2.
* While not required, we encourage greater use of `buck2 run`. With `buck` the `run` command could be quite slow, so people often bypassed it, by compiling and then executing directly from `buck-out`. With `buck2` the `run` command is fast (often milliseconds) so is encouraged.
* By default Buck2 only downloads the files that are direct runtime dependencies of the terminal target, so if your binary invokes another binary or loads files, these should be declared as runtime dependencies (see the next section).

### Runtime dependencies

In Buck1 there were functions to get rule output paths (e.g. `FbcodePaths::getRuleOutputPath()` in C++ and `pathutils.get_build_rule_output_path()` and Python), but these don't work either with hashed buck-out in Buck1, or ever with Buck2, and will fail at runtime. There are two supported mechanisms:

* _Resources_ depend on files, then use language specific APIs for retrieving them at runtime. Resources are supported in a few languages including [C++](https://www.internalfb.com/intern/wiki/Buck-users/C++_Resources/) (e.g. using [`build::getResourcePath`](https://www.internalfb.com/code/search?q=repo%3Afbcode%20build%3A%3AgetResourcePath)) and [Python](https://www.internalfb.com/intern/wiki/Buck-users/Python_Resources_in_fbcode/) (as shown in [D27072737](https://www.internalfb.com/diff/D27072737)). If you wish to depend on an executable, including all its shared objects etc., this mechanism may not be powerful enough.
* Using `command_alias` you can declare a runtime dependency in the `args` or `env` attributes, using either `$(location ...)` or `$(exe_target ...)`, which will ensure the files are available at runtime and pass them to the command.

## Query Migration

In Buck1 there is `uquery` (unconfigured query), `cquery` (configured query) and `query` (somewhere in between). In Buck2, `query` is an alias for `uquery`, so some uses of `query` may need to switch to use `cquery` explicitly.

### Cell qualification

In Buck2 all attributes of type target are qualified with their cell, so the `deps` attribute will contain values like `cell//package:target`, whereas you might see `//package:target` in Buck1.

This difference can be observed with operations such as `buck2 query "attrfilter(deps, //package:target, cell//...)"`, which works in Buck1 (if run from inside the `cell` directory), but in Buck2 won't match anything, as `//package:target` is compared as a string for equality, and lacks the leading `cell//`.

Furthermore, when using `cquery` or `attr.query` (e.g. a query inside of a `genrule`), attributes of type target will include the configuration - i.e. you might see `cell//package:target (my_configuration)`.

In order to express this filter in a way that is Buck1 and Buck2 compatible, you can use `attrregexfilter(deps, '//package:target($| )', cell//...)`, where `//package:target($| )` is a regex asserting the target string ends with `//package:target`, rather than is precisely equal to it.

### Target universes

Buck1 `cquery` always infers a target universe, whereas Buck2 `cquery` doesn't. The result can be that Buck1 produces multiple configurations per target, while Buck2 only produces one. To gain compatibilty, the easiest way is to often specify an explicit `--target-universe`, following the documentation on [this post](https://fb.prod.workplace.com/groups/devx.ci.bffs/permalink/616830502778501/).

## Test Migration

There are a few differences between Buck 1 and Buck 2 that can require changes in tests:

* The layout of `buck-out` has changed (and so has its location: it's also under the repository root instead of the cell root).
* The path to files when a build is ongoing is also relative to the repository root. This can impact e.g. log paths (since your file paths will now include the `fbcode` prefix).

You should ideally just make your tests oblivious to this. In particular:

* Do not access the repository from your tests. Instead, use e.g. a `filegroup`.
* Only access resources in `buck-out` using paths provided by Buck (e.g. via an `$(exe ...)` macro).

Those changes would also be necessary for a future where Buck 2 runs tests on remote execution.

If you get an error about cannot find external files at test/run time see [this section about runtime dependencies](#runtime-dependencies).

## Workflow migration

* In Buck2 all output files are relative to the project root (e.g. `fbsource/buck-out`) rather than the cell root as in Buck1 (e.g. `fbsource/fbcode/buck-out`).
* In Buck2 all output paths include a hash of the current configuration, for example `buck-out/v2/gen/fbcode/8107dfcc75db0fad/foo/libbar.so` contains the hash `8107dfcc75db0fad`. Such files can _also_ be found at `buck-out/v2/gen/fbcode/foo/libbar.so` (without the hash), due to a compatibilty option that is enabled. However, we would encourage using `buck2 run` or `buck2 build --show-output` to obtain the location of the files rather than hardcoding their location, as their unhashed location will change in future.
* In Buck2 building a library target (e.g. one defined with `cpp_library`) causes the target to be compiled, which is not the case for Buck v1 (i.e. Buck v1 will not notice syntax errors or such). That can cause differences when doing `buck2 build fbcode//myproject:my_library_target`, or when doing patterns that include `my_library_target` such as `buck2 build fbcode//...` or `buck2 build fbcode//myproject:`.

## Contbuild migration

Control the version of buck used in contbuild by setting the `buck_version` config option, e.g. [D29913271](https://www.internalfb.com/diff/D29913271):

```python
    "buck_version": "both",  # Build with both buck1 and buck2.
```

Valid settings are `v1` (default), `v2` or `both`. Using `both` setting will spawn 2 parallel contbuild jobs for `v1` and `v2`.
You may also wish to use `buck2_overrides` to customise the oncall or other attributes when using `both`, e.g.:

```python
    "buck2_overrides": {
      "owners": ["<username>"],
      "run_tests": False,
    },
```

## Productionisation

There are two ways of using fbpkg:

1. (new) fbpkg.builder
2. (old) configerator based fbpkg config

For both ways you will need to configure buck version on the fbpkgs configuration itself (in the build rule or in configerator). See bellow how to configure each.

NOTE: If you configure your fbpkg to build with buck2, it will deploy to production code that was built with buck2.

Test your fbpkg with buck2 in CI by creating two seperate fbpkg configs/build rules. Then use buck_overrides contbuild config option to choose which fbpkg is used for buck2 contbuild execution.
* Conveyor Hotfix corner case : Hotfix feature doesnt support 2 contbuilds. Thus if you would like to use 2 fbpkgs(v1, v2) in conveyor you need to build those in a single contbuild. example: D37082297 .
```python
"buck2_overrides": {
      "owners": ["<username>"],
      "run_tests": True,
      "fbpkgs": {
        "some.fbpkg.config.buck2": [],
      },
},
```

### fbpkg.builder integration

The Buck version used by `fbpkg.builder` is configuerd on the build rule itself. To switch it to Buck2 use `version="v2"` parameter. (examples: D35663292)

```python
fbpkg.builder(
    name = "...",
    ...
    buck_opts = fbpkg.buck_opts(version = "v2"),
)
```

Valid settings are `"v2"`, `"v1"` (default).

Buliding the rule with buck1 or buck2 affects only the fbpkg script itself and doesnt apply to the build system used by the fbpkg binary.
Thus building with buck2 an fbpkg that wasnt configured to use buck2, will invoke buck1 to build the actual code.

### fbpkg integration

The Buck version used by `fbpkg` is controlled by Configerator. To switch it to Buck2 use the `buck_version` parameter in the fbpkg configerator config:

```python
  buck_fbpkg(buck_version="v2", ...)
```

Valid settings are `"v2"`, `"v1"` (default).

In case you would like to explicitly use buck1 you can similarly set the `use_buck1` parameter to True. The resulted materialised configerator config will contain `buck1` command instead of `buck`. (Example: D35914012)

```python
  buck_fbpkg(use_buck1=True, ...)
```

The `use_buck2` flag has presedence over `use_buck1`.

### Packman integration

Migrate your packman config to use Buck2 by updating it to select the `buck2` builder type.
You might need to change the rules by removing prefix "//" (example: D34769694).

```shell
$ hg diff
diff --git a/fbcode/fixmyserver/packman.yml b/fbcode/fixmyserver/packman.yml
--- a/fbcode/fixmyserver/packman.yml
+++ b/fbcode/fixmyserver/packman.yml
@@ -5,7 +5,7 @@
     change_log_paths:
       - fixmyserver
     rules:
-      buck:fixmyserver:fixmyserver_xar:
+      buck2:fixmyserver:fixmyserver_xar:
         fixmyserver/fixmyserver_xar/fixmyserver.xar:
           path: /usr/local/bin/fixmyserver.real
     static:
```

NOTE: If you use the `par:` syntactic sugar (i.e. you have a `par:` key under `rules:`), that does not yet (as of 2022-03-01) work with Buck 2. You will need to expand
your par directives into full rules. See D34559280 for an example.

Test the change by doing

```shell
packman build --build-local <path-to-packman.yml>
```

### DotSlash integration

The helper functions in `lib/dotslash.py` accept a `buck_cmd` parameter that can be specified in your `.dotslash.py` config files. Supported values from the `dotslash.BuckCommand` enum are `BUCK` and `BUCK2` (default).

Existing configs can be migrated with a change like:

```shell
$ hg diff
diff --git a/fbcode/tools/bloatfinder/bloatfinder.dotslash.py b/fbcode/tools/bloatfinder/bloatfinder.dotslash.py
--- a/fbcode/tools/bloatfinder/bloatfinder.dotslash.py
+++ b/fbcode/tools/bloatfinder/bloatfinder.dotslash.py
@@ -7,5 +7,5 @@
     target="//tools/bloatfinder:bloatfinder",
     oncall="fbcode_build_infra",
     generated_dotslash_file="fbcode/tools/bloatfinder/deploy/bloatfinder",
-    buck_cmd=dotslash.BuckCommand.BUCK,
+    buck_cmd=dotslash.BuckCommand.BUCK2,
 )
 ```

You can test the changes by adding a comment like `#msdkbuild <name>` on your diff where the `name` can be found in the generated dotslash file for your config. This will attach a sandcastle job to your diff signals and you can inspect the associated build logs to verify that Buck 2 was used successfully. See D37525965 for an example.

## Opt-in GK for buck using buck2

By default without opting in, `buck` command is using `buck1`.

When all your code is working properly with `buck2`, you can move development to start using `buck2` by opting teams into the gk [use_buck2_as_default](https://www.internalfb.com/intern/gatekeeper/projects/use_buck2_as_default/). Such that `buck` command will be using `buck2`.

When opting teams or users into the GK, you can make sure that all their workflows build properly with buck2.
You can use the buck2_adoption.py script that suggests what additional contbuilds need to have a buck2 setup.
[Script guide](https://fb.workplace.com/notes/1064408477443910)

More details in the [post](https://fb.workplace.com/groups/buck2prototyping/permalink/2866678096963005/).

## On Demand migration

A new config is supported in On Demand flavor configs. You can choose which Buck version you like to pre-build the desired projects.

### How to use it

1. Open your flavor config, [an example](https://www.internalfb.com/code/configerator/[99b2a5bc84c5]/source/ondemand/flavors/de.cinc?lines=22), look at the `BuckBuildRequirement` setting.
2. Add buck_command parameter, it's an enum that has three options: BUCK, BUCK1, BUCK2.

    ```python
    PreparerRequirement(
    …
        BuckBuild=BuckBuildRequirement(
            ...
            buck_command=BuckCommand.BUCK2,
        )
    )
    ```

    1. BUCK is the default option, it points to the `buck` command and eventually links to the default version.
    2. BUCK1 points to `buck1`, explicitly uses buck 1.
    3. BUCK2 points to `buck2`, explicitly uses buck 2.

### How to verify

You can either check the on-demand preparer log (see the Test Plan of [D34917341](https://www.internalfb.com/diff/D34917341)), or examine the buck-out folder to verify your setting.

## Non `fbsource` repo

Buck2 inside the `fbsource` megarepo is deployed and updated by the Buck2 team. Outside of that, if you want to deploy Buck2, you need to copy some files over and keep them up to date. [D28634687](https://www.internalfb.com/diff/D28634687) serves as a nice reference for the changes that are required. Broadly speaking:

* You need a `.watchmanconfig` that ignores `buck-out`.
* You need a `.buck2` and `.buckversion` will tells the `buck2` command which version of Buck2 to use. You will want to bump this version periodically, to keep up with the latest bug fixes and improvements.
* You need a `.buckconfig` which says where the `fbcode` cell lives, and within that cell, you need `buck2/prelude/prelude.bzl` which contains the things that are loaded in to every `.bzl` or `TARGETS` file.
* You need to define the rules specific to your project. These can be copied from `fbsource`, or custom to your project. If copying from `fbsource`, bear in mind that one day you might choose to merge with `fbsource`, so an unmodified copy with changes submitted upstream to `fbsource` might be a wise choice. If you are writing your own rules, see [the guide to writing rules](writing_rules.md).
