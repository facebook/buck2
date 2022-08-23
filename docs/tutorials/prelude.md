The buck2 prelude is the 'standard library' for anyone using buck, and provides a common set of interfaces for the ecosystem to program against. It is configured as its own cell, and allows you to plug it in to any project using git submodules.

> Want to follow along? Clone the [starter repo](github.com/arlyon/buck2-starter)

When working with a buck2 project, there are certain concepts that are common to all projects, like downloading files, rules for interacting with a compiler, linking strategy, or run configuration. By extracting these out into a common core, we in a sense solve the M x N compiler problem, allowing for pluggable rules or toolchains, and with it the ability to swap out the backend (toolchain) or frontend (your build graph) at will, mixing and matching as required. So, how do we get started?

### The Prelude Cell

> `git checkout start`

First, lets create a new project.

```sh
> mkdir buck2-hello-world && cd buck2-hello-world && git init
```

`buck2` organises code into groups called 'cells'. These can be considered as the packages of buck, and allow compartmentalizing parts of the build definitions into reusable modules. One such module is the `prelude`, and it is imported by default if available or silently ignored if not found. Lets set up the prelude cell! It is available as a git submodule.

```sh
> git submodule add https://github.com/arlyon/buck2-prelude.git prelude
> touch BUILD .buckconfig
```

Every project (or cell) needs a `.buckconfig` file to signify that it is the root of a buck project, and our BUILD file is used to house our configurations. In the `.buckconfig` we specify the name of the BUILD files that buck should use and our project's cells. Lets try to build something.

`.buckconfig`

```toml
[buildfile]
name=BUILD

[repositories]
prelude = ./prelude
root = .
```

`BUILD`

```python
load("@prelude//:rules.bzl", "cxx_binary")

cxx_binary(
    name = "main",
    link_style = "static",
    srcs = ["main.cpp"],
)
```

`main.cpp`

```cpp
#include <iostream>

int main()
{
    std::cout << "Hello world" << std::endl;
}
```

Between these three files, we have a minimal definition of a C++ binary, using the `cxx_binary` rule provided to us by the prelude. The `load` syntax is fairly simple, and consists of an optional cell (denoted by the `@` symbol) followed by a path into that cell. If no cell is provided, then it defaults to whatever
cell the file is in.

Lets try to build.

[//]: # "todo(arlyon): this is not a nice error"

```sh
> buck2 build //... # syntax to build everything



Traceback (most recent call last):
  * prelude/apple/user/apple_resource_bundle.bzl:35, in <module>
      "_apple_toolchain": _get_apple_resources_tolchain_attr(),
  * prelude/apple/user/apple_resource_bundle.bzl:8, in _get_apple_resources_tolchain_attr
      return attrs.toolchain_dep(default = "platform//toolchain:apple-resources", p...
error: Type of parameters mismatch
 --> prelude/apple/user/apple_resource_bundle.bzl:8:12
  |
8 |     return attrs.toolchain_dep(default = "platform//toolchain:apple-resources", providers = [AppleToolchainInfo])
  |            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  |
Build ID: 373d5f1d-a94e-44b2-8f37-0e76270be17f
Jobs completed: 2. Time elapsed: 0.0s.
[2022-08-11T15:51:34.904+01:00] watchman fresh instance event, clearing cache
BUILD FAILED
```

So what is wrong? Unfortunately the prelude doesn't come with any toolchains, and our rules need one configured to be able to function correctly. These are conventionally stored in the `platform` cell.

### The Platform Cell

> `git checkout platform`

[//]: # "todo(arlyon): we should probably work to make the platform cell optional"

We can set up a minimal toolchain using providers, which is the `buck2` analog for an interface or typeclass. By programming against a set of providers, `buck2` remains decoupled from the execution. A toolchain is simply a rule that returns the set of (or a superset of) the providers required by the prelude's build rules. In the case of the `cxx_binary` rules, these are `CxxToolchainInfo` and `CxxPlatformInfo`.

How to supply these providers is up to the user (you) allowing to mix and match as required, an example in this case being switching between `clang` and `gcc`,
or switching out the linker. The `platform` cell is depended on by the prelude to define its toolchain definitions, so lets create it.

```sh
mkdir toolchain
echo "platform = ./toolchain\n" >> .buckconfig
touch toolchain/.buckconfig toolchain/BUILD
```

Does that looks familiar? We create the platform cell in exactly the same way as our 'root' cell. You will notice now that if you were to try to import things from `//toolchain` after defining it to be a cell, buck2 will error out. _Cells can contain other cells, but files in a child cell are hidden from the parent_. Lets continue onwards and build a toolchain!

`toolchain/.buckconfig`

```toml
[buildfile]
name=BUILD
```

`toolchain/BUILD`

```python
load("//:toolchain.bzl", "cxx_toolchain")

cxx_toolchain(
    name="cxx",
    visibility=["PUBLIC"],
)
```

In our build file, we'll expose a target with the name cxx. This is the name that buck2 uses as the default toolchain if the `_cxx_toolchain` parameter isn't supplied. In the `BUILD` we import a file `toolchain.bzl` which is what is going to house the toolchain's rules. What are rules? They are the building blocks of your build graph, and are composed together to define a build process. Rules can depend on other rules (either build rules, or toolchain rules) as well as define actions which you could imagine are the leaf nodes; actions are the actual commands that are executed while the rules are collections of commands to acheieve some purpose. Lets write a rule that describes our compiler toolchain so that the prelude may understand how to use the tools on your machine to perform its actions.

### Creating rules

> `git checkout prelude-rules`

So, a brief recap. Rules execute actions, and return providers. Lets put these concepts into practice and create a runnable rule that can build a simple cpp project without the prelude, and also populate the `RunInfo` provider such that it may be executed using `buck2 run`. You can get a feel for how these interact in the [no-prelude](no-prelude.md) guide.

### Toolchain rules

Toolchain rules are a special type of rule that is marked to be exclusively for the purpose of toolchain-specific providers. These typically do not contain any substantial actions (leaf nodes) but rather just generate configuration for use by proper rules. Lets run a build and slowly work through the issues as they crop up.

```sh
> buck2 build //...

When running analysis for `root//:main (<unspecified>)`

Caused by:
    0: when looking up configured node root//:main (<unspecified>)
    1: when looking up configured node platform//:cxx (<unspecified>) (<unspecified>)
    2: read_to_string(/home/arlyon/Programming/fbcode/buck2-starter/toolchain/toolchain.bzl)
    3: No such file or directory (os error 2)
Build ID: 258d9223-d775-4acc-891f-abdc67c0b342
Jobs completed: 4. Time elapsed: 0.0s.
BUILD FAILED

> touch toolchain/toolchain.bzl
> buck2 build //...

When running analysis for `root//:main (<unspecified>)`

Caused by:
    0: when looking up configured node root//:main (<unspecified>)
    1: when looking up configured node platform//:cxx (<unspecified>) (<unspecified>)
    2: error: Module has no symbol `cxx_toolchain`
        --> toolchain/BUILD:1:24
         |
       1 | load("//:toolchain.bzl", "cxx_toolchain")
         |                        ^^^^^^^^^^^^^^^

Build ID: 225450e0-b9f7-4a45-b439-65cb0fb49f7f
Jobs completed: 5. Time elapsed: 0.0s.
BUILD FAILED
```

Lets create a rule with the appropriate providers.

`toolchain.bzl`

```python
load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "CxxToolchainInfo",
    "CxxPlatformInfo",
)

def _cxx_toolchain(ctx):
    return [
        DefaultInfo(),
        CxxToolchainInfo(),
        CxxPlatformInfo(name="x86_64"),
    ]


cxx_toolchain = rule(
    impl=_cxx_toolchain,
    attrs={},
    is_toolchain_rule=True,
)
```

```sh
> buck2 build //...

When running analysis for `root//:main (<unspecified>)`

Caused by:
    0: when looking up configured node root//:main (<unspecified>)
    1: when looking up configured node platform//:cxx-hacks (<unspecified>)
    2: No target with name `cxx-hacks` in package `platform//`.
Build ID: f56f0713-749f-468c-9d92-d126712878fb
Jobs completed: 7. Time elapsed: 0.0s.
BUILD FAILED
```

It seems to be complaining about something called cxx_hacks. Lets substitute that in.

[//]: # "todo(arlyon): what do we do about cxx_hacks?"

`toolchain/cxx_hacks.bzl`

```python
def _cxx_hacks_impl(_ctx):
    return [DefaultInfo(), TemplatePlaceholderInfo(
        unkeyed_variables = {
            "cxx-header-tree": "/dev/null/HACK-CXX-HEADER-TREE",
            "output-dwo-dir": "/dev/null/HACK-OUTPUT-DWO-DIR",
        },
    )]

cxx_hacks = rule(
    impl = _cxx_hacks_impl,
    attrs = {},
)
```

And one more time...

```sh
> buck2 build //...

When running analysis for `root//:main (<unspecified>)`

Caused by:
    Traceback (most recent call last):
      * prelude/cxx/cxx.bzl:146, in <module>
          output, comp_db_info = cxx_executable(ctx, params)
      * prelude/cxx/cxx_executable.bzl:128, in cxx_executable
          compile_cmd_output = create_compile_cmds(
      * prelude/cxx/compile.bzl:177, in create_compile_cmds
          base_compile_cmd = _get_compile_base(compiler_info, ext)
    error: Object of type `NoneType` has no attribute `compiler`
       --> prelude/cxx/compile.bzl:332:20
        |
    332 |     cmd = cmd_args(compiler_info.compiler)
        |                    ^^^^^^^^^^^^^^^^^^^^^^

Build ID: 007a8fd6-db43-4d52-b7ae-fe7e2957d803
Jobs completed: 8. Time elapsed: 0.1s.
BUILD FAILED
```

From this point we can go through and fill out all the paths and parameters for our environment.

`toolchain/toolchain.bzl`

```python
load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "CxxCompilerInfo",
    "CCompilerInfo",
    "CxxToolchainInfo",
    "CxxPlatformInfo",
    "LinkerInfo",
    "BinaryUtilitiesInfo",
)

load("@prelude//cxx:headers.bzl",
    "HeaderMode",
)

load("@prelude//linking:link_info.bzl",
    "LinkStyle",
)

DEFAULT_MAKE_COMP_DB = "@prelude//cxx/tools:make_comp_db"

def _cxx_toolchain(ctx):
    """
    A very simple toolchain that is hardcoded to the current environment.
    """

    return [
        DefaultInfo(),
        CxxToolchainInfo(
            mk_comp_db=ctx.attrs.make_comp_db,
            linker_info=LinkerInfo(
                linker=RunInfo(args=["ld"]),
                linker_flags=[
                    "-dynamic-linker",
                    "/lib64/ld-linux-x86-64.so.2",
                    "/usr/lib64/crt1.o",
                    "/usr/lib64/crti.o",
                    "/usr/lib64/crtn.o",
                    "-L/usr/lib/gcc/x86_64-redhat-linux/12",
                    "-lstdc++",
                    "-lm",
                    "-lgcc",
                    "-lc",
                ] + [
                    "/usr/lib/gcc/x86_64-redhat-linux/12/crtendS.o",
                    "/usr/lib/gcc/x86_64-redhat-linux/12/crtbeginS.o"] if ctx.attrs.link_style == "shared" else [
                    "/usr/lib/gcc/x86_64-redhat-linux/12/crtend.o",
                    "/usr/lib/gcc/x86_64-redhat-linux/12/crtbegin.o",],
                archiver=RunInfo(args=["ar", "rcs"]),
                type="gnu",
                link_binaries_locally=True,
                archive_objects_locally=True,
                use_archiver_flags=False,
                static_dep_runtime_ld_flags = [],
                static_pic_dep_runtime_ld_flags = [],
                shared_dep_runtime_ld_flags = [],
                independent_shlib_interface_linker_flags = [],
                mk_shlib_intf = ctx.attrs.make_shlib_intf,
                link_style=LinkStyle(ctx.attrs.link_style),
                link_weight=1,
            ),
            bolt_enabled=False,
            binary_utilities_info=BinaryUtilitiesInfo(
                nm=RunInfo(args=["nm"]),
                ranlib = RunInfo(args=["raninfo"]),
                strip=RunInfo(args=["strip"]),
                dwp=None,
                bolt_msdk=None,
            ),
            cxx_compiler_info=CxxCompilerInfo(
                compiler=RunInfo(args=["clang++"]),
                preprocessor_flags=[],
                compiler_flags=[],
                compiler_type="clang",
            ),
            c_compiler_info=CCompilerInfo(
                preprocessor_flags=[],
            ),
            header_mode = HeaderMode("symlink_tree_only"),
        ),
        CxxPlatformInfo(name="x86_64"),
    ]


cxx_toolchain = rule(
    impl=_cxx_toolchain,
    attrs={
        "make_comp_db": attrs.dep(providers=[RunInfo], default=DEFAULT_MAKE_COMP_DB),
        "make_shlib_intf": attrs.dep(providers=[RunInfo], default=DEFAULT_MAKE_COMP_DB),
        "link_style": attrs.string(default="shared"),
    },
    is_toolchain_rule=True,
)

```

## Conclusion

> git checkout prelude-done

Congrats! From here you can either build or run your target (using the `RunInfo` provider), and also get for free the ability to build libraries, link statically or dynamically, and query the build graph. Now that you have your own toolchain, you can of course mix-and-match this with your own build rules. If you'd like an example of this, you can check out the [no-prelude](./no-prelude.md) guide.
