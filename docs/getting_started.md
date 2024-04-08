---
id: getting_started
title: Getting Started
---

## Installing Buck2

The latest set of `buck2` executables can be found under the
[`latest` release page](https://github.com/facebook/buck2/releases/tag/latest).

Additionally, for each bi-monthly release there is a
[dotslash](https://dotslash-cli.com/) file that is appropriate for checkin to a
repository. This will automatically fetch the correct version and architecture
for each user, and ensures a consistent build environment for each commit in the
repo.

To get started, first install [rustup](https://rustup.rs/), then compile the
`buck2` executable:

```bash
rustup install nightly-2024-02-01
cargo +nightly-2024-02-01 install --git https://github.com/facebook/buck2.git buck2
```

The above commands install `buck2` into a suitable directory, such as
`$HOME/.cargo/bin`, which you should then add to your `$PATH`:

Linux / macOS

```sh
export PATH=$HOME/.cargo/bin:$PATH
```

Windows Powershell

```powershell
$Env:PATH += ";$HOME\.cargo\bin"
```

With Buck2 installed, you can build projects with `buck2`!

### Windows configuration

Some of our rules use symlinks, which are disabled by default for non-admin
Windows users. You can fix that by
[enabling Developer Mode](https://pureinfotech.com/enable-developer-mode-windows-11/).

## Compiling your first project

This section covers the building of a
['hello_world' example project](https://github.com/facebook/buck2/tree/main/examples/hello_world)
that contains a simple C++ binary. If you are interested in seeing how other
languages can be built, take a look at the
[prelude example project](https://github.com/facebook/buck2/tree/main/examples/with_prelude),
which contains Rust, C++, Python, and OCaml targets.

First, clone the buck2 repository and cd into the 'hello_world' project:

```bash
git clone https://github.com/facebook/buck2.git
cd buck2/examples/hello_world
```

`buck2 init --git` is all the setup you need to start building. This will use
git submodule to pull [buck2-prelude](https://github.com/facebook/buck2-prelude)
into your project:

```sh
buck2 init --git
```

To use another version control system, run `buck2 init` and manually download
[buck2-prelude](https://github.com/facebook/buck2-prelude) into `prelude` at
root.

```sh
buck2 init
```

To build the entire project, run:

Note: _Requires clang and lld to be in the path_

```sh
buck2 build //...
```

Note that this uses a
[simple C++ toolchain](https://github.com/facebook/buck2/blob/main/prelude/toolchains/cxx.bzl)
that requires a recent version of `clang` to be installed on your system. This
can be installed with any package manager (ex. `apt install clang`,
`xcode-select --install` on macOS, `choco install llvm`). After installing any
external tools or changing your `PATH`, run `buck2 kill` before running a build.

To list all targets available in the project, run:

```sh
buck2 targets //...
```

To run the main C++ binary, run:

```sh
buck2 run //:main
```

The newly built binary can be found with the `--show-output` flag:

```sh
buck2 build //:main --show-output
```

Output:

```sh
Build ID: 0e890477-5b7f-4829-9ffe-662e572320a0
Jobs completed: 3. Time elapsed: 0.0s.
BUILD SUCCEEDED
root//:main buck-out/v2/gen/root/9f4d83578bb24895/__main__/main
```

## Creating your first hello_world project

This section demonstrates how to create a simple C++ 'hello_world' project.

To get started, make a new folder for your project and cd into it.

```sh
mkdir hello_world
cd hello_world
```

Next, run `buck2 init --git` to initialize the project. This command will set up
your project with `git` and pull in
[buck2-prelude](https://github.com/facebook/buck2-prelude) as a submodule.
Additionally, it will generate multiple files with default values.

```sh
buck2 init --git
```

Next, add the source code `main.cpp` ,

```c++
#include <iostream>
int main() {
    std::cout << "Hello from a C++ Buck2 program!" << std::endl;
}
```

Then, define a `cxx_binary` in the root `BUCK` file:

```Python
# BUCK
cxx_binary(
    name = "main",
    srcs = ["main.cpp"],
    link_style = "static",
)
```

If you try to build `//:main` at this point, you'll see an error about `buck2`
not being able to find `toolchains//:cxx`.

The final step is to define the necessary toolchain targets. For that project,
you need `system_cxx_toolchain` and `system_python_bootstrap_toolchain`, which
will pick up the necessary tools (clang++, python, and so on) from the system.

```Python
# toolchains/BUCK
load("@prelude//toolchains:cxx.bzl", "system_cxx_toolchain")
load("@prelude//toolchains:python.bzl", "system_python_bootstrap_toolchain")

system_cxx_toolchain(
    name = "cxx",
    visibility = ["PUBLIC"],
)

system_python_bootstrap_toolchain(
    name = "python_bootstrap",
    visibility = ["PUBLIC"],
)
```

At this point, your project should have the following files:

```bash
$ tree -a -I "buck-out|prelude|.git"
|-- .buckconfig
|-- .gitmodules
|-- BUCK
|-- main.cpp
`-- toolchains
    `-- BUCK
```

Now, you're ready to see the build in action.

To build the main C++ target, run:

```sh
buck2 build //:main
```

To run the main C++ target, run:

```sh
buck2 run //:main
```

In summary, a `buck2` project requires:

1. A `.buckconfig` file in the root which has a `[cells]` section listing out
   [cells](https://buck2.build/docs/concepts/glossary/#cell)
2. A `prelude` directory, which contains a collection of
   [rules](https://buck2.build/docs/concepts/glossary/#rule) of your choice.
   `buck2 init` will pull in the
   [buck2-prelude](https://github.com/facebook/buck2-prelude.git) as a git
   submodule by default
3. If using the [buck2-prelude](https://github.com/facebook/buck2-prelude.git),
   a `toolchains` directory that declares relevant toolchains. We provide some
   basic toolchains in
   [prelude/toolchains](https://github.com/facebook/buck2/tree/main/prelude/toolchains)
4. `BUCK` files that specify targets for your project

`buck2 init --git` will generate all of these with reasonable default values.

## Learning More

You should now be ready to explore Buck2 for use in your own projects. You can
explore the [examples](https://github.com/facebook/buck2/tree/main/examples)
folder. Look out for more tutorials in the future.

<FbInternalOnly>

## Communication channels

The following channels provide an insight into Buck2:

- [Buck2 Engineering](https://fb.workplace.com/groups/buck2prototyping) -
  Workplace group for discussions about what features Buck2 should have, how
  it's going, status updates, and much more.
- [Buck2 Users](https://fb.workplace.com/groups/buck2users) - Workplace group
  featuring questions from users and reports of bugs.
- [Buck2 Rule Authors](https://fb.workplace.com/groups/347532827186692) -
  Workplace group for discussions about language-specific rules.
- [Buck2 Oncall Hub](https://www.internalfb.com/intern/monitor/oncall_profile?oncall=buck2) -
  urgent tasks and escalation.

</FbInternalOnly>
