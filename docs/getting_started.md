---
id: getting_started
title: Getting Started
---

# Getting started

## Installing Buck2

To get started, first install the `buck2` executable:

```
rustup install nightly
cargo +nightly install --git https://github.com/facebookincubator/buck2.git cli
```

That will install `buck2` into a suitable directory, e.g. `$HOME/.cargo/bin`, which you should then add to your `$PATH`.

```
export PATH=$HOME/.cargo/bin:$PATH
```

## Compiling your first project

Once it is installed, you can now build projects with `buck2`!

In this section, we will go over building the [hello_world example project](https://github.com/facebookincubator/buck2/tree/main/examples/hello_world), which builds a simple C++ binary. If you are interested in seeing how other languages can be built, check the [prelude example project](https://github.com/facebookincubator/buck2/tree/main/examples/prelude) which contains Rust, C++, Python, and OCaml targets.

First, clone the buck2 repository and cd into the hello_world project:

```
$ git clone https://github.com/facebookincubator/buck2.git
$ cd examples/hello_world
```

 `buck2 init` is all the setup you need to start building. This will pull in [buck2-prelude](https://github.com/facebookincubator/buck2-prelude) in to your project:

```
$ buck2 init
```

To build the entire project, run:

```
$ buck2 build //...
```

To list all targets available in the project, run:

```
$ buck2 targets //...
```

To run the main C++ binary, run:

```
$ buck2 run //:main
```

The newly built binary can be found with the `--show-output` flag:

```
$ buck2 build //:main --show-output
```

Output:

```
Build ID: 0e890477-5b7f-4829-9ffe-662e572320a0
Jobs completed: 3. Time elapsed: 0.0s.
BUILD SUCCEEDED
root//:main buck-out/v2/gen/root/9f4d83578bb24895/__main__/main
```

## Creating your first hello_world project

In this section, we’ll demonstrate how to create a simple C++ Hello World project.

To get started, make a new folder for your project and cd into it.

```
$ mkdir hello_world
$ cd hello_world
```


Next, run `buck2 init` to initialize the project. This command will set up your project with `git` and pull in [buck2-prelude](https://github.com/facebookincubator/buck2-prelude) as a submodule. Additionally, it will generate multiple files with default values.

```
$ buck2 init
```

Now let’s add our source code `main.cpp` ,

```
#include <iostream>
int main() {
    std::cout << "Hello from a C++ Buck2 program!" << std::endl;
}
```

Then, define a `cxx_binary` in our root `BUCK` file:

```
# BUCK
cxx_binary(
    name = "main",
    srcs = ["main.cpp"],
    link_style = "static",
)
```

If you try to build `//:main` at this point, you will see an error about `buck2` not being able to find `toolchains//:cxx`.

As a final step, let’s define the necessary toolchain targets. For this project, we will need  `system_cxx_toolchain` and `system_python_bootstrap_toolchain`. These will pick up necessary tools (clang++, python, etc.) from the system.

```
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

```
$ tree -a -I "buck-out|prelude|.git"
|-- .buckconfig
|-- .gitmodules
|-- BUCK
|-- main.cpp
`-- toolchains
    `-- BUCK
```

Now we’re ready to see the build in action.

To build the main C++ target, run:

```
$ buck2 build //:main
```

To run the main C++ target, run:

```
$ buck2 run //:main
```

## Learning More

You should now be ready to explore Buck2 for use in your own projects. You can explore the [examples](https://github.com/facebookincubator/buck2/tree/main/examples) folder and look out for more tutorials in the future.


<FbInternalOnly>

## Communication channels

The following channels provide an insight into Buck2:

* [Buck2 Engineering](https://fb.workplace.com/groups/buck2prototyping) - Workplace group for discussions about what features Buck2 should have, how it's going, status updates, and much more.
* [Buck2 Users](https://fb.workplace.com/groups/buck2users) - Workplace group featuring questions from users and reports of bugs.
* [Buck2 Rule Authors](https://fb.workplace.com/groups/347532827186692) - Workplace group for discussions about language-specific rules.
* [Buck2 Oncall Hub](https://www.internalfb.com/intern/monitor/oncall_profile?oncall=buck2) - urgent tasks and escalation.

</FbInternalOnly>
