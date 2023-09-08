---
id: offline_build_archives
title: Offline build archives
---

Buck2 has the capability of producing so-called "offline build archives". These archives are [Antlir](https://www.internalfb.com/intern/staticdocs/antlir/) OS images which contain _every_ input necessary to build a buck target (or [fbpkg.builder](https://www.internalfb.com/intern/wiki/Fbpkg/fbpkg.builder/) target), including:
* Source files
* [gvfs](https://www.internalfb.com/intern/wiki/Gvfs/) artifacts
* [Dotslash](https://www.internalfb.com/intern/wiki/DotSlash/) executables; these are prefetched and cached in the resulting OS image.
* Buckconfig files from inside and outside the repository

## How to build an image

Offline build archives are primarily centered around producing OS images in which you can build an fbpkg in a fully-offline, low-Meta-dependency environment.

In order to build an image, you need to add a new rule definition along side your `fbpkg.builder` target. Assume you have a service + `fbpkg.builder` definition like so:

```[python]
load("@fbcode_macros//build_defs:cpp_binary.bzl", "cpp_binary")

# Import experimental fbpkg.builder using native buck2 rules
load("//fbpkg:fbpkg.bzl", "fbpkg")

cpp_binary(
    name = "my_service",
    srcs = [
        "main.cpp",
    ],
    deps = [
        "//some/dep:one",
        "//another/dep:two",
    ],
)

fbpkg.builder(
    name = "my.service",
    buck_opts = fbpkg.buck_opts(
        mode = "opt-clang-thinlto",
    ),
    path_actions = {
        "my_service": ":my_service",
    },
)
```

Add a new rule using the `offline_builder` rule type:
```
fbpkg.offline_builder(
    name = "my.service-offline",
    fbpkg = ":my.service",
)
```

The output of `//:my.service-offline` will be an fbpkg containing an Antlir container with all inputs - both from the repo as well as from the network - necessary to build that fbpkg.

## Building inside the image

The OS image will contain all input files necessary to produce the desired output artifact(s) in a no-network, low-dependency environment.

In every offline builder fbpkg, a simple tool called `offline_builder` is included. This is a simple python binary which makes it easier to work with offline build images.

The `offline_builder` utility has three commands:
1. `receive-image`: Unpacks the btrfs sendstream-v2 image that Antlir produces to an on-disk btrfs subvolume
2. `enter-image`: Invokes `systemd-nspawn` to drop you into a shell inside the container image.
3. `build-fbpkg`: Invokes `systemd-nspawn` with the appropriate command to build the fbpkg inside the container image.

NOTE: All `offline_builder` commands must be run with root permissions (e.g. with `sudo`).

The general process of working with an offline build image is the following:
```[bash]
# Fetch the offline builder fbpkg
$ fbpkg fetch my.service-offline:$hash

# Unpack the image on disk
$ sudo ./offline_builder receive-image
INFO:root:`btrfs receive`ing image '/tmp/offline/sendstream.v2' into '/tmp/offline/my.service-offline'
INFO:root:Received image to /tmp/offline/my.service-offline/e5b93112027f479eb30b2c35f831b1aa
INFO:root:Marking image '/tmp/offline/my.service-offline/e5b93112027f479eb30b2c35f831b1aa' as r/w

# Enter the image
$ sudo ./offline_builder enter-image
Spawning container e5b93112027f479eb30b2c35f831b1aa on /temp/offline/my.service-offline/e5b93112027f479eb30b2c35f831b1aa.
Press ^] three times within 1s to kill container.
[facebook@e5b93112027f479eb30b2c35f831b1aa ~]$

# Alternatively, you can also build the fbpkg directly from the host machine
$ sudo ./offline_builder build-fbpkg
...
```

### Working inside the image
`offline_builder enter-image` drops you into the container as the `facebook` service user. This user has various settings and configurations configured on login for buck2 and the new `fbpkg-build` entrypoint to work offline by default with no extra flags or configuration required.

There's a "checkout" of fbsource at `/home/facebook` (not a real source code repository, but enough to get builds working), as well as a cached dotslash directory with prefetched executables.

Inside the fbsource repository are one or more shell scripts to make it easier to perform offline builds:

```[bash]
[facebook@e5b93112027f479eb30b2c35f831b1aa fbsource]$ ls -l *.sh
-rwxr-xr-x 1 facebook root 113 Aug 30 06:21 build_fbpkg.sh
-rwxr-xr-x 1 facebook root 136 Aug 30 06:21 build_fbpkg_belljar.barservice_targets_mode-opt.sh
-rwxr-xr-x 1 facebook root 930 Aug 30 05:48 build_fbpkg_impl.sh

[facebook@e5b93112027f479eb30b2c35f831b1aa fbsource]$ cat build_fbpkg.sh
#!/bin/sh

exec /home/facebook/fbsource/build_fbpkg_impl.sh fbcode//belljar/blanks/barservice:belljar.barservice

[facebook@my_service_offline fbsource]$ ./build_fbpkg.sh
2023-08-08T11:58:01.709968604-07:00  INFO registry_build_utils::build_utils: Using repo found at cwd: /home/facebook/fbsource
Buck UI: https://www.internalfb.com/buck2/1dbe44a6-2089-46b5-b5fe-83ba82facf69
Jobs completed: 78265. Time elapsed: 50.2s.
Cache hits: 0%. Commands: 18 (cached: 0, remote: 0, local: 18)
BUILD SUCCEEDED
2023-08-08T18:59:00.958700Z  WARN buck2_client_ctx::cleanup_ctx: Async cleanup step 'sending invocation to Scribe' took 3.535555792s
{
  "x86_64": "/home/facebook/fbsource/buck-out/v2/gen/fbcode/29146bce1651974e/path/to/my_service_offline/__my_service_offline__/tree"
}
```

> NOTE: today, this only produces a tree of artifacts that represents **uncompressed** fbpkg contents. In the future, this will be updated to produce a fully compressed fbpkg that can be handed off to tupperware. The script above will print out the location of the built fbpkg

## Copy built package out of the container
When invoking `offline_builder build-fbpkg`, a special `offline-out` directory is created in the root of the extracted fbpkg. This directory will be bind-mounted with read/write permissions inside the container, and once the fbpkg build inside the container is done, all build artifacts will be recursively copied to this directory.

```[bash]
$ sudo ./offline_builder build-fbpkg
Spawning container e5b93112027f479eb30b2c35f831b1aa on /tmp/offline/my.service-offline/e5b93112027f479eb30b2c35f831b1aa.
Press ^] three times within 1s to kill container.
2023-08-30T09:20:14.962454825-07:00  INFO registry_build_utils::build_utils: Using repo found at cwd: /home/facebook/fbsource
Buck UI: https://www.internalfb.com/buck2/1847d785-0ebf-44d9-8831-a695e6d6dae0
Jobs completed: 76504. Time elapsed: 24:20.6s.
Cache hits: 0%. Commands: 27640 (cached: 0, remote: 0, local: 27640)
BUILD SUCCEEDED

$ ls -l ./offline-out
total 74M
-rwxr-xr-x 1 1000 users 74M Aug 30 09:44 my_service*
```

You can then copy these build artifacts elsewhere.

## Additional build modes

Iterating by building a full fbpkg can be slow, particularly if the service is built with very optimized but slow modes like `opt-clang-thinlto`. Instead, you can add additional modes in which we'll instrument a build and other inputs and configurations into the final container:

```[bash]
fbpkg.offline_builder(
    name = "my.service-offline",
    fbpkg = ":my.service",
    additional_modes = [
        "fbcode/mode/opt",
        "fbcode/mode/dev",
    ],
)
```

This results in a new shell script at the root of the container fbsource repository, one for each mode:
```[bash]
[facebook@my_service_offline fbsource]$ ls -l *.sh
-rwxr-xr-x 1 facebook users 167 Jul 20 10:07 build_fbpkg.sh
-rwxr-xr-x 1 facebook users 123 Jul 20 10:07 build_fbpkg_my_service_targets_mode-opt.sh
-rwxr-xr-x 1 facebook users 123 Jul 20 10:07 build_fbpkg_my_service_targets_mode-dev.sh

[facebook@my_service_offline fbsource]$ cat build_my_service_targets_mode-opt.sh
#!/bin/sh

exec buck2 build @fbsource//fbcode/mode/opt fbcode//my/service:my_service
```

These shell scripts by default build all targets specified in the `fbpkg.builder` target, but you can also invoke the `buck2 build` command accordingly.

## Recovering your service with an offline build image

Offline build images are provided to be able to **build a fully hermetic, reproducible build in a completely isolated environment** with no external dependencies. The primary scenario in mind is the need to "fix-forward" when Remote Execution or CAS are unavailable.

The expected process for working with and recovering services in an offline build image looks like the following:

1. Follow steps in ["How to build an image"](#how-to-build-an-image)
1. Enter the image with `sudo ./offline_builder enter-image`
1. Edit any files that need to be edited
1. Run your build with `./build_fbpkg_$name_targets_mode-opt.sh`
1. ??? (TBD)

NOTE: For faster incremental builds, **stay in the container**. Without this, the buck2 daemon will be killed on container exit, and it has to spend a ton of time rebuilding internal graph state.

## Implementation details

Several environment variables must be set to perform fully-offline builds. These are all set in user-level `.bash_profile` scripts (which take effect on login):
* `BUCK_OFFLINE_BUILD` must be set; this indicates to buck2 that it should only perform *local execution* and disable talking to the RE cache layer. Support is also built into `fbpkg-build` to use a fake fbpkg uuid and build token rather than allocating one from the fbpkg frontend service.
* `DOTSLASH_CACHE` is set to `/home/facebook/dotslash_cache`. This tells dotslash to look in this directory for prefetched dotslash executables.

There are also several buckconfigs that must be set to get buck2 working fully-offline:
* `buck2.file_watcher = notify` disables watchman; we don't need watchman during an offline build
* `buck2.use_network_action_output_cache = true` tells buck2 to use precached action outputs from `ctx.actions.download_file` to e.g. perform a HEAD request to figure out if a file needs to be redownloaded during the build. These action outputs are cached in `fbsource/buck-out/v2/offline-cache`.
