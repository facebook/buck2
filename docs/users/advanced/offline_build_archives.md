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

The output of `//:my.service-offline` will be an Antlir container that itself can be packaged up in an fbpkg and shipped elsewhere.

## Building inside the image

The OS image will contain all input files necessary to produce the desired output artifact(s) in a no-network, low-dependency environment.

To test out a build on your devserver and ensure you _can_ actually produce the output you expect, you can materialize the container in a separate location and enter with with `systemd-nspawn`:

```[bash]
# Use Antlir's special `=container` rule to generate the container locally.
$ buck2 run //my/service:my.service-offline=container -- --snapshot-into ~/local/my_service_offline

# Enter the container as the build user with no network, you will need root access
$ sudo systemd-nspawn -D ~/local/my_service_offline --register=no --user facebook --private-network

[facebook@my_service_offline ~]$ ls -l
total 0
drwxr-xr-x 1 facebook users   16 Jul 20 10:13 dotslash_cache
drwxr-xr-x 1 facebook users 2142 Jul 20 10:28 fbsource
```

This drops you into the container as the `facebook` service user. This user has various settings and configurations configured on login for buck2 and the new `fbpkg-build` entrypoint to work offline by default with no extra flags or configuration required.

There's a "checkout" of fbsource at `/home/facebook` (not a real source code repository, but enough to get builds working), as well as a cached dotslash directory with prefetched executables.

Inside the fbsource repository are one or more shell scripts to make it easier to perform offline builds:

```[bash]
[facebook@my_service_offline fbsource]$ ls -l *.sh
-rwxr-xr-x 1 facebook users 167 Jul 20 10:07 build_fbpkg_my_service.sh

[facebook@my_service_offline fbsource]$ cat build_fbpkg_my_service.sh
#!/bin/sh

exec fbpkg-build --offline --build-local --no-publish fbcode//my/service:my.service

[facebook@my_service_offline fbsource]$ ./build_fbpkg_my_service.sh
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
You can access the files inside the container, from your dev server, you can cp the generated fbpkg out as root. The path will be the image base path (the path you pass through --snapshot-into when built the image) + path inside container (the output of the build script), in our case it is shown below
```[bash]
[yourunix@devvm4242.vll0 /]$ ls ~/local/my_service_offline/home/facebook/fbsource/buck-out/v2/gen/fbcode/29146bce1651974e/path/to/my_service_offline/__my_service_offline__/tree
server.par
[yourunix@devvm4242.vll0 /]$ sudo cp ~/local/my_service_offline/home/facebook/fbsource/buck-out/v2/gen/fbcode/29146bce1651974e/path/to/my_service_offline/__my_service_offline__/tree/server.par /tmp/your_test_dir
```


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
-rwxr-xr-x 1 facebook users 167 Jul 20 10:07 build_fbpkg_my_service.sh
-rwxr-xr-x 1 facebook users 123 Jul 20 10:07 build_fbpkg_my_service_targets_mode-opt.sh
-rwxr-xr-x 1 facebook users 123 Jul 20 10:07 build_fbpkg_my_service_targets_mode-dev.sh

[facebook@my_service_offline fbsource]$ cat build_my_service_targets_mode-opt.sh
#!/bin/sh

exec buck2 build @fbsource//fbcode/mode/opt fbcode//my/service:my_service
```

These shell scripts by default build all targets specified in the `fbpkg.builder` target, but you can also invoke the `buck2 build` command accordingly.

## Implementation details

Several environment variables must be set to perform fully-offline builds. These are all set in user-level `.bash_profile` scripts (which take effect on login):
* `BUCK_OFFLINE_BUILD` must be set; this indicates to buck2 that it should only perform *local execution* and disable talking to the RE cache layer. Support is also built into `fbpkg-build` to use a fake fbpkg uuid and build token rather than allocating one from the fbpkg frontend service.
* `DOTSLASH_CACHE` is set to `/home/facebook/dotslash_cache`. This tells dotslash to look in this directory for prefetched dotslash executables.

There are also several buckconfigs that must be set to get buck2 working fully-offline:
* `buck2.file_watcher = notify` disables watchman; we don't need watchman during an offline build
* `buck2.use_network_action_output_cache = true` tells buck2 to use precached action outputs from `ctx.actions.download_file` to e.g. perform a HEAD request to figure out if a file needs to be redownloaded during the build. These action outputs are cached in `fbsource/buck-out/v2/offline-cache`.
