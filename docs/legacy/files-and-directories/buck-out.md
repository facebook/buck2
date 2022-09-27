# buck-out

Buck stores build artifacts in a directory named `buck-out` in the root of your [project](https://buck.build/about/overview.html).
You should not make assumptions about where Buck places your build artifacts within the directory structure beneath `buck-out` as these locations depend on Buck's implementation and could potentially change over time. Instead, to obtain the location of the build artifact for a particular target, use the `--show-output` option with the [`buck build`](https://buck.build/command/build.html) or the [`buck targets`](https://buck.build/command/targets.html) command.

```
buck targets --show-output <target>
buck build --show-output <target>
```

You can also obtain the locations of your build artifacts by specifying either the `--build-report` or`--keep-going` options with `buck build`.
Note that `--show-output` is going to be deprecated soon for `buck build` and replaced with `--show-outputs`. `--show-outputs` may print more than one build artifact per build target.

```
buck build --build-report <target>
buck build --keep-going <target>
```

For more information about these options, see the topics for the [`buck build`](https://buck.build/command/build.html) and [`buck targets`](https://buck.build/command/targets.html) commands.
