# Update the Docker image for remote execution whenever you make a change to the Nix package set.
#
#   nix run path:.#dockerBuild \
#     | gzip --fast --no-name \
#     | nix run nixpkgs#skopeo -- copy \
#         --insecure-policy \
#         --digestfile image.digest \
#         --preserve-digests docker-archive:/dev/stdin \
#         docker://ghcr.io/$GITHUB_USER/buck2-remote-persistent-worker:latest
#
# NOTE, Change to a container registry that you have access to.
image = "docker://ghcr.io/aherrmann/buck2-remote-persistent-worker@sha256:23832b49492ef77601111218661b8fcc6eafbcc49cb9abffd08d79988dda540e"

def _platforms(ctx):
    constraints = dict()
    constraints.update(ctx.attrs.cpu_configuration[ConfigurationInfo].constraints)
    constraints.update(ctx.attrs.os_configuration[ConfigurationInfo].constraints)
    configuration = ConfigurationInfo(constraints = constraints, values = {})

    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = configuration,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = True,
            remote_cache_enabled = True,
            allow_cache_uploads = True,
            use_limited_hybrid = True,
            use_persistent_workers = ctx.attrs.use_persistent_workers,
            use_remote_persistent_workers = ctx.attrs.use_persistent_workers,
            remote_execution_properties = {
                "OSFamily": "Linux",
                "container-image": image,
                "workload-isolation-type": "podman",
                "recycle-runner": True,  # required for remote persistent workers
                "nonroot-workspace": True,
            },
            remote_execution_use_case = "buck2-default",
            remote_output_paths = "output_paths",
        ),
    )

    return [DefaultInfo(), ExecutionPlatformRegistrationInfo(platforms = [platform])]

buildbuddy = rule(
    attrs = {
        "cpu_configuration": attrs.dep(providers = [ConfigurationInfo]),
        "os_configuration": attrs.dep(providers = [ConfigurationInfo]),
        "use_persistent_workers": attrs.bool(default = False),
    },
    impl = _platforms
)
