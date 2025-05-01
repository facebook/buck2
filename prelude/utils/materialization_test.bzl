# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def materialization_test(artifacts: list[Artifact]) -> Provider:
    """
    Produce a ExternalRunnerTestInfo that just checks we can materialize the
    target.
    """

    # This legitimately looks at host_info because a few lines later it
    # specifically requires local execution. If we had better support for test
    # execution platfroms this wouldn't be necessary, but that's where we are
    # today.
    if host_info().os.is_windows:
        command = ["cmd.exe", "/v:off", "/c", "exit 0"]
    else:
        command = ["true"]

    return ExternalRunnerTestInfo(
        type = "custom",
        command = [cmd_args(command, hidden = artifacts)],
        # Force it to run locally and thus force materialization.
        default_executor = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = False,
        ),
    )
