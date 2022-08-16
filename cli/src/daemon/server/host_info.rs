/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use cli_proto::client_context::HostPlatformOverride;

pub(crate) fn get_host_info(
    host_platform: HostPlatformOverride,
) -> (InterpreterHostPlatform, InterpreterHostArchitecture) {
    let linux = InterpreterHostPlatform::Linux;
    let mac = InterpreterHostPlatform::MacOS;
    let windows = InterpreterHostPlatform::Windows;

    let interpreter_platform = match host_platform {
        HostPlatformOverride::Linux => linux,
        HostPlatformOverride::MacOs => mac,
        HostPlatformOverride::Windows => windows,
        HostPlatformOverride::Default => match std::env::consts::OS {
            "linux" => linux,
            "macos" => mac,
            "windows" => windows,
            v => unimplemented!("no support yet for operating system `{}`", v),
        },
    };
    // This compiles in the target architecture, which should be sufficient, as
    // we currently only run e.g. x86_64 on x86_64.
    let interpreter_architecture = match std::env::consts::ARCH {
        "aarch64" => InterpreterHostArchitecture::AArch64,
        "x86_64" => InterpreterHostArchitecture::X86_64,
        v => unimplemented!("no support yet for architecture `{}`", v),
    };
    (interpreter_platform, interpreter_architecture)
}
