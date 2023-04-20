/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::client_context::HostArchOverride;
use buck2_cli_proto::client_context::HostPlatformOverride;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;

pub fn get_host_info(
    host_platform: HostPlatformOverride,
    host_arch: HostArchOverride,
) -> (InterpreterHostPlatform, InterpreterHostArchitecture) {
    let interpreter_platform = match host_platform {
        HostPlatformOverride::Linux => InterpreterHostPlatform::Linux,
        HostPlatformOverride::MacOs => InterpreterHostPlatform::MacOS,
        HostPlatformOverride::Windows => InterpreterHostPlatform::Windows,
        HostPlatformOverride::DefaultPlatform => match std::env::consts::OS {
            "linux" => InterpreterHostPlatform::Linux,
            "macos" => InterpreterHostPlatform::MacOS,
            "windows" => InterpreterHostPlatform::Windows,
            "freebsd" => InterpreterHostPlatform::FreeBsd,
            _ => InterpreterHostPlatform::Unknown,
        },
    };
    // This compiles in the target architecture, which should be sufficient, as
    // we currently only run e.g. x86_64 on x86_64.
    let interpreter_architecture = match host_arch {
        HostArchOverride::AArch64 => InterpreterHostArchitecture::AArch64,
        HostArchOverride::X8664 => InterpreterHostArchitecture::X86_64,
        HostArchOverride::DefaultArch => match std::env::consts::ARCH {
            "aarch64" => InterpreterHostArchitecture::AArch64,
            "x86_64" => InterpreterHostArchitecture::X86_64,
            v => unimplemented!("no support yet for architecture `{}`", v),
        },
    };

    (interpreter_platform, interpreter_architecture)
}
