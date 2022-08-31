/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use once_cell::sync::Lazy;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::structs::FrozenStruct;
use starlark::values::AllocFrozenValue;
use starlark::values::FrozenHeap;
use starlark::values::FrozenValue;
use starlark::values::OwnedFrozenValue;
use starlark::values::Value;

use crate::extra::BuildContext;
use crate::extra::InterpreterHostArchitecture;
use crate::extra::InterpreterHostPlatform;

fn new_host_info(
    host_platform: InterpreterHostPlatform,
    host_architecture: InterpreterHostArchitecture,
) -> OwnedFrozenValue {
    let heap = FrozenHeap::new();

    fn new_struct<V: AllocFrozenValue + Copy>(
        heap: &FrozenHeap,
        values: &[(&str, V)],
    ) -> FrozenValue {
        let mut fields = SmallMap::with_capacity(values.len());
        for (k, v) in values {
            fields.insert(heap.alloc_str(k), heap.alloc(*v));
        }
        heap.alloc(FrozenStruct::new(fields))
    }

    let os = new_struct(
        &heap,
        &[
            ("is_linux", host_platform == InterpreterHostPlatform::Linux),
            ("is_macos", host_platform == InterpreterHostPlatform::MacOS),
            (
                "is_windows",
                host_platform == InterpreterHostPlatform::Windows,
            ),
            ("is_freebsd", false),
            ("is_unknown", false),
        ],
    );

    let arch = new_struct(
        &heap,
        &[
            (
                "is_x86_64",
                host_architecture == InterpreterHostArchitecture::X86_64,
            ),
            (
                "is_aarch64",
                host_architecture == InterpreterHostArchitecture::AArch64,
            ),
            ("is_arm", false),
            ("is_armeb", false),
            ("is_i386", false),
            ("is_mips", false),
            ("is_mips64", false),
            ("is_mipsel", false),
            ("is_mipsel64", false),
            ("is_powerpc", false),
            ("is_ppc64", false),
            ("is_unknown", false),
        ],
    );

    let info = new_struct(
        &heap,
        &[
            ("os", os),
            ("arch", arch),
            // TODO(cjhopman): Remove in favour of version_info() in Buck v1 and v2
            // We want to be able to determine if we are on Buck v2 or not, this mechanism
            // is quick, cheap and Buck v1 compatible.
            ("buck2", FrozenValue::new_bool(true)),
        ],
    );

    // Safe because the value info was allocated into the heap
    unsafe { OwnedFrozenValue::new(heap.into_ref(), info) }
}

#[starlark_module]
pub fn register_host_info(builder: &mut GlobalsBuilder) {
    #[starlark(speculative_exec_safe)]
    fn host_info<'v>(eval: &mut Evaluator) -> anyhow::Result<Value<'v>> {
        // TODO: Do something about this. This information shouldn't be exposed in the general
        // api because the initial build file processing should be host-independent.
        // If we can't migrate uses off of this, we may need to support detecting at least the
        // os correctly.

        // Some modules call host_info a lot, so cache the values we might expect
        // and avoid reallocating them.
        static HOST_PLATFORM_LINUX_AARCH64: Lazy<OwnedFrozenValue> = Lazy::new(|| {
            new_host_info(
                InterpreterHostPlatform::Linux,
                InterpreterHostArchitecture::AArch64,
            )
        });
        static HOST_PLATFORM_LINUX_X86_64: Lazy<OwnedFrozenValue> = Lazy::new(|| {
            new_host_info(
                InterpreterHostPlatform::Linux,
                InterpreterHostArchitecture::X86_64,
            )
        });
        static HOST_PLATFORM_MACOS_AARCH64: Lazy<OwnedFrozenValue> = Lazy::new(|| {
            new_host_info(
                InterpreterHostPlatform::MacOS,
                InterpreterHostArchitecture::AArch64,
            )
        });
        static HOST_PLATFORM_MACOS_X86_64: Lazy<OwnedFrozenValue> = Lazy::new(|| {
            new_host_info(
                InterpreterHostPlatform::MacOS,
                InterpreterHostArchitecture::X86_64,
            )
        });
        static HOST_PLATFORM_WINDOWS_AARCH64: Lazy<OwnedFrozenValue> = Lazy::new(|| {
            new_host_info(
                InterpreterHostPlatform::Windows,
                InterpreterHostArchitecture::AArch64,
            )
        });
        static HOST_PLATFORM_WINDOWS_X86_64: Lazy<OwnedFrozenValue> = Lazy::new(|| {
            new_host_info(
                InterpreterHostPlatform::Windows,
                InterpreterHostArchitecture::X86_64,
            )
        });

        let host_platform = BuildContext::from_context(eval)?.host_platform;
        let host_architecture = BuildContext::from_context(eval)?.host_architecture;
        let v = match (host_platform, host_architecture) {
            (InterpreterHostPlatform::Linux, InterpreterHostArchitecture::AArch64) => {
                &HOST_PLATFORM_LINUX_AARCH64
            }
            (InterpreterHostPlatform::Linux, InterpreterHostArchitecture::X86_64) => {
                &HOST_PLATFORM_LINUX_X86_64
            }
            (InterpreterHostPlatform::MacOS, InterpreterHostArchitecture::AArch64) => {
                &HOST_PLATFORM_MACOS_AARCH64
            }
            (InterpreterHostPlatform::MacOS, InterpreterHostArchitecture::X86_64) => {
                &HOST_PLATFORM_MACOS_X86_64
            }
            (InterpreterHostPlatform::Windows, InterpreterHostArchitecture::AArch64) => {
                &HOST_PLATFORM_WINDOWS_AARCH64
            }
            (InterpreterHostPlatform::Windows, InterpreterHostArchitecture::X86_64) => {
                &HOST_PLATFORM_WINDOWS_X86_64
            }
        };
        Ok(v.value())
    }
}
