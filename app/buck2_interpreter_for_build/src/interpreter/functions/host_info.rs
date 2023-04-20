/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::extra::XcodeVersionInfo;
use derivative::Derivative;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::structs::AllocStruct;
use starlark::values::AllocFrozenValue;
use starlark::values::FrozenHeap;
use starlark::values::FrozenValue;
use starlark::values::OwnedFrozenValue;
use starlark::values::Value;

use crate::interpreter::build_context::BuildContext;

fn new_host_info(
    host_platform: InterpreterHostPlatform,
    host_architecture: InterpreterHostArchitecture,
    xcode_info: Option<&XcodeVersionInfo>,
) -> OwnedFrozenValue {
    let heap = FrozenHeap::new();

    fn new_struct<V: AllocFrozenValue + Copy>(
        heap: &FrozenHeap,
        values: &[(&str, V)],
    ) -> FrozenValue {
        heap.alloc(AllocStruct(values.iter().copied()))
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

    let xcode = {
        let (version_string, major_version, minor_version, patch_version, build_number) =
            match xcode_info {
                Some(i) => (
                    heap.alloc(i.version_string.as_str()),
                    heap.alloc(i.major_version.as_str()),
                    heap.alloc(i.minor_version.as_str()),
                    heap.alloc(i.patch_version.as_str()),
                    heap.alloc(i.build_number.as_str()),
                ),
                None => (
                    FrozenValue::new_none(),
                    FrozenValue::new_none(),
                    FrozenValue::new_none(),
                    FrozenValue::new_none(),
                    FrozenValue::new_none(),
                ),
            };

        new_struct(
            &heap,
            &[
                ("version_string", version_string),
                ("major_version", major_version),
                ("minor_version", minor_version),
                ("patch_version", patch_version),
                ("build_number", build_number),
            ],
        )
    };

    let info = new_struct(
        &heap,
        &[
            ("os", os),
            ("arch", arch),
            // TODO(cjhopman): Remove in favour of version_info() in Buck v1 and v2
            // We want to be able to determine if we are on Buck v2 or not, this mechanism
            // is quick, cheap and Buck v1 compatible.
            ("buck2", FrozenValue::new_bool(true)),
            ("xcode", xcode),
        ],
    );

    // Safe because the value info was allocated into the heap
    unsafe { OwnedFrozenValue::new(heap.into_ref(), info) }
}

#[starlark_module]
pub fn register_host_info(builder: &mut GlobalsBuilder) {
    // Keeping this `speculative_exec_safe` is safe because BuildContext's `HostInfo`,
    // even when evaluated speculatively, is going to be the same across all interpreters
    // that might reuse each other's output.
    #[starlark(speculative_exec_safe)]
    fn host_info<'v>(eval: &mut Evaluator<'v, '_>) -> anyhow::Result<Value<'v>> {
        let host_info = &BuildContext::from_context(eval)?.host_info;
        Ok(host_info.value.owned_value(eval.frozen_heap()))
    }
}

#[derive(Derivative, Clone, Debug, Allocative)]
#[derivative(PartialEq)]
pub struct HostInfo {
    platform: InterpreterHostPlatform,
    arch: InterpreterHostArchitecture,
    xcode: Option<XcodeVersionInfo>,
    #[derivative(PartialEq = "ignore")]
    value: OwnedFrozenValue,
}

impl HostInfo {
    pub(crate) fn new(
        platform: InterpreterHostPlatform,
        arch: InterpreterHostArchitecture,
    ) -> Self {
        // Hack: Xcode version must be inferred from the platform we're running on
        // since multiple Xcodes can live side-by-side and this is external state.
        // Only try to populate this struct if we're running on macOS.
        let xcode = match platform {
            // TODO(raulgarcia4): Actually do something with any underlying
            // errors in `XcodeVersionInfo`, rather than discarding them.
            InterpreterHostPlatform::MacOS => XcodeVersionInfo::new().ok(),
            _ => None,
        };
        let value = new_host_info(platform, arch, xcode.as_ref());
        Self {
            platform,
            arch,
            xcode,
            value,
        }
    }
}
