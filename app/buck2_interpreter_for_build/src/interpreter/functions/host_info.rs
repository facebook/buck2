/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_interpreter::extra::xcode::XcodeVersionInfo;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use derivative::Derivative;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::structs::AllocStruct;
use starlark::values::structs::StructRef;
use starlark::values::AllocFrozenValue;
use starlark::values::FrozenHeap;
use starlark::values::FrozenValue;
use starlark::values::OwnedFrozenValue;
use starlark::values::ValueOfUnchecked;

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

    let platform = |name, x| (name, host_platform == x);
    let os = new_struct(
        &heap,
        &[
            platform("is_linux", InterpreterHostPlatform::Linux),
            platform("is_macos", InterpreterHostPlatform::MacOS),
            platform("is_windows", InterpreterHostPlatform::Windows),
            platform("is_freebsd", InterpreterHostPlatform::FreeBsd),
            platform("is_unknown", InterpreterHostPlatform::Unknown),
        ],
    );

    let host = |name, x| (name, host_architecture == x);
    let arch = new_struct(
        &heap,
        &[
            host("is_x86_64", InterpreterHostArchitecture::X86_64),
            host("is_aarch64", InterpreterHostArchitecture::AArch64),
            host("is_arm", InterpreterHostArchitecture::Arm),
            ("is_armeb", false),
            host("is_i386", InterpreterHostArchitecture::X86),
            host("is_mips", InterpreterHostArchitecture::Mips),
            host("is_mips64", InterpreterHostArchitecture::Mips64),
            ("is_mipsel", false),
            ("is_mipsel64", false),
            host("is_powerpc", InterpreterHostArchitecture::PowerPc),
            host("is_ppc64", InterpreterHostArchitecture::PowerPc64),
            host("is_unknown", InterpreterHostArchitecture::Unknown),
        ],
    );

    let xcode = {
        let mk_value = |sel: fn(&XcodeVersionInfo) -> &String| match xcode_info {
            Some(i) => heap.alloc(sel(i).as_str()),
            None => FrozenValue::new_none(),
        };

        new_struct(
            &heap,
            &[
                ("version_string", mk_value(|x| &x.version_string)),
                ("major_version", mk_value(|x| &x.major_version)),
                ("minor_version", mk_value(|x| &x.minor_version)),
                ("patch_version", mk_value(|x| &x.patch_version)),
                ("build_number", mk_value(|x| &x.build_number)),
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
pub(crate) fn register_host_info(builder: &mut GlobalsBuilder) {
    /// The `host_info()` function is used to get the current OS and processor architecture on the host. The structure returned is laid out thusly:
    ///
    /// ```python
    /// struct(
    ///     os=struct(
    ///         is_linux=True|False,
    ///         is_macos=True|False,
    ///         is_windows=True|False,
    ///         is_freebsd=True|False,
    ///         is_unknown=True|False,
    ///     ),
    ///     arch=struct(
    ///         is_aarch64=True|False,
    ///         is_arm=True|False,
    ///         is_armeb=True|False,
    ///         is_i386=True|False,
    ///         is_mips=True|False,
    ///         is_mips64=True|False,
    ///         is_mipsel=True|False,
    ///         is_mipsel64=True|False,
    ///         is_powerpc=True|False,
    ///         is_ppc64=True|False,
    ///         is_x86_64=True|False,
    ///         is_unknown=True|False,
    ///     ),
    /// )
    /// ```
    #[starlark(speculative_exec_safe)]
    fn host_info<'v>(
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueOfUnchecked<'v, StructRef<'v>>> {
        // Keeping this `speculative_exec_safe` is safe because BuildContext's `HostInfo`,
        // even when evaluated speculatively, is going to be the same across all interpreters
        // that might reuse each other's output.
        let host_info = &BuildContext::from_context(eval)?.host_info;
        Ok(ValueOfUnchecked::new(
            host_info.value.owned_value(eval.frozen_heap()),
        ))
    }
}

#[derive(Derivative, Clone, Debug, Allocative)]
#[derivative(PartialEq)]
pub struct HostInfo {
    // These first three fields are for equality only, otherwise not used
    pub platform: InterpreterHostPlatform,
    arch: InterpreterHostArchitecture,
    xcode: Option<XcodeVersionInfo>,
    // The actual value which we ignore for equality, which is OK because of above
    #[derivative(PartialEq = "ignore")]
    value: OwnedFrozenValue,
}

impl HostInfo {
    pub(crate) fn new(
        platform: InterpreterHostPlatform,
        arch: InterpreterHostArchitecture,
        xcode: Option<XcodeVersionInfo>,
    ) -> Self {
        let value = new_host_info(platform, arch, xcode.as_ref());
        Self {
            platform,
            arch,
            xcode,
            value,
        }
    }
}
