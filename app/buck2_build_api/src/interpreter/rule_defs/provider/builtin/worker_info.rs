/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::atomic;
use std::sync::atomic::AtomicU64;

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use buck2_error::BuckErrorContext;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::list::AllocList;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueOfUncheckedGeneric;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::FrozenStarlarkCmdArgs;
use crate::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;

/// Provider that signals that a rule is a worker tool
#[internal_provider(worker_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[freeze(validator = validate_worker_info, bounds = "V: ValueLike<'freeze>")]
#[repr(C)]
pub struct WorkerInfoGen<V: ValueLifetimeless> {
    // Command to spawn a new worker
    pub exe: ValueOfUncheckedGeneric<V, FrozenStarlarkCmdArgs>,
    // Maximum number of concurrent commands to execute on a worker instance without queuing
    pub concurrency: ValueOfUncheckedGeneric<V, NoneOr<usize>>,
    // Whether to always run actions using this worker via the streaming API
    pub streaming: ValueOfUncheckedGeneric<V, bool>,
    // Bazel remote persistent worker protocol capable worker
    pub supports_bazel_remote_persistent_worker_protocol: ValueOfUncheckedGeneric<V, bool>,

    pub id: u64,
}

fn next_id() -> u64 {
    static LAST_ID: AtomicU64 = AtomicU64::new(0);
    LAST_ID.fetch_add(1, atomic::Ordering::Relaxed) + 1
}

#[starlark_module]
fn worker_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenWorkerInfo)]
    fn WorkerInfo<'v>(
        #[starlark(default = AllocList::EMPTY)] exe: Value<'v>,
        #[starlark(require = named, default = NoneOr::None)] concurrency: NoneOr<
            ValueOf<'v, usize>,
        >,
        #[starlark(require = named, default = NoneType)] streaming: Value<'v>,
        #[starlark(require = named, default = false)]
        supports_bazel_remote_persistent_worker_protocol: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<WorkerInfo<'v>> {
        let heap = eval.heap();
        let valid_exe = StarlarkCmdArgs::try_from_value(exe)?;
        let exe = ValueOfUnchecked::new(heap.alloc(valid_exe));
        let id = next_id();
        Ok(WorkerInfo {
            exe,
            id,
            concurrency: heap.alloc_typed_unchecked(concurrency).cast(),
            streaming: ValueOfUnchecked::new(streaming),
            supports_bazel_remote_persistent_worker_protocol: heap
                .alloc_typed_unchecked(supports_bazel_remote_persistent_worker_protocol)
                .cast(),
        })
    }
}

impl<'v, V: ValueLike<'v>> WorkerInfoGen<V> {
    pub fn exe_command_line(&self) -> &'v dyn CommandLineArgLike {
        ValueAsCommandLineLike::unpack_value_err(self.exe.get().to_value())
            .expect("validated at construction")
            .0
    }

    pub fn concurrency(&self) -> Option<usize> {
        self.concurrency
            .to_value()
            .unpack()
            .expect("validated at construction")
            .into_option()
    }

    pub fn streaming(&self) -> bool {
        NoneOr::<bool>::unpack_value(self.streaming.get().to_value())
            .unwrap()
            .unwrap()
            .into_option()
            .unwrap_or(false)
    }

    pub fn supports_bazel_remote_persistent_worker_protocol(&self) -> bool {
        self.supports_bazel_remote_persistent_worker_protocol
            .to_value()
            .unpack()
            .expect("validated at construction")
    }
}

fn validate_worker_info<'v, V>(info: &WorkerInfoGen<V>) -> buck2_error::Result<()>
where
    V: ValueLike<'v>,
{
    let exe = StarlarkCmdArgs::try_from_value(info.exe.get().to_value()).with_buck_error_context(
        || {
            format!(
                "Value for `exe` field is not a command line: `{}`",
                info.exe
            )
        },
    )?;
    if exe.is_empty() {
        return Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Input,
            "Value for `exe` field is an empty command line: `{}`",
            info.exe
        ));
    }

    Ok(())
}
