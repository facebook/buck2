/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::FreezeBranded;
use starlark::values::StarlarkPagable;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::list::AllocList;
use starlark::values::none::NoneOr;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::cmd_args::FrozenStarlarkCmdArgs;
use crate::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;
use crate::interpreter::rule_defs::provider::builtin::worker_info::WorkerInfo;

/// Provider that signals that a rule can run using a worker
#[internal_provider(worker_run_info_creator)]
#[derive(
    Clone,
    Debug,
    Trace,
    FreezeBranded,
    ProvidesStaticType,
    Allocative,
    StarlarkPagable
)]
#[repr(C)]
pub struct WorkerRunInfo<'v> {
    // Configuration needed to spawn a new local worker
    worker: Option<ValueTyped<'v, WorkerInfo<'v>>>,

    // Configuration needed to spawn a new remote worker
    remote_worker: Option<ValueTyped<'v, WorkerInfo<'v>>>,

    // Command to execute without spawning a worker, when the build environment or configuration does not support workers
    exe: ValueOfUnchecked<'v, FrozenStarlarkCmdArgs<'static>>,
}

#[starlark_module]
fn worker_run_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenWorkerRunInfo)]
    fn WorkerRunInfo<'v>(
        #[starlark(require = named, default = NoneOr::None)] worker: NoneOr<
            ValueTyped<'v, WorkerInfo<'v>>,
        >,
        #[starlark(require = named, default = NoneOr::None)] remote_worker: NoneOr<
            ValueTyped<'v, WorkerInfo<'v>>,
        >,
        #[starlark(require = named, default = AllocList::EMPTY)] exe: Value<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<WorkerRunInfo<'v>> {
        let heap = eval.heap();
        let valid_exe = StarlarkCmdArgs::try_from_value(exe)?;

        Ok(WorkerRunInfo {
            worker: worker.into_option(),
            remote_worker: remote_worker.into_option(),
            exe: ValueOfUnchecked::new(heap.alloc(valid_exe)),
        })
    }
}

impl<'v> WorkerRunInfo<'v> {
    pub fn worker(&self) -> Option<ValueTyped<'v, WorkerInfo<'v>>> {
        self.worker
    }

    pub fn remote_worker(&self) -> Option<ValueTyped<'v, WorkerInfo<'v>>> {
        self.remote_worker
    }

    pub fn exe(&self) -> ValueTyped<'v, StarlarkCmdArgs<'v>> {
        ValueTyped::new_err(self.exe.get()).expect("validated at construction")
    }
}
