/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::list::AllocList;
use starlark::values::Freeze;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueTyped;

use crate::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;
use crate::interpreter::rule_defs::provider::builtin::worker_info::WorkerInfo;

/// Provider that signals that a rule can run using a worker
#[internal_provider(worker_run_info_creator)]
#[derive(Clone, Debug, Coerce, Trace, Freeze, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct WorkerRunInfoGen<V> {
    // Configuration needed to spawn a new worker
    #[provider(field_type = WorkerInfo)]
    worker: V,

    // Command to execute without spawning a worker, when the build environment or configuration does not support workers
    #[provider(field_type = StarlarkCmdArgs)]
    exe: V,
}

#[starlark_module]
fn worker_run_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenWorkerRunInfo)]
    fn WorkerRunInfo<'v>(
        #[starlark(require = named)] worker: ValueOf<'v, &'v WorkerInfo<'v>>,
        #[starlark(require = named, default = AllocList::EMPTY)] exe: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<WorkerRunInfo<'v>> {
        let heap = eval.heap();
        let valid_exe = StarlarkCmdArgs::try_from_value(exe)?;
        Ok(WorkerRunInfo {
            worker: *worker,
            exe: heap.alloc(valid_exe),
        })
    }
}

impl<'v, V: ValueLike<'v>> WorkerRunInfoGen<V> {
    pub fn worker(&self) -> ValueOf<'v, &'v WorkerInfo<'v>> {
        ValueOf::unpack_value(self.worker.to_value()).expect("validated at construction")
    }

    pub fn exe(&self) -> ValueTyped<'v, StarlarkCmdArgs<'v>> {
        ValueTyped::new(self.exe.to_value()).expect("validated at construction")
    }
}
