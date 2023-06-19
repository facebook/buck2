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
use anyhow::Context;
use buck2_build_api_derive::internal_provider;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::list::AllocList;
use starlark::values::Freeze;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::StarlarkCommandLine;

/// Provider that signals that a rule is a worker tool
#[internal_provider(worker_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[freeze(validator = validate_worker_info, bounds = "V: ValueLike<'freeze>")]
#[repr(C)]
pub struct WorkerInfoGen<V> {
    // Command to spawn a new worker
    #[provider(field_type = "StarlarkCommandLine")]
    pub exe: V,

    pub id: u64,
}

fn next_id() -> u64 {
    static LAST_ID: AtomicU64 = AtomicU64::new(0);
    LAST_ID.fetch_add(1, atomic::Ordering::Relaxed) + 1
}

#[starlark_module]
fn worker_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(dot_type = "WorkerInfo")]
    fn WorkerInfo<'v>(
        #[starlark(default = AllocList::EMPTY)] exe: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<WorkerInfo<'v>> {
        let heap = eval.heap();
        let valid_exe = StarlarkCommandLine::try_from_value(exe)?;
        let exe = heap.alloc(valid_exe);
        let id = next_id();
        Ok(WorkerInfo { exe, id })
    }
}

impl<'v, V: ValueLike<'v>> WorkerInfoGen<V> {
    pub fn exe_command_line(&self) -> &'v dyn CommandLineArgLike {
        self.exe
            .to_value()
            .as_command_line()
            .expect("validated at construction")
    }
}

fn validate_worker_info<'v, V>(info: &WorkerInfoGen<V>) -> anyhow::Result<()>
where
    V: ValueLike<'v>,
{
    let exe = StarlarkCommandLine::try_from_value(info.exe.to_value()).with_context(|| {
        format!(
            "Value for `exe` field is not a command line: `{}`",
            info.exe
        )
    })?;
    if exe.is_empty() {
        return Err(anyhow::anyhow!(
            "Value for `exe` field is an empty command line: `{}`",
            info.exe
        ));
    }

    Ok(())
}
