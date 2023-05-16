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
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::StarlarkCommandLine;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

/// Provider that signals that a rule is runnable
#[internal_provider(run_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[repr(transparent)]
pub struct RunInfoGen<V> {
    /// The command to run, stored as CommandLine
    #[provider(field_type = "StarlarkCommandLine")]
    args: V,
}

#[starlark_module]
fn run_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(dot_type = "RunInfo")]
    fn RunInfo<'v>(
        #[starlark(default = AllocList::EMPTY)] args: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<RunInfo<'v>> {
        let heap = eval.heap();
        let valid_args = StarlarkCommandLine::try_from_value(args)?;
        Ok(RunInfo {
            args: heap.alloc(valid_args),
        })
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for RunInfoGen<V> {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        self.args
            .to_value()
            .as_command_line()
            .expect("a command line from construction")
            .add_to_command_line(cli, context)?;
        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        self.args
            .to_value()
            .as_command_line()
            .expect("a command line from construction")
            .visit_artifacts(visitor)?;
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        self.args
            .to_value()
            .as_command_line()
            .expect("a command line from construction")
            .contains_arg_attr()
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}
