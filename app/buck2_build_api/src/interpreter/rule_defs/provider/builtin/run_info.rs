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
use starlark::values::list::ListRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueOfUncheckedGeneric;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::FrozenStarlarkCmdArgs;
use crate::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;
use crate::interpreter::rule_defs::cmd_args::StarlarkCommandLineValueUnpack;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

/// Provider that signals that a rule is runnable
#[internal_provider(run_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[repr(transparent)]
pub struct RunInfoGen<V: ValueLifetimeless> {
    /// The command to run, stored as CommandLine
    args: ValueOfUncheckedGeneric<V, FrozenStarlarkCmdArgs>,
}

#[starlark_module]
fn run_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenRunInfo)]
    fn RunInfo<'v>(
        // TODO(nga): make the argument either named or positional.
        #[starlark(default = StarlarkCommandLineValueUnpack::List(ListRef::empty()))]
        args: StarlarkCommandLineValueUnpack<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<RunInfo<'v>> {
        let heap = eval.heap();
        let valid_args = StarlarkCmdArgs::try_from_value_typed(args)?;
        Ok(RunInfo {
            args: ValueOfUnchecked::<FrozenStarlarkCmdArgs>::new(heap.alloc(valid_args)),
        })
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for RunInfoGen<V> {
    fn register_me(&self) {
        command_line_arg_like_impl!(RunInfo::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
    ) -> buck2_error::Result<()> {
        ValueAsCommandLineLike::unpack_value_err(self.args.get().to_value())
            .expect("a command line from construction")
            .0
            .add_to_command_line(cli, context)?;
        Ok(())
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> buck2_error::Result<()> {
        ValueAsCommandLineLike::unpack_value_err(self.args.get().to_value())
            .expect("a command line from construction")
            .0
            .visit_artifacts(visitor)?;
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        ValueAsCommandLineLike::unpack_value_err(self.args.get().to_value())
            .expect("a command line from construction")
            .0
            .contains_arg_attr()
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}
