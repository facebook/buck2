/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::types::cell_path::StarlarkCellPath;
use starlark::values::type_repr::StarlarkTypeRepr;

use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;

impl CommandLineArgLike for StarlarkCellPath {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkCellPath::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
    ) -> buck2_error::Result<()> {
        let path = ctx.resolve_cell_path(self.0.as_ref())?.into_string();
        cli.push_arg(path);
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor,
    ) -> buck2_error::Result<()> {
        Ok(())
    }

    fn add_to_action_inputs_hash(
        &self,
        hasher: &mut dyn std::hash::Hasher,
    ) -> buck2_error::Result<bool> {
        hasher.write(self.0.as_ref().path().as_str().as_bytes());
        Ok(true)
    }
}
