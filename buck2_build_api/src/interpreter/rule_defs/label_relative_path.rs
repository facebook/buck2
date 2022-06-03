/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::paths::CellPath;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{NoSerialize, StarlarkValue},
};

use crate::interpreter::rule_defs::cmd_args::{CommandLineArgLike, CommandLineBuilder};

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize)]
pub struct LabelRelativePath(pub CellPath);

starlark_simple_value!(LabelRelativePath);

impl<'v> StarlarkValue<'v> for LabelRelativePath {
    starlark_type!("label_relative_path");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(label_relative_path_methods)
    }
}

impl CommandLineArgLike for LabelRelativePath {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        cli.add_arg_string(cli.resolve_cell_path(&self.0)?.into_string());
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

#[starlark_module]
fn label_relative_path_methods(builder: &mut MethodsBuilder) {
    fn add(this: &LabelRelativePath, arg: &str) -> anyhow::Result<LabelRelativePath> {
        Ok(LabelRelativePath((*this).0.join_normalized(arg)?))
    }
}
