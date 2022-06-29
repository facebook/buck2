/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Definitions of core functionality just for bxl functions to access

use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use anyhow::Context;
use buck2_build_api::interpreter::rule_defs::cmd_args::register_args_function;
use buck2_build_api::interpreter::rule_defs::provider::registration::register_builtin_providers;
use buck2_bxl_core::BxlFunctionLabel;
use buck2_interpreter::build_defs::register_natives;
use buck2_interpreter::common::BxlFilePath;
use buck2_interpreter::extra::BuildContext;
use buck2_interpreter::functions::host_info::register_host_info;
use buck2_interpreter::functions::read_config::register_read_config;
use cli_args::CliArgs;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::dict::DictOf;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use thiserror::Error;

use crate::bxl::starlark_defs::cli_args::ArgAccessor;
use crate::bxl::starlark_defs::cli_args::CliArgValue;
use crate::bxl::starlark_defs::functions::register_label_function;

pub mod alloc_node;
pub mod analysis_result;
pub mod artifacts;
pub mod build_result;
pub mod cli_args;
pub mod context;
pub mod cquery;
pub mod file_set;
pub mod functions;
pub mod nodes;
pub mod providers_expr;
pub mod target_expr;
pub mod targetset;
pub mod uquery;
use starlark::starlark_module;

use crate::bxl::eval::CliResolutionCtx;

#[starlark_module]
pub fn register_bxl_function(builder: &mut GlobalsBuilder) {
    fn bxl<'v>(
        #[starlark(require = named)] implementation: Value<'v>,
        #[starlark(require = named)] cli_args: DictOf<'v, &'v str, &'v CliArgs>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let build_context = BuildContext::from_context(eval)?;
        let bxl_path = (*build_context
            .starlark_path
            .unpack_bxl_file()
            .ok_or_else(|| anyhow::anyhow!("`bxl` can only be declared in bxl files"))?)
        .clone();
        let cli_args = cli_args
            .to_dict()
            .into_iter()
            .map(|(arg, def)| (arg.to_owned(), def.clone()))
            .collect();

        Ok(eval.heap().alloc(BxlFunction {
            bxl_path,
            id: RefCell::new(None),
            implementation,
            cli_args,
            docs: Some(doc.to_owned()),
        }))
    }
}

fn register_bxl_defs(globals: &mut GlobalsBuilder) {
    globals.struct_("cli_args", cli_args::register_cli_args_module);
    register_bxl_function(globals);
    register_label_function(globals);
    register_read_config(globals);
    register_host_info(globals);
}

pub fn configure_bxl_file_globals(globals_builder: &mut GlobalsBuilder) {
    register_natives(globals_builder);
    register_args_function(globals_builder);
    register_bxl_defs(globals_builder);
    register_builtin_providers(globals_builder);
}

/// Errors around rule declaration, instantiation, validation, etc
#[derive(Debug, Error)]
pub enum BxlError {
    #[error("Bxl defined in `{0}` must be assigned to a variable, e.g. `my_bxl = bxl(...)`")]
    BxlNotAssigned(String),
    #[error("`{0}` can't be frozen because we don't freeze BXL contexts")]
    NoFreeze(&'static str),
}

/// The callable created by `bxl()`
#[derive(Debug, Clone, ProvidesStaticType, Trace, NoSerialize)]
pub(crate) struct BxlFunction<'v> {
    // The bxl path that contains the bxl() call; stored here so we can retrieve extra
    /// information during `export_as()`
    #[trace(unsafe_ignore)]
    bxl_path: BxlFilePath,
    /// Once exported, the `import_path` and `name` of the callable. Used in DICE to retrieve bxl
    /// implementations
    #[trace(unsafe_ignore)]
    id: RefCell<Option<BxlFunctionLabel>>,
    /// The implementation function for this bxl. Must be callable and take a ctx
    implementation: Value<'v>,
    /// the cli args to this bxl function
    #[trace(unsafe_ignore)]
    cli_args: SmallMap<String, CliArgs>,
    docs: Option<String>,
}

impl<'v> Display for BxlFunction<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.id.borrow() {
            Some(id) => write!(f, "{}()", id.name),
            None => write!(f, "<unbound bxl>"),
        }
    }
}

impl<'v> AllocValue<'v> for BxlFunction<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> StarlarkValue<'v> for BxlFunction<'v> {
    starlark_type!("bxl");

    fn export_as(&self, variable_name: &str, _eval: &mut Evaluator<'v, '_>) {
        *self.id.borrow_mut() = Some(BxlFunctionLabel {
            bxl_path: self.bxl_path.clone(),
            name: variable_name.to_owned(),
        });
    }
}

impl<'v> Freeze for BxlFunction<'v> {
    type Frozen = FrozenBxlFunction;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let frozen_impl = self.implementation.freeze(freezer)?;
        let docs = self.docs;
        let id = match self.id.into_inner() {
            Some(x) => x,
            None => return Err(BxlError::BxlNotAssigned(self.bxl_path.id().to_string()).into()),
        };
        let bxl_id = Arc::new(id);

        Ok(FrozenBxlFunction {
            implementation: frozen_impl,
            cli_args: self.cli_args,
            bxl_id,
            docs,
        })
    }
}

#[derive(Debug, Display, ProvidesStaticType, NoSerialize)]
#[display(fmt = "{}()", "bxl_id.name")]
pub struct FrozenBxlFunction {
    implementation: FrozenValue,
    cli_args: SmallMap<String, CliArgs>,
    bxl_id: Arc<BxlFunctionLabel>,
    docs: Option<String>,
}
starlark_simple_value!(FrozenBxlFunction);

impl<'v> StarlarkValue<'v> for FrozenBxlFunction {
    starlark_type!("bxl");
}

impl FrozenBxlFunction {
    pub fn get_impl(&self) -> FrozenValue {
        self.implementation
    }

    pub fn to_clap<'v>(&'v self, mut clap: clap::Command<'v>) -> clap::Command<'v> {
        if let Some(docs) = self.docs.as_ref() {
            clap = clap.about(docs.as_str())
        }

        for (arg, def) in self.cli_args.iter() {
            clap = clap.arg(def.to_clap(clap::Arg::new(arg.as_str()).long(arg.as_str())))
        }

        clap
    }

    /// parses the cli args as defined by this bxl function
    pub async fn parse_clap<'a>(
        &self,
        clap: clap::ArgMatches,
        ctx: &CliResolutionCtx<'a>,
    ) -> anyhow::Result<SmallMap<String, CliArgValue>> {
        let mut res = SmallMap::with_capacity(self.cli_args.len());

        for (arg, cli) in self.cli_args.iter() {
            res.insert(
                arg.clone(),
                cli.parse_clap(ArgAccessor::Clap { clap: &clap, arg }, ctx)
                    .await
                    .with_context(|| format!("when parsing cli flag `{}` for bxl function", arg))?,
            );
        }

        Ok(res)
    }
}
