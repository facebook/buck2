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

use allocative::Allocative;
use anyhow::Context;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_interpreter::build_context::STARLARK_PATH_FROM_BUILD_CONTEXT;
use buck2_interpreter::bxl::BxlFunctions;
use buck2_interpreter::bxl::BXL_FUNCTIONS;
use buck2_interpreter::path::BxlFilePath;
use buck2_util::collections::ordered_map::OrderedMap;
use cli_args::CliArgs;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::collections::SmallSet;
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
use crate::bxl::starlark_defs::cli_args::CliArgError;
use crate::bxl::starlark_defs::cli_args::CliArgValue;
use crate::bxl::starlark_defs::functions::register_artifact_function;
use crate::bxl::starlark_defs::functions::register_error_handling_function;
use crate::bxl::starlark_defs::functions::register_instant_function;
use crate::bxl::starlark_defs::functions::register_label_function;
use crate::bxl::starlark_defs::functions::register_target_function;
pub mod alloc_node;
pub mod analysis_result;
pub mod artifacts;
pub mod audit;
pub mod build_result;
pub mod cli_args;
pub mod context;
pub mod cquery;
pub mod event;
pub mod file_expr;
pub mod file_set;
pub mod functions;
pub mod nodes;
pub mod providers_expr;
mod query_util;
pub mod target_expr;
pub mod targetset;
pub mod time;
pub mod uquery;
use starlark::starlark_module;

use crate::bxl::eval::CliResolutionCtx;

#[starlark_module]
pub fn register_bxl_function(builder: &mut GlobalsBuilder) {
    fn bxl<'v>(
        #[starlark(require = named)] r#impl: Value<'v>,
        #[starlark(require = named)] cli_args: DictOf<'v, &'v str, &'v CliArgs>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let implementation = r#impl;

        let bxl_path = (*(STARLARK_PATH_FROM_BUILD_CONTEXT.get()?)(eval)?
            .unpack_bxl_file()
            .ok_or_else(|| anyhow::anyhow!("`bxl` can only be declared in bxl files"))?)
        .clone();

        let mut unresolved_cli_args = SmallMap::new();
        let mut short_args = SmallSet::new();

        for (arg, def) in cli_args.to_dict().into_iter() {
            if let Some(short) = def.short {
                if short_args.contains(&short) {
                    return Err(CliArgError::DuplicateShort(short.to_owned()).into());
                } else {
                    short_args.insert(short.to_owned());
                }
            }
            unresolved_cli_args.insert(arg.to_owned(), def.clone());
        }

        Ok(eval.heap().alloc(BxlFunction {
            bxl_path,
            id: RefCell::new(None),
            implementation,
            cli_args: unresolved_cli_args,
            docs: Some(doc.to_owned()),
        }))
    }
}

pub(crate) fn init_bxl_functions() {
    BXL_FUNCTIONS.init(BxlFunctions {
        register_cli_args_struct: |g| g.struct_("cli_args", cli_args::register_cli_args_module),
        register_bxl_function,
        register_artifact_function,
        register_label_function,
        register_target_function,
        register_instant_function,
        register_error_handling_function,
    });
}

/// Errors around rule declaration, instantiation, validation, etc
#[derive(Debug, Error)]
enum BxlError {
    #[error("Bxl defined in `{0}` must be assigned to a variable, e.g. `my_bxl = bxl(...)`")]
    BxlNotAssigned(String),
}

/// The callable created by `bxl()`
#[derive(Debug, Clone, ProvidesStaticType, Trace, NoSerialize, Allocative)]
pub(crate) struct BxlFunction<'v> {
    // The bxl path that contains the bxl() call; stored here so we can retrieve extra
    /// information during `export_as()`
    bxl_path: BxlFilePath,
    /// Once exported, the `import_path` and `name` of the callable. Used in DICE to retrieve bxl
    /// implementations
    id: RefCell<Option<BxlFunctionLabel>>,
    /// The implementation function for this bxl. Must be callable and take a ctx
    implementation: Value<'v>,
    /// the cli args to this bxl function
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
            None => return Err(BxlError::BxlNotAssigned(self.bxl_path.to_string()).into()),
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

#[derive(Debug, Display, ProvidesStaticType, NoSerialize, Allocative)]
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
    pub fn implementation(&self) -> FrozenValue {
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

    /// Parses the cli args as defined by this bxl function. Automatically changes the CLI args
    /// to snakecase when accessed from the bxl context.
    pub(crate) async fn parse_clap<'a>(
        &self,
        clap: clap::ArgMatches,
        ctx: &CliResolutionCtx<'a>,
    ) -> anyhow::Result<OrderedMap<String, CliArgValue>> {
        let mut res = OrderedMap::with_capacity(self.cli_args.len());

        for (arg, cli) in self.cli_args.iter() {
            let snake_case_args = arg.replace('-', "_");
            if res.contains_key(&snake_case_args) {
                return Err(CliArgError::DefinedBothKebabAndSnakeCase(arg.clone()).into());
            }
            res.insert(
                snake_case_args,
                cli.parse_clap(ArgAccessor::Clap { clap: &clap, arg }, ctx)
                    .await
                    .with_context(|| {
                        format!("Error parsing cli flag `{}` for bxl function", arg)
                    })?,
            );
        }

        Ok(res)
    }
}
