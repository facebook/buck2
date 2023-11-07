/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::paths::CellRelativePath;
use buck2_interpreter::cfg_constructor::REGISTER_SET_CFG_CONSTRUCTOR;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter_for_build::interpreter::build_context::BuildContext;
use buck2_interpreter_for_build::interpreter::build_context::PerFileTypeContext;
use buck2_interpreter_for_build::interpreter::package_file_extra::PackageFileExtra;
use buck2_interpreter_for_build::interpreter::package_file_extra::MAKE_CFG_CONSTRUCTOR;
use buck2_node::cfg_constructor::CfgConstructorImpl;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;
use starlark::values::starlark_value;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::NoSerialize;
use starlark::values::OwnedFrozenValue;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;

use crate::CfgConstructor;

#[derive(Debug, buck2_error::Error)]
enum RegisterCfgConstructorError {
    #[error("`set_cfg_constructor()` can only be called from the repository root `PACKAGE` file")]
    NotPackageRoot,
    #[error("`set_cfg_constructor()` can only be called at most once")]
    AlreadyRegistered,
}

/// Value stored in `PackageFileExtra.cfg_constructor` field.
/// Not a real Starlark value.
#[derive(
    Debug,
    derive_more::Display,
    Trace,
    NoSerialize,
    ProvidesStaticType,
    Allocative
)]
#[display(fmt = "{:?}", "self")]
struct StarlarkCfgConstructor<'v> {
    stage0: Value<'v>,
    stage1: Value<'v>,
    key: String,
}

#[derive(
    Debug,
    derive_more::Display,
    NoSerialize,
    ProvidesStaticType,
    Allocative
)]
#[display(fmt = "{:?}", "self")]
struct FrozenStarlarkCfgConstructor {
    stage0: FrozenValue,
    stage1: FrozenValue,
    key: String,
}

#[starlark_value(type = "StarlarkCfgConstructor")]
impl<'v> StarlarkValue<'v> for StarlarkCfgConstructor<'v> {}

#[starlark_value(type = "StarlarkCfgConstructor")]
impl<'v> StarlarkValue<'v> for FrozenStarlarkCfgConstructor {
    type Canonical = StarlarkCfgConstructor<'v>;
}

impl<'v> Freeze for StarlarkCfgConstructor<'v> {
    type Frozen = FrozenStarlarkCfgConstructor;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let StarlarkCfgConstructor {
            stage0,
            stage1,
            key,
        } = self;
        let (stage0, stage1) = (stage0, stage1).freeze(freezer)?;
        Ok(FrozenStarlarkCfgConstructor {
            stage0,
            stage1,
            key,
        })
    }
}

fn make_cfg_constructor(
    cfg_constructor: OwnedFrozenValue,
) -> anyhow::Result<Arc<dyn CfgConstructorImpl>> {
    let cfg_constructor = cfg_constructor.downcast_anyhow::<FrozenStarlarkCfgConstructor>()?;
    let (cfg_constructor_pre_constraint_analysis, cfg_constructor_post_constraint_analysis) = unsafe {
        (
            OwnedFrozenValue::new(cfg_constructor.owner().dupe(), cfg_constructor.stage0),
            OwnedFrozenValue::new(cfg_constructor.owner().dupe(), cfg_constructor.stage1),
        )
    };
    let key = cfg_constructor.key.clone();
    Ok(Arc::new(CfgConstructor {
        cfg_constructor_pre_constraint_analysis,
        cfg_constructor_post_constraint_analysis,
        key,
    }))
}

#[starlark_module]
pub(crate) fn register_set_cfg_constructor(globals: &mut GlobalsBuilder) {
    /// Register global cfg constructor.
    ///
    /// This function can only be called from the repository root `PACKAGE` file.
    ///
    /// Parameters:
    ///   stage0: The first cfg constructor that will be invoked before configuration rules are analyzed.
    ///   stage1: The second cfg constructor that will be invoked after configuration rules are analyzed.
    ///   key: The key for cfg modifiers on PACKAGE values and metadata.
    fn set_cfg_constructor<'v>(
        #[starlark(require=named)] stage0: Value<'v>,
        #[starlark(require=named)] stage1: Value<'v>,
        #[starlark(require=named)] key: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<NoneType> {
        let build_context = BuildContext::from_context(eval)?;
        let ctx = match &build_context.additional {
            PerFileTypeContext::Package(ctx) => ctx,
            _ => return Err(RegisterCfgConstructorError::NotPackageRoot.into()),
        };
        if ctx.path
            != PackageFilePath::for_dir(CellPathRef::new(
                build_context.cell_info.cell_resolver().root_cell(),
                CellRelativePath::empty(),
            ))
        {
            return Err(RegisterCfgConstructorError::NotPackageRoot.into());
        }
        let package_file_extra: &PackageFileExtra = PackageFileExtra::get_or_init(eval)?;
        if package_file_extra.cfg_constructor.get().is_some() {
            return Err(RegisterCfgConstructorError::AlreadyRegistered.into());
        }
        package_file_extra.cfg_constructor.get_or_init(|| {
            eval.heap().alloc_complex(StarlarkCfgConstructor {
                stage0,
                stage1,
                key: key.to_owned(),
            })
        });
        Ok(NoneType)
    }
}

pub(crate) fn init_registration() {
    MAKE_CFG_CONSTRUCTOR.init(make_cfg_constructor);
    REGISTER_SET_CFG_CONSTRUCTOR.init(register_set_cfg_constructor);
}
