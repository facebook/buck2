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
use buck2_interpreter::downstream_crate_starlark_defs::REGISTER_BUCK2_CFG_CONSTRUCTOR_GLOBALS;
use buck2_interpreter_for_build::interpreter::build_context::BuildContext;
use buck2_interpreter_for_build::interpreter::build_context::PerFileTypeContext;
use buck2_interpreter_for_build::interpreter::package_file_extra::PackageFileExtra;
use buck2_interpreter_for_build::interpreter::package_file_extra::MAKE_CFG_CONSTRUCTOR;
use buck2_node::cfg_constructor::CfgConstructorImpl;
use buck2_node::metadata::key::MetadataKeyRef;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::starlark_value;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::NoSerialize;
use starlark::values::OwnedFrozenValue;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;

use crate::CfgConstructor;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
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
#[display("{:?}", self)]
struct StarlarkCfgConstructor<'v> {
    stage0: Value<'v>,
    stage1: Value<'v>,
    key: String,
    aliases: Option<Value<'v>>,
    extra_data: Option<Value<'v>>,
}

#[derive(
    Debug,
    derive_more::Display,
    NoSerialize,
    ProvidesStaticType,
    Allocative
)]
#[display("{:?}", self)]
struct FrozenStarlarkCfgConstructor {
    stage0: FrozenValue,
    stage1: FrozenValue,
    key: String,
    aliases: Option<FrozenValue>,
    extra_data: Option<FrozenValue>,
}

#[starlark_value(type = "StarlarkCfgConstructor")]
impl<'v> StarlarkValue<'v> for StarlarkCfgConstructor<'v> {}

#[starlark_value(type = "StarlarkCfgConstructor")]
impl<'v> StarlarkValue<'v> for FrozenStarlarkCfgConstructor {
    type Canonical = StarlarkCfgConstructor<'v>;
}

impl<'v> Freeze for StarlarkCfgConstructor<'v> {
    type Frozen = FrozenStarlarkCfgConstructor;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let StarlarkCfgConstructor {
            stage0,
            stage1,
            key,
            aliases,
            extra_data,
        } = self;
        let (stage0, stage1, aliases, extra_data) =
            (stage0, stage1, aliases, extra_data).freeze(freezer)?;
        Ok(FrozenStarlarkCfgConstructor {
            stage0,
            stage1,
            key,
            aliases,
            extra_data,
        })
    }
}

fn make_cfg_constructor(
    cfg_constructor: OwnedFrozenValue,
) -> buck2_error::Result<Arc<dyn CfgConstructorImpl>> {
    let cfg_constructor = cfg_constructor.downcast_starlark::<FrozenStarlarkCfgConstructor>()?;
    let (
        cfg_constructor_pre_constraint_analysis,
        cfg_constructor_post_constraint_analysis,
        aliases,
        extra_data,
    ) = unsafe {
        (
            OwnedFrozenValue::new(cfg_constructor.owner().dupe(), cfg_constructor.stage0),
            OwnedFrozenValue::new(cfg_constructor.owner().dupe(), cfg_constructor.stage1),
            cfg_constructor
                .aliases
                .map(|v| OwnedFrozenValue::new(cfg_constructor.owner().dupe(), v)),
            cfg_constructor
                .extra_data
                .map(|v| OwnedFrozenValue::new(cfg_constructor.owner().dupe(), v)),
        )
    };
    let key = MetadataKeyRef::new(&cfg_constructor.key)?.to_owned();
    Ok(Arc::new(CfgConstructor {
        cfg_constructor_pre_constraint_analysis,
        cfg_constructor_post_constraint_analysis,
        key,
        aliases,
        extra_data,
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
    ///   aliases: The aliases map to use for input modifiers.
    ///   extra_data: Some extra data that may be used by `set_cfg_constructor` implementation that is
    ///     custom to our implementation and may not be used in other context like open-source.
    fn set_cfg_constructor<'v>(
        #[starlark(require=named)] stage0: Value<'v>,
        #[starlark(require=named)] stage1: Value<'v>,
        #[starlark(require=named)] key: &str,
        #[starlark(require = named, default = NoneOr::None)] aliases: NoneOr<Value<'v>>,
        #[starlark(require = named, default = NoneOr::None)] extra_data: NoneOr<Value<'v>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<NoneType> {
        let build_context = BuildContext::from_context(eval)?;
        let ctx = match &build_context.additional {
            PerFileTypeContext::Package(ctx) => ctx,
            _ => {
                return Err(
                    buck2_error::Error::from(RegisterCfgConstructorError::NotPackageRoot).into(),
                );
            }
        };
        if ctx.path.dir()
            != CellPathRef::new(
                build_context.cell_info.cell_resolver().root_cell(),
                CellRelativePath::empty(),
            )
        {
            return Err(
                buck2_error::Error::from(RegisterCfgConstructorError::NotPackageRoot).into(),
            );
        }
        let package_file_extra: &PackageFileExtra = PackageFileExtra::get_or_init(eval)?;
        if package_file_extra.cfg_constructor.get().is_some() {
            return Err(
                buck2_error::Error::from(RegisterCfgConstructorError::AlreadyRegistered).into(),
            );
        }
        package_file_extra.cfg_constructor.get_or_init(|| {
            eval.heap().alloc_complex(StarlarkCfgConstructor {
                stage0,
                stage1,
                key: key.to_owned(),
                aliases: aliases.into_option(),
                extra_data: extra_data.into_option(),
            })
        });
        Ok(NoneType)
    }
}

pub(crate) fn init_registration() {
    MAKE_CFG_CONSTRUCTOR.init(make_cfg_constructor);
    REGISTER_BUCK2_CFG_CONSTRUCTOR_GLOBALS.init(register_set_cfg_constructor);
}
