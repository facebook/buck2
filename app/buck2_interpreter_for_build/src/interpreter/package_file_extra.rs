/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::OnceCell;
use std::cell::RefCell;
use std::sync::Arc;

use allocative::Allocative;
use buck2_node::cfg_constructor::CfgConstructorImpl;
use buck2_node::metadata::key::MetadataKey;
use buck2_util::late_binding::LateBinding;
use starlark::any::ProvidesStaticType;
use starlark::environment::FrozenModule;
use starlark::eval::Evaluator;
use starlark::values::starlark_value;
use starlark::values::Freeze;
use starlark::values::FreezeErrorContext;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::NoSerialize;
use starlark::values::OwnedFrozenRef;
use starlark::values::OwnedFrozenValue;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Tracer;
use starlark::values::Value;
use starlark_map::small_map::SmallMap;

use crate::interpreter::extra_value::FrozenInterpreterExtraValue;
use crate::interpreter::extra_value::InterpreterExtraValue;
use crate::super_package::package_value::FrozenStarlarkPackageValue;
use crate::super_package::package_value::StarlarkPackageValue;

/// `Module.extra_value` when evaluating `PACKAGE` file.
#[derive(
    Default,
    Debug,
    NoSerialize,
    derive_more::Display,
    ProvidesStaticType,
    Allocative
)]
#[display("{:?}", self)]
pub struct PackageFileExtra<'v> {
    pub cfg_constructor: OnceCell<Value<'v>>,
    pub(crate) package_values: RefCell<SmallMap<MetadataKey, StarlarkPackageValue<'v>>>,
}

unsafe impl<'v> Trace<'v> for PackageFileExtra<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        let PackageFileExtra {
            cfg_constructor,
            package_values,
        } = self;
        cfg_constructor.trace(tracer);
        for (k, v) in package_values.get_mut().iter_mut() {
            fn assert_static<T: 'static>(_t: &T) {}
            assert_static(k);
            v.trace(tracer);
        }
    }
}

#[derive(
    Debug,
    NoSerialize,
    derive_more::Display,
    ProvidesStaticType,
    Allocative
)]
#[display("{:?}", self)]
pub struct FrozenPackageFileExtra {
    pub(crate) cfg_constructor: Option<FrozenValue>,
    pub(crate) package_values: SmallMap<MetadataKey, FrozenStarlarkPackageValue>,
}

/// Resolve `FrozenPackageFileExtra.cfg_constructor` to a `CfgConstructorImpl`.
pub static MAKE_CFG_CONSTRUCTOR: LateBinding<
    fn(OwnedFrozenValue) -> buck2_error::Result<Arc<dyn CfgConstructorImpl>>,
> = LateBinding::new("MAKE_CFG_CONSTRUCTOR");

// TODO(nga): this does not need to be fully starlark_value,
// but we don't have lighter machinery for that.
#[starlark_value(type = "PackageFileExtra")]
impl<'v> StarlarkValue<'v> for PackageFileExtra<'v> {}

#[starlark_value(type = "PackageFileExtra")]
impl<'v> StarlarkValue<'v> for FrozenPackageFileExtra {
    type Canonical = FrozenPackageFileExtra;
}

impl<'v> Freeze for PackageFileExtra<'v> {
    type Frozen = FrozenPackageFileExtra;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let PackageFileExtra {
            cfg_constructor,
            package_values,
        } = self;
        let cfg_constructor = cfg_constructor.into_inner().freeze(freezer)?;
        let package_values = package_values
            .into_inner()
            .into_iter_hashed()
            .map(|(k, v)| {
                let v = v
                    .freeze(freezer)
                    .freeze_error_context(&format!("freezing `{k}`"))?;
                Ok((k, v))
            })
            .collect::<FreezeResult<SmallMap<MetadataKey, FrozenStarlarkPackageValue>>>()?;
        Ok(FrozenPackageFileExtra {
            cfg_constructor,
            package_values,
        })
    }
}

impl<'v> PackageFileExtra<'v> {
    pub fn get_or_init(
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<&'v PackageFileExtra<'v>> {
        Ok(InterpreterExtraValue::get(eval.module())?
            .package_extra
            .get_or_init(Default::default))
    }
}

impl FrozenPackageFileExtra {
    pub(crate) fn get(
        module: &FrozenModule,
    ) -> buck2_error::Result<Option<OwnedFrozenRef<FrozenPackageFileExtra>>> {
        Ok(FrozenInterpreterExtraValue::get(module)?
            .into_owned_frozen_ref()
            .try_map_option(|x| x.value.package_extra.as_ref()))
    }
}
