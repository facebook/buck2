/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::tuple::UnpackTuple;
use starlark::values::typing::TypeCompiled;
use starlark::values::typing::TypeType;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueOf;

use crate::dynamic::attrs::DynamicAttrType;

/// Attribute type for dynamic actions. Created from `dynattrs` module.
#[derive(
    Debug,
    derive_more::Display,
    ProvidesStaticType,
    Allocative,
    NoSerialize
)]
#[display("{}", ty)]
pub struct StarlarkDynamicAttrType {
    pub(crate) ty: DynamicAttrType,
}

#[starlark_value(type = "DynamicAttrType", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkDynamicAttrType {}

impl<'v> AllocValue<'v> for StarlarkDynamicAttrType {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

#[starlark_module]
fn struct_dynattrs(globals: &mut GlobalsBuilder) {
    fn output() -> starlark::Result<StarlarkDynamicAttrType> {
        Ok(StarlarkDynamicAttrType {
            ty: DynamicAttrType::Output,
        })
    }

    fn artifact_value() -> starlark::Result<StarlarkDynamicAttrType> {
        Ok(StarlarkDynamicAttrType {
            ty: DynamicAttrType::ArtifactValue,
        })
    }

    fn dynamic_value() -> starlark::Result<StarlarkDynamicAttrType> {
        Ok(StarlarkDynamicAttrType {
            ty: DynamicAttrType::DynamicValue,
        })
    }

    fn value<'v>(
        #[starlark(require = pos)] ty: ValueOf<'v, TypeType>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<StarlarkDynamicAttrType> {
        let ty = TypeCompiled::new(ty.value, eval.heap())?;
        // We allocate a type in the frozen heap (which is not garbage collected),
        // which is fine because this code is not meant to be executed outside top-level code.
        let ty = ty.to_frozen(eval.frozen_heap());
        Ok(StarlarkDynamicAttrType {
            ty: DynamicAttrType::Value(ty),
        })
    }

    fn list<'v>(
        #[starlark(require = pos)] ty: &'v StarlarkDynamicAttrType,
    ) -> starlark::Result<StarlarkDynamicAttrType> {
        let ty = ty.ty.clone();
        Ok(StarlarkDynamicAttrType {
            ty: DynamicAttrType::List(Box::new(ty)),
        })
    }

    fn dict<'v>(
        #[starlark(require = pos)] key: ValueOf<'v, TypeType>,
        #[starlark(require = pos)] value: &'v StarlarkDynamicAttrType,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<StarlarkDynamicAttrType> {
        let key = TypeCompiled::new(key.value, eval.heap())?;
        // See the comment above about frozen heap.
        let key = key.to_frozen(eval.frozen_heap());
        let value = value.ty.clone();
        Ok(StarlarkDynamicAttrType {
            ty: DynamicAttrType::Dict(Box::new((key, value))),
        })
    }

    fn tuple<'v>(
        #[starlark(args)] args: UnpackTuple<&'v StarlarkDynamicAttrType>,
    ) -> starlark::Result<StarlarkDynamicAttrType> {
        let items = args.items.into_iter().map(|x| x.ty.clone()).collect();
        Ok(StarlarkDynamicAttrType {
            ty: DynamicAttrType::Tuple(items),
        })
    }

    fn option<'v>(
        #[starlark(require = pos)] ty: &'v StarlarkDynamicAttrType,
    ) -> starlark::Result<StarlarkDynamicAttrType> {
        let ty = ty.ty.clone();
        Ok(StarlarkDynamicAttrType {
            ty: DynamicAttrType::Option(Box::new(ty)),
        })
    }

    const DynamicAttrType: StarlarkValueAsType<StarlarkDynamicAttrType> =
        StarlarkValueAsType::new();
}

pub(crate) fn register_dynamic_attrs(globals: &mut GlobalsBuilder) {
    globals.namespace("dynattrs", struct_dynattrs);
}
