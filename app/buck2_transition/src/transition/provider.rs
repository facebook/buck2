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
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_complex_value;
use starlark::starlark_module;
use starlark::typing::ParamIsRequired;
use starlark::typing::ParamSpec;
use starlark::util::ArcStr;
use starlark::values::list::ListRef;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::none::NoneOr;
use starlark::values::tuple::TupleRef;
use starlark::values::typing::StarlarkCallable;
use starlark::values::typing::StarlarkCallableChecked;
use starlark::values::typing::StarlarkCallableParamSpec;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUncheckedGeneric;

use crate::transition::starlark::ImplSingleReturnTy;
use crate::transition::starlark::IMPL_ATTRS_PARAM;
use crate::transition::starlark::IMPL_PLATFORM_PARAM;

struct TransitionImplParams;

impl StarlarkCallableParamSpec for TransitionImplParams {
    fn params() -> ParamSpec {
        ParamSpec::new_named_only([
            (
                ArcStr::new_static(IMPL_PLATFORM_PARAM.name),
                ParamIsRequired::Yes,
                IMPL_PLATFORM_PARAM.ty.dupe(),
            ),
            (
                ArcStr::new_static(IMPL_ATTRS_PARAM.name),
                ParamIsRequired::No,
                IMPL_ATTRS_PARAM.ty.dupe(),
            ),
        ])
        .unwrap()
    }
}

#[internal_provider(transition_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[repr(C)]
pub(crate) struct TransitionInfoGen<V: ValueLifetimeless> {
    pub(crate) impl_: ValueOfUncheckedGeneric<V, StarlarkCallable<'static>>,
    pub(crate) attrs: ValueOfUncheckedGeneric<V, NoneOr<UnpackListOrTuple<String>>>,
}

#[starlark_module]
fn transition_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenTransitionInfo)]
    fn TransitionInfo<'v>(
        #[starlark(require = named)] r#impl: StarlarkCallableChecked<
            'v,
            TransitionImplParams,
            ImplSingleReturnTy,
        >,
        #[starlark(require = named)] attrs: Option<ValueOf<'v, UnpackListOrTuple<String>>>,
    ) -> starlark::Result<TransitionInfo<'v>> {
        let attrs = attrs.map_or(Value::new_none(), |v| v.value);
        Ok(TransitionInfo {
            impl_: ValueOfUncheckedGeneric::new(r#impl.0),
            attrs: ValueOfUncheckedGeneric::new(attrs),
        })
    }
}

impl<'v, V: ValueLike<'v>> TransitionInfoGen<V> {
    pub(crate) fn get_attrs_names(&self) -> Option<impl IntoIterator<Item = &'v str>> {
        let v = self.attrs.get().to_value();
        let slice: &[_] = if v.is_none() {
            return None;
        } else if let Some(v) = ListRef::from_value(v) {
            v
        } else if let Some(v) = TupleRef::from_value(v) {
            v.content()
        } else {
            unreachable!("Checked at construction");
        };
        Some(slice.iter().map(|v| {
            v.unpack_starlark_str()
                .expect("checked at construction")
                .as_str()
        }))
    }
}
