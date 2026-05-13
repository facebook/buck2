/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::RefCell;
use std::sync::Arc;
use std::sync::LazyLock;

use allocative::Allocative;
use buck2_build_api::interpreter::rule_defs::provider::builtin::platform_info::PlatformInfo;
use buck2_core::bzl::ImportPath;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ProvidersLabel;
use buck2_error::BuckErrorContext;
use buck2_hash::StdBuckHashSet;
use buck2_interpreter::build_context::starlark_path_from_build_context;
use buck2_interpreter::coerce::COERCE_PROVIDERS_LABEL_FOR_BZL;
use buck2_interpreter::downstream_crate_starlark_defs::REGISTER_BUCK2_TRANSITION_GLOBALS;
use buck2_interpreter::late_binding_ty::TransitionReprLate;
use buck2_interpreter::types::transition::TransitionValue;
use derive_more::Display;
use dupe::Dupe;
use either::Either;
use gazebo::prelude::*;
use pagable::PagableDeserialize;
use pagable::PagableSerialize;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::pagable::StarlarkDeserialize;
use starlark::pagable::StarlarkDeserializeContext;
use starlark::pagable::StarlarkSerialize;
use starlark::pagable::StarlarkSerializeContext;
use starlark::starlark_complex_values;
use starlark::starlark_module;
use starlark::typing::ParamIsRequired;
use starlark::typing::ParamSpec;
use starlark::typing::Ty;
use starlark::util::ArcStr;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValue;
use starlark::values::NoSerialize;
use starlark::values::StarlarkPagable;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::dict::DictType;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::starlark_value;
use starlark::values::structs::StructRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::typing::StarlarkCallableChecked;
use starlark::values::typing::StarlarkCallableParamSpec;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum TransitionError {
    #[error("Transition must be assigned to a variable, e.g. `android_cpus = transition(...)`")]
    TransitionNotAssigned,
    #[error("`transition` can only be declared in .bzl files")]
    OnlyBzl,
    #[error("Non-unique list of attrs")]
    NonUniqueAttrs,
}

/// Wrapper for `ProvidersTargetLabel` which is `Trace`.
#[derive(Trace, Debug, Allocative)]
struct ProvidersLabelTrace(ProvidersLabel);

#[derive(Debug, Display, Trace, ProvidesStaticType, NoSerialize, Allocative)]
#[display("transition")]
pub(crate) struct Transition<'v> {
    /// The name of this transition, filled in by `export_as()`. This must be set before this
    /// object can be used.
    id: RefCell<Option<Arc<TransitionId>>>,
    /// The path where this `Transition` is created and assigned.
    path: ImportPath,
    implementation: Value<'v>,
    /// Providers needed for the transition function. A map by target label.
    refs: SmallMap<StringValue<'v>, ProvidersLabelTrace>,
    /// Transition function accesses theses attributes.
    attrs: Option<Vec<StringValue<'v>>>,
    /// Is this split transition? I. e. transition to multiple configurations.
    split: bool,
}

#[derive(
    Debug,
    Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative,
    StarlarkPagable
)]
#[display("transition")]
pub(crate) struct FrozenTransition {
    #[starlark_pagable(pagable)]
    id: Arc<TransitionId>,
    pub(crate) implementation: FrozenValue,
    // Mixed: keys are starlark, `ProvidersLabel` is pagable-only (`buck2_core`
    // cannot depend on `starlark`) — so the generic `SmallMap<K, V>:
    // StarlarkSerialize` blanket doesn't apply. Bridge here.
    #[starlark_pagable(
        serialize_with = "serialize_refs",
        deserialize_with = "deserialize_refs"
    )]
    pub(crate) refs: SmallMap<FrozenStringValue, ProvidersLabel>,
    pub(crate) attrs_names: Option<Vec<FrozenStringValue>>,
    #[starlark_pagable(pagable)]
    pub(crate) split: bool,
}

fn serialize_refs(
    field: &SmallMap<FrozenStringValue, ProvidersLabel>,
    ctx: &mut dyn StarlarkSerializeContext,
) -> starlark::Result<()> {
    PagableSerialize::pagable_serialize(&field.len(), ctx.pagable())?;
    for (k, v) in field.iter() {
        StarlarkSerialize::starlark_serialize(k, ctx)?;
        PagableSerialize::pagable_serialize(v, ctx.pagable())?;
    }
    Ok(())
}

fn deserialize_refs(
    ctx: &mut dyn StarlarkDeserializeContext<'_>,
) -> starlark::Result<SmallMap<FrozenStringValue, ProvidersLabel>> {
    let len = usize::pagable_deserialize(ctx.pagable())?;
    let mut map = SmallMap::with_capacity(len);
    for _ in 0..len {
        let k = FrozenStringValue::starlark_deserialize(ctx)?;
        let v = <ProvidersLabel as PagableDeserialize>::pagable_deserialize(ctx.pagable())?;
        map.insert(k, v);
    }
    Ok(map)
}

#[starlark_value(type = "Transition", skip_pagable)]
impl<'v> StarlarkValue<'v> for Transition<'v> {
    fn export_as(
        &self,
        variable_name: &str,
        _eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<()> {
        let mut id = self.id.borrow_mut();
        // First export wins
        if id.is_none() {
            *id = Some(Arc::new(TransitionId::MagicObject {
                path: self.path.clone(),
                name: variable_name.to_owned(),
            }));
        }
        Ok(())
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn TransitionValue>(self);
    }
}

#[starlark_value(type = "Transition", skip_pagable)]
impl<'v> StarlarkValue<'v> for FrozenTransition {
    type Canonical = Transition<'v>;

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn TransitionValue>(self);
    }
}

impl Freeze for Transition<'_> {
    type Frozen = FrozenTransition;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<FrozenTransition> {
        let implementation = freezer.freeze(self.implementation)?;
        let id = self.id.into_inner().ok_or(FreezeError::new(
            TransitionError::TransitionNotAssigned.to_string(),
        ))?;
        // N.B. collect::<Result<_>> sets the lower bound to zero,
        // which can cause over-allocations in frozen containers.
        let mut refs = SmallMap::with_capacity(self.refs.len());
        for (k, v) in self.refs {
            refs.insert(k.freeze(freezer)?, v.0);
        }
        let attrs = self
            .attrs
            .map(|a| a.into_try_map(|a| a.freeze(freezer)))
            .transpose()?;
        let split = self.split;
        Ok(FrozenTransition {
            id,
            implementation,
            refs,
            attrs_names: attrs,
            split,
        })
    }
}

starlark_complex_values!(Transition);

impl TransitionValue for Transition<'_> {
    fn transition_id(&self) -> buck2_error::Result<Arc<TransitionId>> {
        self.id
            .borrow()
            .as_ref()
            .map(Dupe::dupe)
            .ok_or_else(|| TransitionError::TransitionNotAssigned.into())
    }
}

impl TransitionValue for FrozenTransition {
    fn transition_id(&self) -> buck2_error::Result<Arc<TransitionId>> {
        Ok(self.id.dupe())
    }
}

pub(crate) struct ParamNameAndType {
    pub(crate) name: pagable::StaticStr,
    pub(crate) ty: LazyLock<Ty>,
}

pagable::static_str!(IMPL_PLATFORM_NAME = "platform");
pagable::static_str!(IMPL_REFS_NAME = "refs");
pagable::static_str!(IMPL_ATTRS_NAME = "attrs");

pub(crate) static IMPL_PLATFORM_PARAM: ParamNameAndType = ParamNameAndType {
    name: IMPL_PLATFORM_NAME,
    ty: LazyLock::new(PlatformInfo::starlark_type_repr),
};
static IMPL_REFS_PARAM: ParamNameAndType = ParamNameAndType {
    name: IMPL_REFS_NAME,
    ty: LazyLock::new(StructRef::starlark_type_repr),
};
pub(crate) static IMPL_ATTRS_PARAM: ParamNameAndType = ParamNameAndType {
    name: IMPL_ATTRS_NAME,
    ty: LazyLock::new(StructRef::starlark_type_repr),
};

pub(crate) type ImplSingleReturnTy<'v> = PlatformInfo<'v>;
type ImplSplitReturnTy<'v> = DictType<String, PlatformInfo<'v>>;

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
                ArcStr::new_static(IMPL_REFS_PARAM.name),
                ParamIsRequired::Yes,
                IMPL_REFS_PARAM.ty.dupe(),
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

// This function is not optimized, but it is called like 10 times during the heavy build.
fn validate_transition_impl(
    implementation: Value,
    attrs: bool,
    split: bool,
) -> buck2_error::Result<()> {
    let expected_return_type = match split {
        false => ImplSingleReturnTy::starlark_type_repr(),
        true => ImplSplitReturnTy::starlark_type_repr(),
    };

    implementation
        .check_callable_with(
            [],
            [
                (IMPL_PLATFORM_PARAM.name.as_str(), &*IMPL_PLATFORM_PARAM.ty),
                (IMPL_REFS_PARAM.name.as_str(), &*IMPL_REFS_PARAM.ty),
            ]
            .into_iter()
            .chain(match attrs {
                true => Some((IMPL_ATTRS_PARAM.name.as_str(), &*IMPL_ATTRS_PARAM.ty)),
                false => None,
            }),
            None,
            None,
            &expected_return_type,
        )
        .buck_error_context("`impl` function signature is incorrect")
}

#[starlark_module]
fn register_transition_function(builder: &mut GlobalsBuilder) {
    fn transition<'v>(
        #[starlark(require = named)] r#impl: StarlarkCallableChecked<
            'v,
            TransitionImplParams,
            Either<ImplSingleReturnTy, ImplSplitReturnTy>,
        >,
        #[starlark(require = named)] refs: UnpackDictEntries<StringValue<'v>, StringValue<'v>>,
        #[starlark(require = named)] attrs: Option<UnpackListOrTuple<StringValue<'v>>>,
        #[starlark(require = named, default = false)] split: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Transition<'v>> {
        let implementation = r#impl.0;

        let refs = refs
            .entries
            .into_iter()
            .map(|(n, r)| {
                Ok((
                    n,
                    ProvidersLabelTrace((COERCE_PROVIDERS_LABEL_FOR_BZL.get()?)(eval, &r)?),
                ))
            })
            .collect::<buck2_error::Result<_>>()?;

        let path: ImportPath = (*starlark_path_from_build_context(eval)?
            .unpack_load_file()
            .ok_or(buck2_error::Error::from(TransitionError::OnlyBzl))?)
        .clone();

        if let Some(attrs) = &attrs {
            let attrs_set: StdBuckHashSet<StringValue> = attrs.items.iter().copied().collect();
            if attrs_set.len() != attrs.items.len() {
                return Err(buck2_error::Error::from(TransitionError::NonUniqueAttrs).into());
            }
        };

        validate_transition_impl(implementation, attrs.is_some(), split)?;

        Ok(Transition {
            id: RefCell::new(None),
            path,
            implementation,
            refs,
            attrs: attrs.map(|a| a.items),
            split,
        })
    }
}

pub(crate) fn init_register_transition() {
    REGISTER_BUCK2_TRANSITION_GLOBALS.init(register_transition_function);
    TransitionReprLate::init(Transition::starlark_type_repr());
}
