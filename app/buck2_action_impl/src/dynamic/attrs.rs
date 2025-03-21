/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::BoundBuildArtifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_build_api::dynamic_value::DynamicValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::unpack_artifact::UnpackArtifactOrDeclaredArtifact;
use buck2_core::deferred::dynamic::DynamicLambdaResultsKey;
use buck2_error::buck2_error;
use dupe::Dupe;
use indexmap::IndexSet;
use starlark::typing::Ty;
use starlark::values::dict::DictRef;
use starlark::values::list::ListRef;
use starlark::values::tuple::TupleRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::typing::TypeCompiled;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::Trace;
use starlark::values::Tracer;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark_map::small_map::SmallMap;

use crate::context::dynamic_output::DynamicActionsOutputArtifactBinder;
use crate::dynamic::dynamic_value::StarlarkDynamicValue;
use crate::dynamic::resolved_dynamic_value::StarlarkResolvedDynamicValue;

#[derive(Clone, Debug, derive_more::Display, Allocative)]
pub(crate) enum DynamicAttrType {
    /// `OutputArtifact`.
    #[display("dynattrs.output()")]
    Output,
    /// Take `Artifact`, provide `ArtifactValue`.
    #[display("dynattrs.artifact_value()")]
    ArtifactValue,
    /// Take `DynamicValue`, provide `ResolvedDynamicValue`.
    #[display("dynattrs.dynamic_value()")]
    DynamicValue,
    /// Pass arbitrary starlark value.
    #[display("dynattrs.value({})", _0)]
    Value(TypeCompiled<FrozenValue>),
    /// List.
    #[display("dynattrs.list({})", _0)]
    List(Box<DynamicAttrType>),
    /// Tuple.
    #[display("dynattrs.tuple({})", _0.iter().map(|x| format!("{}", x)).collect::<Vec<_>>().join(", "))]
    Tuple(Box<[DynamicAttrType]>),
    /// Value or `None`.
    #[display("dynattrs.option({})", _0)]
    Option(Box<DynamicAttrType>),
    /// Dict.
    #[display("dynattrs.dict({}, {})", _0.0, _0.1)]
    Dict(Box<(TypeCompiled<FrozenValue>, DynamicAttrType)>),
}

#[derive(Debug, Trace, Allocative)]
#[trace(bound = "V: Trace<'v>, O: 'static")]
pub(crate) enum DynamicAttrValue<
    // Starlark value passed as is from dynamic actions creation site to impl.
    V: ValueLifetimeless,
    // `OutputArtifact` during creation, and `BoundBuildArtifact` after artifact is bound.
    O,
> {
    Output(#[trace(static)] O),
    ArtifactValue(Artifact),
    DynamicValue(DynamicValue),
    Value(V),
    List(Box<[DynamicAttrValue<V, O>]>),
    Tuple(Box<[DynamicAttrValue<V, O>]>),
    Dict(SmallMap<V, DynamicAttrValue<V, O>>),
    Option(Option<Box<DynamicAttrValue<V, O>>>),
}

// We implement `Freeze` manually because starlark `derive(Freeze)` does not support custom bounds.
impl<V: ValueLifetimeless, O> Freeze for DynamicAttrValue<V, O> {
    type Frozen = DynamicAttrValue<V::Frozen, O>;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        match self {
            DynamicAttrValue::Output(o) => Ok(DynamicAttrValue::Output(o)),
            DynamicAttrValue::ArtifactValue(a) => Ok(DynamicAttrValue::ArtifactValue(a)),
            DynamicAttrValue::DynamicValue(d) => Ok(DynamicAttrValue::DynamicValue(d)),
            DynamicAttrValue::Value(v) => Ok(DynamicAttrValue::Value(v.freeze(freezer)?)),
            DynamicAttrValue::List(l) => Ok(DynamicAttrValue::List(l.freeze(freezer)?)),
            DynamicAttrValue::Dict(d) => Ok(DynamicAttrValue::Dict(d.freeze(freezer)?)),
            DynamicAttrValue::Tuple(t) => Ok(DynamicAttrValue::Tuple(t.freeze(freezer)?)),
            DynamicAttrValue::Option(o) => Ok(DynamicAttrValue::Option(o.freeze(freezer)?)),
        }
    }
}

#[derive(Debug, Allocative)]
pub struct DynamicAttrValues<V: ValueLifetimeless, O: 'static> {
    /// Indexed by attrs definitions in `DynamicActionCallable`.
    pub(crate) values: Box<[DynamicAttrValue<V, O>]>,
}

unsafe impl<'v, V: ValueLifetimeless + Trace<'v>, O> Trace<'v> for DynamicAttrValues<V, O> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        let DynamicAttrValues { values } = self;
        values.trace(tracer);
    }
}

impl<V: ValueLifetimeless, O> Freeze for DynamicAttrValues<V, O> {
    type Frozen = DynamicAttrValues<V::Frozen, O>;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        Ok(DynamicAttrValues {
            values: self.values.freeze(freezer)?,
        })
    }
}

impl<'v> DynamicAttrValue<Value<'v>, BoundBuildArtifact> {
    fn for_each_node(&self, f: &mut impl FnMut(&Self)) {
        f(self);
        match self {
            DynamicAttrValue::Output(_)
            | DynamicAttrValue::ArtifactValue(_)
            | DynamicAttrValue::DynamicValue(_)
            | DynamicAttrValue::Value(_) => {}
            DynamicAttrValue::List(xs) => {
                for x in xs.iter() {
                    x.for_each_node(f);
                }
            }
            DynamicAttrValue::Tuple(xs) => {
                for x in xs.iter() {
                    x.for_each_node(f);
                }
            }
            DynamicAttrValue::Dict(xs) => {
                for x in xs.values() {
                    x.for_each_node(f);
                }
            }
            DynamicAttrValue::Option(x) => {
                if let Some(x) = x {
                    x.for_each_node(f);
                }
            }
        }
    }
}

impl<'v> DynamicAttrValue<Value<'v>, OutputArtifact> {
    fn bind(
        self,
        bind: &mut DynamicActionsOutputArtifactBinder,
    ) -> buck2_error::Result<DynamicAttrValue<Value<'v>, BoundBuildArtifact>> {
        match self {
            DynamicAttrValue::Output(output) => Ok(DynamicAttrValue::Output(bind.bind(output)?)),
            DynamicAttrValue::ArtifactValue(v) => Ok(DynamicAttrValue::ArtifactValue(v)),
            DynamicAttrValue::DynamicValue(v) => Ok(DynamicAttrValue::DynamicValue(v)),
            DynamicAttrValue::Value(v) => Ok(DynamicAttrValue::Value(v)),
            DynamicAttrValue::List(xs) => Ok(DynamicAttrValue::List(
                xs.into_vec()
                    .into_iter()
                    .map(|x| x.bind(bind))
                    .collect::<buck2_error::Result<_>>()?,
            )),
            DynamicAttrValue::Tuple(xs) => Ok(DynamicAttrValue::Tuple(
                xs.into_vec()
                    .into_iter()
                    .map(|x| x.bind(bind))
                    .collect::<buck2_error::Result<_>>()?,
            )),
            DynamicAttrValue::Dict(xs) => {
                let mut r = SmallMap::with_capacity(xs.len());
                for (k, v) in xs.into_iter_hashed() {
                    r.insert_hashed_unique_unchecked(k, v.bind(bind)?);
                }
                Ok(DynamicAttrValue::Dict(r))
            }
            DynamicAttrValue::Option(x) => match x {
                Some(x) => Ok(DynamicAttrValue::Option(Some(Box::new(x.bind(bind)?)))),
                None => Ok(DynamicAttrValue::Option(None)),
            },
        }
    }
}

impl<'v> DynamicAttrValues<Value<'v>, BoundBuildArtifact> {
    fn for_each_node(&self, f: &mut impl FnMut(&DynamicAttrValue<Value<'v>, BoundBuildArtifact>)) {
        for value in &self.values {
            value.for_each_node(f);
        }
    }

    pub(crate) fn outputs(&self) -> IndexSet<BoundBuildArtifact> {
        let mut outputs = IndexSet::new();
        self.for_each_node(&mut |value| {
            if let DynamicAttrValue::Output(output) = value {
                outputs.insert(output.dupe());
            }
        });
        outputs
    }

    pub(crate) fn artifact_values(&self) -> IndexSet<Artifact> {
        let mut artifact_values = IndexSet::new();
        self.for_each_node(&mut |value| {
            if let DynamicAttrValue::ArtifactValue(artifact) = value {
                artifact_values.insert(artifact.dupe());
            }
        });
        artifact_values
    }

    pub(crate) fn dynamic_values(&self) -> IndexSet<DynamicValue> {
        let mut dynamic_values = IndexSet::new();
        self.for_each_node(&mut |value| {
            if let DynamicAttrValue::DynamicValue(dynamic_value) = value {
                dynamic_values.insert(dynamic_value.dupe());
            }
        });
        dynamic_values
    }
}

impl<'v> DynamicAttrValues<Value<'v>, OutputArtifact> {
    pub(crate) fn bind(
        self,
        key: &DynamicLambdaResultsKey,
    ) -> buck2_error::Result<DynamicAttrValues<Value<'v>, BoundBuildArtifact>> {
        let DynamicAttrValues { values } = self;
        let mut bind = DynamicActionsOutputArtifactBinder::new(key);
        Ok(DynamicAttrValues {
            values: values
                .into_vec()
                .into_iter()
                .map(|v| v.bind(&mut bind))
                .collect::<buck2_error::Result<_>>()?,
        })
    }
}

impl DynamicAttrType {
    /// Parameter type of `impl` function.
    pub(crate) fn impl_param_ty(&self) -> Ty {
        match self {
            DynamicAttrType::Output => StarlarkOutputArtifact::starlark_type_repr(),
            DynamicAttrType::ArtifactValue => StarlarkArtifactValue::starlark_type_repr(),
            DynamicAttrType::DynamicValue => StarlarkResolvedDynamicValue::starlark_type_repr(),
            DynamicAttrType::Value(ty) => ty.as_ty().dupe(),
            DynamicAttrType::List(item_ty) => Ty::list(item_ty.impl_param_ty()),
            DynamicAttrType::Tuple(item_tys) => {
                Ty::tuple(item_tys.iter().map(|ty| ty.impl_param_ty()).collect())
            }
            DynamicAttrType::Option(ty) => Ty::union2(ty.impl_param_ty(), Ty::none()),
            DynamicAttrType::Dict(k_v) => {
                let (k, v) = &**k_v;
                Ty::dict(k.as_ty().dupe(), v.impl_param_ty())
            }
        }
    }

    /// Parameter type of callable created by `dynamic_actions`.
    pub(crate) fn callable_param_ty(&self) -> Ty {
        match self {
            DynamicAttrType::Output => StarlarkOutputArtifact::starlark_type_repr(),
            DynamicAttrType::ArtifactValue => {
                UnpackArtifactOrDeclaredArtifact::starlark_type_repr()
            }
            DynamicAttrType::DynamicValue => StarlarkDynamicValue::starlark_type_repr(),
            DynamicAttrType::Value(ty) => ty.as_ty().dupe(),
            DynamicAttrType::List(item_ty) => Ty::list(item_ty.callable_param_ty()),
            DynamicAttrType::Tuple(item_tys) => {
                Ty::tuple(item_tys.iter().map(|ty| ty.callable_param_ty()).collect())
            }
            DynamicAttrType::Option(ty) => Ty::union2(ty.callable_param_ty(), Ty::none()),
            DynamicAttrType::Dict(k_v) => {
                let (k, v) = &**k_v;
                Ty::dict(k.as_ty().dupe(), v.callable_param_ty())
            }
        }
    }

    pub(crate) fn coerce<'v>(
        &self,
        value: Value<'v>,
    ) -> buck2_error::Result<DynamicAttrValue<Value<'v>, OutputArtifact>> {
        match self {
            DynamicAttrType::Output => {
                let artifact = <&StarlarkOutputArtifact>::unpack_value_err(value)?;
                Ok(DynamicAttrValue::Output(artifact.artifact()?))
            }
            DynamicAttrType::ArtifactValue => {
                let artifact = UnpackArtifactOrDeclaredArtifact::unpack_value_err(value)?;
                Ok(DynamicAttrValue::ArtifactValue(artifact.artifact()?))
            }
            DynamicAttrType::DynamicValue => {
                let dynamic_value = <&StarlarkDynamicValue>::unpack_value_err(value)?;
                Ok(DynamicAttrValue::DynamicValue(
                    dynamic_value.dynamic_value.dupe(),
                ))
            }
            DynamicAttrType::Value(ty) => {
                if !ty.matches(value) {
                    return Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Expecting a value of type `{}`, got: {}",
                        ty,
                        value.to_string_for_type_error()
                    ));
                }
                Ok(DynamicAttrValue::Value(value))
            }
            DynamicAttrType::List(elem_ty) => {
                let list = <&ListRef>::unpack_value_err(value)?;
                let mut res = Vec::with_capacity(list.len());
                for elem in list.iter() {
                    res.push(elem_ty.coerce(elem)?);
                }
                Ok(DynamicAttrValue::List(res.into_boxed_slice()))
            }
            DynamicAttrType::Dict(elem_ty) => {
                let (key_ty, value_ty) = &**elem_ty;
                let dict = DictRef::unpack_value_err(value)?;
                let mut res = SmallMap::with_capacity(dict.len());
                for (key, value) in dict.iter_hashed() {
                    if !key_ty.matches(key.into_key()) {
                        return Err(buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "Expecting a key of type `{}`, got: {}",
                            key_ty,
                            key.to_string_for_type_error()
                        ));
                    }
                    res.insert_hashed_unique_unchecked(key, value_ty.coerce(value)?);
                }
                Ok(DynamicAttrValue::Dict(res))
            }
            DynamicAttrType::Tuple(elem_tys) => {
                let tuple = <&TupleRef>::unpack_value_err(value)?;
                if tuple.len() != elem_tys.len() {
                    return Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Expecting a tuple of length {}, got: {}",
                        elem_tys.len(),
                        tuple.len()
                    ));
                }
                let mut res = Vec::with_capacity(elem_tys.len());
                for (elem, ty) in tuple.iter().zip(elem_tys.iter()) {
                    res.push(ty.coerce(elem)?);
                }
                Ok(DynamicAttrValue::Tuple(res.into_boxed_slice()))
            }
            DynamicAttrType::Option(elem_ty) => {
                if value.is_none() {
                    Ok(DynamicAttrValue::Option(None))
                } else {
                    let elem = elem_ty.coerce(value)?;
                    Ok(DynamicAttrValue::Option(Some(Box::new(elem))))
                }
            }
        }
    }
}
