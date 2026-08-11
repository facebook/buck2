/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use display_container::fmt_keyed_container;
use dupe::Dupe;
use either::Either;
use once_cell::unsync::OnceCell;
use starlark_derive::NoSerialize;
use starlark_derive::starlark_module;
use starlark_derive::starlark_value;
use starlark_map::StarlarkHasher;
use starlark_map::small_map::SmallMap;
use starlark_map::sorted_map::SortedMap;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::environment::Methods;
use crate::environment::MethodsBuilder;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::eval::ParametersSpec;
use crate::eval::ParametersSpecParam;
use crate::pagable::StarlarkPagable;
use crate::typing::ParamIsRequired;
use crate::typing::ParamSpec;
use crate::typing::Ty;
use crate::typing::callable::TyCallable;
use crate::typing::starlark_value::TyStarlarkValue;
use crate::typing::user::TyUser;
use crate::typing::user::TyUserFields;
use crate::typing::user::TyUserParams;
use crate::util::ArcStr;
use crate::values::AllocValue;
use crate::values::FreezeBranded;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;
use crate::values::function::FUNCTION_TYPE;
use crate::values::record::Record;
use crate::values::record::field::Field;
use crate::values::record::matcher::RecordTypeMatcher;
use crate::values::record::ty_record_type::TyRecordData;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::type_instance_id::StarlarkTypeIdDomain;
use crate::values::types::type_instance_id::TypeInstanceId;
use crate::values::typing::type_compiled::type_matcher_factory::TypeMatcherFactory;

pub trait RecordVariant: Allocative + Debug + 'static {
    fn get_or_init_ty(
        &self,
        f: impl FnOnce() -> crate::Result<Arc<TyRecordData>>,
    ) -> crate::Result<()>;

    fn get_ty(&self) -> Option<&Arc<TyRecordData>>;
}

#[doc(hidden)]
#[derive(Debug, Allocative)]
pub struct RecordVariantUnfrozen {
    #[allocative(skip)] // FIXME(JakobDegen): Allocative OSS release
    ty: OnceCell<Arc<TyRecordData>>,
}

impl RecordVariant for RecordVariantUnfrozen {
    fn get_or_init_ty(
        &self,
        f: impl FnOnce() -> crate::Result<Arc<TyRecordData>>,
    ) -> crate::Result<()> {
        self.ty.get_or_try_init(f)?;
        Ok(())
    }

    fn get_ty(&self) -> Option<&Arc<TyRecordData>> {
        self.ty.get()
    }
}

#[doc(hidden)]
#[derive(Debug, Allocative, starlark_derive::StarlarkPagable)]
pub struct RecordVariantFrozen {
    pub(crate) ty: Option<Arc<TyRecordData>>,
}

impl RecordVariant for RecordVariantFrozen {
    fn get_or_init_ty(
        &self,
        _f: impl FnOnce() -> crate::Result<Arc<TyRecordData>>,
    ) -> crate::Result<()> {
        // `ty` is fixed at freeze time, so this is intentionally a no-op;
        // callers handle `get_ty` returning `None`.
        Ok(())
    }

    fn get_ty(&self) -> Option<&Arc<TyRecordData>> {
        self.ty.as_ref()
    }
}

#[derive(Debug, thiserror::Error)]
enum RecordTypeError {
    #[error(
        "Record instance cannot be created if record type is not assigned to a global variable"
    )]
    RecordTypeNotAssigned,
}

/// The result of `record()`, being the type of records.
#[derive(
    Debug,
    Trace,
    NoSerialize,
    ProvidesStaticType,
    Allocative,
    starlark_derive::StarlarkPagable
)]
#[starlark_pagable(bound = "V: StarlarkPagable")]
#[trace(bound = "")]
pub struct RecordTypeGen<'v, V: RecordVariant + 'static> {
    pub(crate) id: TypeInstanceId,
    #[trace(static)]
    pub(crate) ty_record_data: V,
    /// The V is the type the field must satisfy (e.g. `"string"`)
    pub(crate) fields: SmallMap<String, Field<'v>>,
}

impl<'v, V: RecordVariant> Display for RecordTypeGen<'v, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_keyed_container(f, "record(", ")", "=", &self.fields)
    }
}

/// Type of a record in a heap.
pub type RecordType<'v> = RecordTypeGen<'v, RecordVariantUnfrozen>;
/// Type of a record in a frozen heap.
pub type FrozenRecordType<'v> = RecordTypeGen<'v, RecordVariantFrozen>;

crate::register_simple_vtable_entry!(FrozenRecordType<'static>);
// SAFETY: The vtable entry is registered above. The deser type id is
// lifetime-erased, so the `'static` instantiation covers all heap lifetimes.
unsafe impl<'v> crate::__derive_refs::VtableRegistered for FrozenRecordType<'v> {}
crate::register_ty_starlark_value!(RecordType<'_>);
crate::register_ty_starlark_value!(FrozenRecordType<'_>);

pub(super) type AnyRecordType<'v> = Either<&'v RecordType<'v>, &'v FrozenRecordType<'v>>;

impl<'v> AllocValue<'v> for RecordType<'v> {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        heap.alloc_complex_branded(self)
    }
}

impl<'v, V: RecordVariant> StarlarkTypeRepr for &'v RecordTypeGen<'v, V> {
    type Canonical = <RecordTypeGen<'v, V> as StarlarkValue<'v>>::Canonical;

    #[inline]
    fn starlark_type_repr() -> crate::typing::Ty {
        <RecordTypeGen<'v, V> as StarlarkValue<'v>>::get_type_starlark_repr()
    }
}

impl<'v, V: RecordVariant> UnpackValue<'v> for &'v RecordTypeGen<'v, V> {
    type Error = std::convert::Infallible;

    #[inline]
    fn unpack_value_impl(x: Value<'v>) -> Result<Option<Self>, Self::Error> {
        Ok(x.downcast_ref())
    }
}

pub(super) fn record_fields<'v>(
    x: Either<&'v RecordType<'v>, &'v FrozenRecordType<'v>>,
) -> &'v SmallMap<String, Field<'v>> {
    x.either(|x| &x.fields, |x| &x.fields)
}

impl<'v> RecordType<'v> {
    /// Creates a new `RecordType`.
    pub fn new(fields: SmallMap<String, Field<'v>>, id: TypeInstanceId) -> Self {
        Self {
            id,
            fields,
            ty_record_data: RecordVariantUnfrozen {
                ty: OnceCell::new(),
            },
        }
    }
}

impl<'v> FreezeBranded for RecordType<'v> {
    type Frozen<'fv> = FrozenRecordType<'fv>;

    fn freeze<'fv>(self, freezer: &Freezer<'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        Ok(FrozenRecordType {
            id: self.id,
            fields: self.fields.freeze(freezer)?,
            ty_record_data: RecordVariantFrozen {
                ty: self.ty_record_data.ty.into_inner(),
            },
        })
    }
}

impl<'v, V: RecordVariant> RecordTypeGen<'v, V> {
    fn ty_record_data(&self) -> Option<&Arc<TyRecordData>> {
        V::get_ty(&self.ty_record_data)
    }

    pub(super) fn instance_ty(&self) -> Ty {
        self.ty_record_data()
            .expect("Instances can only be created if named are assigned")
            .ty_record
            .dupe()
    }

    pub(crate) fn make_parameter_spec(
        name: &str,
        fields: &SmallMap<String, Field<'_>>,
    ) -> ParametersSpec<FrozenValue> {
        ParametersSpec::new_named_only(
            name,
            fields.iter().map(|(name, field)| {
                (
                    name.as_str(),
                    match field.default {
                        None => ParametersSpecParam::Required,
                        Some(_default) => ParametersSpecParam::Optional,
                    },
                )
            }),
        )
    }
}

starlark::methods_static!(RECORD_TYPE_METHODS = record_type_methods);

#[starlark_value(type = FUNCTION_TYPE)]
impl<'v, V: RecordVariant> StarlarkValue<'v> for RecordTypeGen<'v, V> {
    type Canonical = FrozenRecordType<'v>;

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> crate::Result<()> {
        for (name, typ) in &self.fields {
            name.hash(hasher);
            // No need to hash typ.1, since it was computed from typ.0
            typ.write_hash(hasher)?;
        }
        Ok(())
    }

    fn invoke(
        &self,
        me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> crate::Result<Value<'v>> {
        let Some(ty_record_data) = self.ty_record_data() else {
            return Err(crate::Error::new_other(
                RecordTypeError::RecordTypeNotAssigned,
            ));
        };

        let this = me;

        ty_record_data
            .parameter_spec
            .parser(args, eval, |param_parser, eval| {
                let fields = record_fields(AnyRecordType::unpack_value_err(this).unwrap());
                let mut values = Vec::with_capacity(fields.len());
                for (name, field) in fields.iter() {
                    let value = match field.default {
                        None => {
                            let v: Value = param_parser.next()?;
                            field.typ.check_type(v, Some(name))?;
                            v
                        }
                        Some(default) => {
                            let v: Option<Value> = param_parser.next_opt()?;
                            match v {
                                None => default,
                                Some(v) => {
                                    field.typ.check_type(v, Some(name))?;
                                    v
                                }
                            }
                        }
                    };
                    values.push(value);
                }
                Ok(eval.heap().alloc_complex_branded(Record {
                    typ: this,
                    values: values.into_boxed_slice(),
                }))
            })
    }

    fn get_methods() -> Option<&'static Methods>
    where
        Self: Sized,
    {
        Some(RECORD_TYPE_METHODS.methods())
    }

    fn eval_type(&self) -> Option<Ty> {
        self.ty_record_data().map(|t| t.ty_record.dupe())
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        self.ty_record_data().map(|t| t.ty_record_type.dupe())
    }

    fn export_as(
        &self,
        variable_name: &str,
        _eval: &mut Evaluator<'v, '_, '_>,
    ) -> crate::Result<()> {
        V::get_or_init_ty(&self.ty_record_data, || {
            let fields: SortedMap<String, Ty> = self
                .fields
                .iter()
                .map(|(name, field)| (name.clone(), field.ty()))
                .collect();

            let ty_record = Ty::custom(TyUser::new(
                variable_name.to_owned(),
                TyStarlarkValue::new::<Record>(),
                self.id,
                TyUserParams {
                    matcher: Some(TypeMatcherFactory::new(RecordTypeMatcher { id: self.id })),
                    fields: TyUserFields {
                        known: fields,
                        unknown: false,
                    },
                    ..TyUserParams::default()
                },
            )?);

            let ty_record_type = Ty::custom(TyUser::new(
                format!("record[{variable_name}]"),
                TyStarlarkValue::new::<RecordType>(),
                TypeInstanceId::from_identity(StarlarkTypeIdDomain::RecordTypeOfType, &self.id),
                TyUserParams {
                    callable: Some(TyCallable::new(
                        ParamSpec::new_named_only(self.fields.iter().map(|(name, field)| {
                            (
                                ArcStr::from(name.as_str()),
                                if field.default.is_some() {
                                    ParamIsRequired::No
                                } else {
                                    ParamIsRequired::Yes
                                },
                                field.ty(),
                            )
                        }))?,
                        ty_record.dupe(),
                    )),
                    ..TyUserParams::default()
                },
            )?);

            Ok(Arc::new(TyRecordData {
                name: variable_name.to_owned(),
                ty_record,
                ty_record_type,
                parameter_spec: Self::make_parameter_spec(variable_name, &self.fields),
            }))
        })
    }
}

#[starlark_module]
fn record_type_methods(methods: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn r#type<'v>(this: AnyRecordType<'v>) -> starlark::Result<&'v str> {
        let ty_record_type = match this {
            Either::Left(x) => x.ty_record_data.get_ty(),
            Either::Right(x) => x.ty_record_data.get_ty(),
        };
        Ok(ty_record_type.map_or(Record::TYPE, |s| s.name.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_record_type_as_type_pass() {
        assert::pass(
            r"
RecPass = record(a = field(int), b = field(int))

def f_pass(x: RecPass):
    return x.a

f_pass(RecPass(a = 1, b = 2))
",
        );
    }

    #[test]
    fn test_record_type_as_type_compile_time() {
        assert::fail_golden(
            "src/values/types/record/record_type/record_type_as_type_compile_time.golden",
            r"
RecFailCt1 = record(a = field(int), b = field(int))
RecFailCt2 = record(a = field(int), b = field(int))

def f_fail_ct(x: RecFailCt1):
    return x.a

def test():
    f_fail_ct(RecFailCt2(a = 1, b = 2))
",
        );
    }

    #[test]
    fn test_record_type_as_type_runtime() {
        assert::fail_golden(
            "src/values/types/record/record_type/record_type_as_type_runtime.golden",
            r"
RecFailRt1 = record(a = field(int), b = field(int))
RecFailRt2 = record(a = field(int), b = field(int))

def f_fail_rt(x: RecFailRt1):
    return x.a

noop(f_fail_rt)(RecFailRt2(a = 1, b = 2))
",
        );
    }

    #[test]
    fn test_anon_record() {
        assert::fail_golden(
            "src/values/types/record/record_type/anon_record.golden",
            "record(a = field(int))(a = 1)",
        );
    }

    #[test]
    fn test_missing_field_error() {
        assert::fail_golden(
            "src/values/types/record/record_type/missing_field_error.golden",
            r#"
RecFail = record(a = field(int), b = field(int))

_x = RecFail(a = 1)
"#,
        );
    }
}
