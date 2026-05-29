/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::provider::id::ProviderId;
use display_container::fmt_keyed_container;
use dupe::Dupe;
use indexmap::map::RawEntryApiV1;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::coerce::coerce;
use starlark::collections::Hashed;
use starlark::collections::StarlarkHasher;
use starlark::eval::Evaluator;
use starlark::eval::ParametersParser;
use starlark::typing::Ty;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::StarlarkPagable;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::any::FrozenAnyValue;
use starlark::values::starlark_value;

use crate::interpreter::rule_defs::provider::ProviderLike;
use crate::interpreter::rule_defs::provider::callable::UserProviderCallableData;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum UserProviderError {
    #[error("Value for parameter `{0}` mismatches type `{1}`: `{2}`")]
    MismatchedType(String, Ty, String),
    #[error("Required parameter `{0}` is missing")]
    MissingParameter(String),
}

/// The result of calling the output of `provider()`. This is just a simple data structure of
/// either immediately available values or, later, `FutureValue` types that are resolved
/// asynchronously
#[derive(
    Debug,
    Clone,
    Coerce,
    Trace,
    Freeze,
    ProvidesStaticType,
    Allocative,
    StarlarkPagable
)]
#[repr(C)]
pub struct UserProviderGen<'v, V: ValueLike<'v>> {
    pub(crate) callable: FrozenAnyValue<UserProviderCallableData>,
    attributes: Box<[V]>,
    _marker: PhantomData<&'v ()>,
}

starlark_complex_value!(pub UserProvider<'v>);

impl<'v, V: ValueLike<'v>> UserProviderGen<'v, V> {
    pub(crate) fn callable_data(&self) -> &UserProviderCallableData {
        &self.callable
    }

    fn iter_items(&self) -> impl Iterator<Item = (&str, V)> {
        let callable_data = self.callable_data();
        assert_eq!(callable_data.fields.len(), self.attributes.len());
        callable_data
            .fields
            .keys()
            .map(|s| s.as_str())
            .zip(self.attributes.iter().copied())
    }
}

impl<'v, V: ValueLike<'v>> Display for UserProviderGen<'v, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_keyed_container(
            f,
            &format!("{}(", self.callable_data().provider_id.name),
            ")",
            "=",
            self.iter_items(),
        )
    }
}

#[starlark_value(type = "Provider")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for UserProviderGen<'v, V>
where
    Self: ProvidesStaticType<'v>,
{
    fn dir_attr(&self) -> Vec<String> {
        self.callable_data().fields.keys().cloned().collect()
    }

    fn get_attr(&self, attribute: &str, _heap: Heap<'v>) -> Option<Value<'v>> {
        self.get_attr_hashed(Hashed::new(attribute), _heap)
    }

    fn get_attr_hashed(&self, attribute: Hashed<&str>, _heap: Heap<'v>) -> Option<Value<'v>> {
        let index = self
            .callable_data()
            .fields
            .raw_entry_v1()
            .index_from_hash(attribute.hash().promote(), |k| k == attribute.key())?;
        Some(self.attributes[index].to_value())
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        let this: &UserProvider = coerce(self);
        let other: &UserProvider = match UserProvider::from_value(other) {
            Some(other) => other,
            None => return Ok(false),
        };
        if this.callable_data().provider_id != other.callable_data().provider_id {
            return Ok(false);
        }
        if this.attributes.len() != other.attributes.len() {
            // If provider ids are equal, then providers point to the same provider callable,
            // and lengths should be equal. So this code is unreachable.
            return Ok(false);
        }
        for ((k1, v1), (k2, v2)) in this.iter_items().zip(other.iter_items()) {
            if k1 != k2 {
                // If provider ids are equal, then providers point to the same provider callable,
                // and keys should be equal. So this code is unreachable.
                return Ok(false);
            }
            if !v1.equals(v2)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        self.callable_data().provider_id.hash(hasher);
        for (k, v) in self.iter_items() {
            k.hash(hasher);
            v.write_hash(hasher)?;
        }
        Ok(())
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn ProviderLike>(self);
    }
}

impl<'v, V: ValueLike<'v>> serde::Serialize for UserProviderGen<'v, V> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_map(self.iter_items())
    }
}

impl<'v, V: ValueLike<'v>> ProviderLike<'v> for UserProviderGen<'v, V> {
    fn id(&self) -> &Arc<ProviderId> {
        &self.callable_data().provider_id
    }

    fn items(&self) -> Vec<(&str, Value<'v>)> {
        self.iter_items().map(|(k, v)| (k, v.to_value())).collect()
    }
}

/// Creates instances of mutable `UserProvider`s; called from a `NativeFunction`
pub(crate) fn user_provider_creator<'v>(
    callable: FrozenAnyValue<UserProviderCallableData>,
    eval: &Evaluator<'v, '_, '_>,
    param_parser: &mut ParametersParser<'v, '_>,
) -> buck2_error::Result<Value<'v>> {
    let heap = eval.heap();
    let callable_data: &UserProviderCallableData = &callable;
    let values = callable_data
        .fields
        .iter()
        .map(|(name, field)| match param_parser.next_opt()? {
            Some(value) => {
                if !field.ty.matches(value) {
                    return Err(UserProviderError::MismatchedType(
                        name.to_owned(),
                        field.ty.as_ty().dupe(),
                        value.to_repr(),
                    )
                    .into());
                }
                Ok(value)
            }
            None => match field.default {
                Some(default) => Ok(default.to_value()),
                None => Err(UserProviderError::MissingParameter(name.to_owned()).into()),
            },
        })
        .collect::<buck2_error::Result<Box<[Value]>>>()?;
    Ok(heap.alloc(UserProvider {
        callable,
        attributes: values,
        _marker: PhantomData,
    }))
}
