/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::coerce::coerce;
use starlark::coerce::Coerce;
use starlark::collections::Hashed;
use starlark::collections::StarlarkHasher;
use starlark::environment::Methods;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::eval::ParametersParser;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::FrozenRef;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::provider::callable::UserProviderCallableData;
use crate::interpreter::rule_defs::provider::provider_methods;
use crate::interpreter::rule_defs::provider::ProviderLike;

/// The result of calling the output of `provider()`. This is just a simple data structure of
/// either immediately available values or, later, `FutureValue` types that are resolved
/// asynchronously

#[derive(Debug, Clone, Coerce, Trace, Freeze, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct UserProviderGen<'v, V: ValueLike<'v>> {
    callable: FrozenRef<'static, UserProviderCallableData>,
    attributes: Vec<V>,
    _marker: PhantomData<&'v ()>,
}

starlark_complex_value!(pub UserProvider<'v>);

impl<'v, V: ValueLike<'v>> UserProviderGen<'v, V> {
    fn iter_items(&self) -> impl Iterator<Item = (&str, V)> {
        assert_eq!(self.callable.fields.len(), self.attributes.len());
        self.callable
            .fields
            .iter()
            .map(|s| s.as_str())
            .zip(self.attributes.iter().copied())
    }
}

impl<'v, V: ValueLike<'v>> Display for UserProviderGen<'v, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_keyed_container(
            f,
            &format!("{}(", self.callable.provider_id.name),
            ")",
            "=",
            self.iter_items(),
        )
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for UserProviderGen<'v, V>
where
    Self: ProvidesStaticType<'v>,
{
    starlark_type!("provider");

    fn matches_type(&self, ty: &str) -> bool {
        ty == "provider" || ty == self.callable.provider_id.name
    }

    fn dir_attr(&self) -> Vec<String> {
        self.callable.fields.iter().cloned().collect()
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        self.get_attr_hashed(Hashed::new(attribute), heap)
    }

    fn get_attr_hashed(&self, attribute: Hashed<&str>, _heap: &'v Heap) -> Option<Value<'v>> {
        let index = self.callable.fields.get_index_of_hashed(attribute)?;
        Some(self.attributes[index].to_value())
    }

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(provider_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        let this: &UserProvider = coerce(self);
        let other: &UserProvider = match UserProvider::from_value(other) {
            Some(other) => other,
            None => return Ok(false),
        };
        if this.callable.provider_id != other.callable.provider_id {
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

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.callable.provider_id.hash(hasher);
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
        &self.callable.provider_id
    }

    fn get_field(&self, name: &str) -> Option<Value<'v>> {
        let index = self.callable.fields.get_index_of(name)?;
        Some(self.attributes[index].to_value())
    }

    fn items(&self) -> Vec<(&str, Value<'v>)> {
        self.iter_items().map(|(k, v)| (k, v.to_value())).collect()
    }
}

/// Creates instances of mutable `UserProvider`s; called from a `NativeFunction`
pub(crate) fn user_provider_creator<'v>(
    callable: FrozenRef<'static, UserProviderCallableData>,
    eval: &Evaluator<'v, '_>,
    mut param_parser: ParametersParser<'v, '_>,
) -> anyhow::Result<Value<'v>> {
    let heap = eval.heap();
    let values = callable
        .fields
        .iter()
        .map(|field| param_parser.next(field))
        .collect::<anyhow::Result<Vec<Value>>>()?;
    Ok(heap.alloc(UserProvider {
        callable,
        attributes: values,
        _marker: PhantomData,
    }))
}
