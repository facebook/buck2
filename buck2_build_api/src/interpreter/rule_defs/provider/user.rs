/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;

use buck2_core::provider::id::ProviderId;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use gazebo::display::display_keyed_container;
use serde::Serializer;
use starlark::collections::Hashed;
use starlark::collections::SmallMap;
use starlark::collections::StarlarkHasher;
use starlark::environment::Methods;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::eval::ParametersParser;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::provider::provider_methods;
use crate::interpreter::rule_defs::provider::ProviderLike;
use crate::interpreter::rule_defs::provider::ValueAsProviderLike;

/// The result of calling the output of `provider()`. This is just a simple data structure of
/// either immediately available values or, later, `FutureValue` types that are resolved
/// asynchronously

#[derive(Debug, Clone, Coerce, Trace, Freeze, ProvidesStaticType)]
#[repr(C)]
pub struct UserProviderGen<V> {
    #[trace(unsafe_ignore)]
    #[freeze(identity)]
    id: Arc<ProviderId>,
    attributes: SmallMap<String, V>,
}

starlark_complex_value!(pub UserProvider);

impl<V: Display> Display for UserProviderGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_keyed_container(
            f,
            &format!("{}(", self.id.name),
            ")",
            "=",
            self.attributes.iter(),
        )
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for UserProviderGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("provider");

    fn matches_type(&self, ty: &str) -> bool {
        ty == "provider" || ty == self.id.name
    }

    fn dir_attr(&self) -> Vec<String> {
        self.attributes.keys().cloned().collect()
    }

    fn has_attr(&self, attribute: &str) -> bool {
        self.attributes.contains_key(attribute)
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        self.get_attr_hashed(Hashed::new(attribute), heap)
    }

    fn get_attr_hashed(&self, attribute: Hashed<&str>, _heap: &'v Heap) -> Option<Value<'v>> {
        Some(self.attributes.get_hashed(attribute)?.to_value())
    }

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(provider_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match other.as_provider() {
            None => Ok(false),
            Some(o) => {
                if self.id != *o.id() {
                    return Ok(false);
                }

                let items = o.items();
                if self.attributes.len() != items.len() {
                    return Ok(false);
                }
                for ((k1, v1), (k2, v2)) in self.attributes.iter().zip(items.iter()) {
                    if k1 != k2 {
                        return Ok(false);
                    }
                    if !v1.equals(*v2)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
        }
    }

    fn compare(&self, other: Value<'v>) -> anyhow::Result<Ordering> {
        match other.as_provider() {
            None => ValueError::unsupported_with(self, "compare", other),
            Some(o) => {
                match self.id.cmp(o.id()) {
                    Ordering::Equal => {}
                    v => return Ok(v),
                }
                let items = o.items();
                for ((k1, v1), (k2, v2)) in self.attributes.iter().zip(items.iter()) {
                    match k1.as_str().cmp(k2) {
                        Ordering::Equal => {}
                        v => return Ok(v),
                    }

                    match v1.compare(*v2)? {
                        Ordering::Equal => {}
                        v => return Ok(v),
                    }
                }
                match self.attributes.len().cmp(&items.len()) {
                    Ordering::Equal => {}
                    v => return Ok(v),
                }
                Ok(Ordering::Equal)
            }
        }
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.id.hash(hasher);
        for (k, v) in self.attributes.iter() {
            k.hash(hasher);
            v.write_hash(hasher)?;
        }
        Ok(())
    }
}

impl<'v, V: ValueLike<'v>> serde::Serialize for UserProviderGen<V> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_map(self.attributes.iter())
    }
}

impl<'v, V: ValueLike<'v>> ProviderLike<'v> for UserProviderGen<V>
where
    UserProviderGen<V>: Debug,
{
    fn id(&self) -> &Arc<ProviderId> {
        &self.id
    }

    fn get_field(&self, name: &str) -> Option<Value<'v>> {
        self.attributes.get(name).map(|v| v.to_value())
    }

    fn items(&self) -> Vec<(&str, Value<'v>)> {
        self.attributes
            .iter()
            .map(|(k, v)| (k.as_str(), v.to_value()))
            .collect()
    }
}

/// Creates instances of mutable `UserProvider`s; called from a `NativeFunction`
pub(crate) fn user_provider_creator<'v>(
    id: Arc<ProviderId>,
    fields: &[String],
    eval: &Evaluator<'v, '_>,
    mut param_parser: ParametersParser<'v, '_>,
) -> anyhow::Result<Value<'v>> {
    let heap = eval.heap();
    let values = fields
        .iter()
        .map(|field| {
            let user_value = param_parser.next(field)?;
            Ok((field.to_owned(), user_value))
        })
        .collect::<anyhow::Result<SmallMap<String, Value>>>()?;
    Ok(heap.alloc(UserProvider {
        id,
        attributes: values,
    }))
}
