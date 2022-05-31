/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::{Debug, Display};

use buck2_core::{provider::ConfiguredProvidersLabel, target::TargetLabel};
use serde::{Serialize, Serializer};
use starlark::{
    collections::{small_map::Entry, SmallMap},
    values::Value,
};

use crate::attrs::{
    attr_type::attr_literal::{AttrConfig, AttrLiteral, ConfiguredAttrTraversal},
    coerced_attr::SelectError,
    AttrResolutionContext,
};

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct ConfiguredAttr(pub(crate) AttrLiteral<ConfiguredAttr>);

impl Serialize for ConfiguredAttr {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // TODO this is inefficient. We should impl Serialize and derive value from this instead.
        self.to_json()
            .map_err(|e| serde::ser::Error::custom(format!("{}", e)))?
            .serialize(s)
    }
}

impl Display for ConfiguredAttr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Debug for ConfiguredAttr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl ConfiguredAttr {
    pub(crate) fn new(v: AttrLiteral<ConfiguredAttr>) -> Self {
        Self(v)
    }

    /// Used for concatting the configured result of concatted selects. For most types this isn't allowed (it
    /// should be unreachable as concat-ability is checked during coercion and the type would've returned false from `AttrType::supports_concat`).
    /// This is used when a select() is added to another value, like `select(<...>) + select(<...>)` or `select(<...>) + [...]`.
    pub fn concat(self, items: impl Iterator<Item = anyhow::Result<Self>>) -> anyhow::Result<Self> {
        let mismatch = |ty, attr: AttrLiteral<ConfiguredAttr>| {
            Err(SelectError::ConcatNotSupportedValues(ty, attr.to_string()).into())
        };

        match self.0 {
            AttrLiteral::List(res, ty) => {
                let mut res = res.into_vec();
                for x in items {
                    match x?.0 {
                        AttrLiteral::List(items, ty2) => {
                            if ty != ty2 {
                                return Err(SelectError::ConcatListDifferentTypes(
                                    ty.to_string(),
                                    ty2.to_string(),
                                )
                                .into());
                            } else {
                                res.extend(items.into_vec());
                            }
                        }
                        attr => return mismatch("list", attr),
                    }
                }
                Ok(Self(AttrLiteral::List(res.into_boxed_slice(), ty)))
            }
            AttrLiteral::Dict(left) => {
                let mut res = SmallMap::new();
                for (k, v) in left {
                    res.insert(k, v);
                }
                for x in items {
                    match x?.0 {
                        AttrLiteral::Dict(right) => {
                            for (k, v) in right {
                                match res.entry(k) {
                                    Entry::Vacant(e) => {
                                        e.insert(v);
                                    }
                                    Entry::Occupied(e) => {
                                        return Err(SelectError::DictConcatDuplicateKeys(
                                            e.key().to_string(),
                                        )
                                        .into());
                                    }
                                }
                            }
                        }
                        attr => return mismatch("dict", attr),
                    }
                }
                Ok(Self(AttrLiteral::Dict(res.into_iter().collect())))
            }
            AttrLiteral::String(mut res) => {
                for x in items {
                    match x?.0 {
                        AttrLiteral::String(right) => res.push_str(&right),
                        attr => return mismatch("string", attr),
                    }
                }
                Ok(Self(AttrLiteral::String(res)))
            }
            AttrLiteral::Arg(left) => {
                let res = left.concat(items.map(|x| match x?.0 {
                    AttrLiteral::Arg(x) => Ok(x),
                    attr => {
                        Err(SelectError::ConcatNotSupportedValues("arg", attr.to_string()).into())
                    }
                }))?;
                Ok(Self(AttrLiteral::Arg(res)))
            }
            val => Err(SelectError::ConcatNotSupported(val.to_string()).into()),
        }
    }

    /// Traverses the configured attribute and calls the traverse for every encountered target label (in deps, sources, or other places).
    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        self.0.traverse(traversal)
    }

    /// "Resolves" the configured value to the resolved value provided to the rule implementation.
    ///
    /// `resolve` may return multiple values. It is up to the caller to fail if
    /// an inappropriate number of elements is returned. e.g. `attr.list()` might
    /// accept and merge multiple returned values from `attr.source()`, but
    /// `attr.optional()` might only accept a single value, and fail otherwise.
    pub(crate) fn resolve<'v>(
        &self,
        ctx: &'v dyn AttrResolutionContext,
    ) -> anyhow::Result<Vec<Value<'v>>> {
        self.0.resolve(ctx)
    }

    /// Resolving a single value is common, so `resolve_single` will validate
    /// this function's output, and return a single value or an error.
    pub(crate) fn resolve_single<'v>(
        &self,
        ctx: &'v dyn AttrResolutionContext,
    ) -> anyhow::Result<Value<'v>> {
        self.0.resolve_single(ctx)
    }

    pub fn unpack_dep(&self) -> Option<&ConfiguredProvidersLabel> {
        match &self.0 {
            AttrLiteral::Dep(d) => Some(d.label()),
            _ => None,
        }
    }

    pub fn try_into_dep(self) -> Option<ConfiguredProvidersLabel> {
        match self.0 {
            AttrLiteral::Dep(d) => Some(d.into_label()),
            _ => None,
        }
    }

    pub fn try_into_configuration_dep(self) -> Option<TargetLabel> {
        match self.0 {
            AttrLiteral::ConfigurationDep(d) => Some(d),
            _ => None,
        }
    }

    pub fn unpack_list(&self) -> Option<&[ConfiguredAttr]> {
        match &self.0 {
            AttrLiteral::List(v, _) => Some(v),
            _ => None,
        }
    }

    pub fn try_into_list(self) -> Option<Vec<ConfiguredAttr>> {
        match self.0 {
            AttrLiteral::List(v, _) => Some(v.into_vec()),
            _ => None,
        }
    }

    pub fn unpack_label(&self) -> Option<&ConfiguredProvidersLabel> {
        match &self.0 {
            AttrLiteral::Label(label) => Some(label),
            _ => None,
        }
    }

    pub fn try_into_label(self) -> Option<ConfiguredProvidersLabel> {
        match self.0 {
            AttrLiteral::Label(label) => Some(*label),
            _ => None,
        }
    }
}
