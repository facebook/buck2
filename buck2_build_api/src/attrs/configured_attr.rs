/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use buck2_core::{provider::ConfiguredProvidersLabel, target::TargetLabel};
use gazebo::prelude::*;
use serde::{Serialize, Serializer};
use starlark::values::Value;

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
    pub fn concat(self, other: ConfiguredAttr) -> anyhow::Result<Self> {
        match (self.0, other.0) {
            (AttrLiteral::List(left, lt), AttrLiteral::List(right, rt)) => {
                if lt != rt {
                    Err(SelectError::ConcatNotSupportedValues(
                        AttrLiteral::List(left, lt).to_string(),
                        AttrLiteral::List(right, rt).to_string(),
                    )
                    .into())
                } else {
                    let mut res = left.into_vec();
                    res.extend(right.into_vec());
                    Ok(Self(AttrLiteral::List(res.into_boxed_slice(), lt)))
                }
            }
            (AttrLiteral::Dict(mut left), AttrLiteral::Dict(right)) => {
                let mut index = HashMap::new();
                for (i, (k, _)) in left.iter().enumerate() {
                    index.insert(k, i);
                }
                // As the index borrows left, we need to resolve the mapping before we start modifying it.
                let mapping: Vec<_> = right.map(|(k, _)| index.get(k).copied());
                for (idx, (k, v)) in itertools::zip(mapping, right) {
                    if idx.is_some() {
                        return Err(SelectError::DictConcatDuplicateKeys(k.to_string()).into());
                    } else {
                        left.push((k, v));
                    }
                }
                Ok(Self(AttrLiteral::Dict(left)))
            }
            (AttrLiteral::String(mut left), AttrLiteral::String(right)) => {
                left.push_str(&right);
                Ok(Self(AttrLiteral::String(left)))
            }
            (AttrLiteral::Arg(left), AttrLiteral::Arg(right)) => {
                Ok(Self(AttrLiteral::Arg(left.concat(right))))
            }
            (lhs, rhs) => {
                Err(SelectError::ConcatNotSupportedValues(lhs.to_string(), rhs.to_string()).into())
            }
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
