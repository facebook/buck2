/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use buck2_core::collections::ordered_map::OrderedMap;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::TargetLabel;
use serde::Serialize;
use serde::Serializer;
use starlark_map::small_map;

use crate::attrs::attr_type::attr_config::AttrConfig;
use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;

#[derive(Debug, thiserror::Error)]
enum ConfiguredAttrError {
    #[error("addition not supported for this attribute type `{0}`.")]
    ConcatNotSupported(String),
    #[error("addition not supported for these attribute type `{0}` and value `{1}`.")]
    ConcatNotSupportedValues(&'static str, String),
    #[error("got same key in both sides of dictionary concat (key `{0}`).")]
    DictConcatDuplicateKeys(String),
    #[error("addition not supported for lists of different types, got `{0}` and `{1}`.")]
    ConcatListDifferentTypes(String, String),
}

#[derive(Eq, PartialEq, Hash, Clone, Allocative)]
pub struct ConfiguredAttr(pub AttrLiteral<ConfiguredAttr>);

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
    pub fn new(v: AttrLiteral<ConfiguredAttr>) -> Self {
        Self(v)
    }

    /// Traverses the configured attribute and calls the traverse for every encountered target label (in deps, sources, or other places).
    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn ConfiguredAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        self.0.traverse(traversal)
    }

    /// Used for concatting the configured result of concatted selects. For most types this isn't allowed (it
    /// should be unreachable as concat-ability is checked during coercion and the type would've returned false from `AttrType::supports_concat`).
    /// This is used when a select() is added to another value, like `select(<...>) + select(<...>)` or `select(<...>) + [...]`.
    pub fn concat(self, items: impl Iterator<Item = anyhow::Result<Self>>) -> anyhow::Result<Self> {
        let mismatch = |ty, attr: AttrLiteral<ConfiguredAttr>| {
            Err(ConfiguredAttrError::ConcatNotSupportedValues(ty, attr.to_string()).into())
        };

        match self.0 {
            AttrLiteral::List(res, ty) => {
                let mut res = res.into_vec();
                for x in items {
                    match x?.0 {
                        AttrLiteral::List(items, ty2) => {
                            if ty != ty2 {
                                return Err(ConfiguredAttrError::ConcatListDifferentTypes(
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
                let mut res = OrderedMap::new();
                for (k, v) in left {
                    res.insert(k, v);
                }
                for x in items {
                    match x?.0 {
                        AttrLiteral::Dict(right) => {
                            for (k, v) in right {
                                match res.entry(k) {
                                    small_map::Entry::Vacant(e) => {
                                        e.insert(v);
                                    }
                                    small_map::Entry::Occupied(e) => {
                                        return Err(ConfiguredAttrError::DictConcatDuplicateKeys(
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
                let res = left.concat(items.map(|x| {
                    match x?.0 {
                        AttrLiteral::Arg(x) => Ok(x),
                        attr => Err(ConfiguredAttrError::ConcatNotSupportedValues(
                            "arg",
                            attr.to_string(),
                        )
                        .into()),
                    }
                }))?;
                Ok(Self(AttrLiteral::Arg(res)))
            }
            val => Err(ConfiguredAttrError::ConcatNotSupported(val.to_string()).into()),
        }
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
