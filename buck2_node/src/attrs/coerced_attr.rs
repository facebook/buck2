/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::hash::Hash;

use allocative::Allocative;
use buck2_core::collections::ordered_map::OrderedMap;
use buck2_core::configuration::Configuration;
use buck2_core::configuration::ConfigurationData;
use buck2_core::target::TargetLabel;
use gazebo::dupe::Dupe;
use gazebo::prelude::IterDuped;
use gazebo::prelude::SliceExt;
use itertools::Itertools;
use serde::Serialize;
use serde::Serializer;

use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::traversal::CoercedAttrTraversal;

#[derive(thiserror::Error, Debug)]
enum SelectError {
    #[error("None of {} conditions matched configuration `{}` and no default was set:\n{}",
        .1.len(),
        .0,
        .1.iter().map(| s | format ! ("  {}", s)).join("\n"),
    )]
    MissingDefault(Configuration, Vec<TargetLabel>),
    #[error(
        "Both select keys `{0}` and `{1}` match the configuration, but neither is more specific"
    )]
    TwoKeysDoNotRefineEachOther(String, String),
    #[error("concat with no items (internal error)")]
    ConcatEmpty,
}

/// CoercedAttr is the "coerced" representation of an attribute. It has been type-checked and converted to
/// specific types (for example, where we expect target-like things, it has been converted to something like
/// a TargetLable or ProvidersLabel).
///
/// CoercedAttr  provides support for the `select()` function. All coerced data is
/// potentially represented by a select that represents possibly different
/// configured representations.
///
/// CoercedData::Concat supports a representation for when a selectable is added
/// to something. Not all types support this case and those will return an error
/// during coercion and not ever use the ::Concat case.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Allocative)]
pub enum CoercedAttr {
    Literal(AttrLiteral<Self>),
    Selector(Box<(OrderedMap<TargetLabel, Self>, Option<Self>)>),
    Concat(Vec<Self>),
}

// This is just to help understand any impact that changes have to the size of this.
// We store a lot of these, so we try to keep it to a reasonable size.
static_assertions::assert_eq_size!(CoercedAttr, [usize; 4]);

/// Provides roughly the stringified version of the starlark code that would produce this attr. For example, a dictionary
/// of string keys and values may result in `{"key1":"value1","key2":"value2"}` (note that strings will explicitly include
/// the wrapping `"`).
impl Display for CoercedAttr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoercedAttr::Literal(v) => Display::fmt(v, f),
            CoercedAttr::Selector(box (items, default)) => {
                write!(f, "select(")?;
                for (i, (condition, value)) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "\"{}\"={}", condition, value)?;
                }
                if let Some(default) = default {
                    if !items.is_empty() {
                        write!(f, ",")?;
                    }
                    write!(f, "\"DEFAULT\"={}", default)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            CoercedAttr::Concat(items) => write!(f, "{}", items.iter().format("+")),
        }
    }
}

impl Serialize for CoercedAttr {
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

impl CoercedAttr {
    pub fn new_literal(value: AttrLiteral<CoercedAttr>) -> Self {
        Self::Literal(value)
    }

    /// Converts the coerced attr to a serde_json Value. This is generally just used for debugging or introspective
    /// things, a lot of the types will be dropped without special handling. For example, an artifact will just end
    /// up as the stringified version of its coerced value (i.e. while `//a:b` might represent some list of targets,
    /// in to_json it just appears as the string "//a:b").
    pub fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        match self {
            CoercedAttr::Literal(v) => v.to_json(),
            CoercedAttr::Selector(box (selector, default)) => {
                let mut map = serde_json::Map::new();
                for (k, v) in selector.iter() {
                    map.insert(k.to_string(), v.to_json()?);
                }
                if let Some(default) = default {
                    map.insert("DEFAULT".to_owned(), default.to_json()?);
                }
                let select = serde_json::Value::Object(map);

                Ok(serde_json::Value::Object(serde_json::Map::from_iter([
                    (
                        "__type".to_owned(),
                        serde_json::Value::String("selector".to_owned()),
                    ),
                    ("entries".to_owned(), select),
                ])))
            }
            CoercedAttr::Concat(items) => {
                Ok(serde_json::Value::Object(serde_json::Map::from_iter([
                    (
                        "__type".to_owned(),
                        serde_json::Value::String("concat".to_owned()),
                    ),
                    (
                        "items".to_owned(),
                        serde_json::Value::Array(items.try_map(CoercedAttr::to_json)?),
                    ),
                ])))
            }
        }
    }

    /// Returns `true` if the result of evaluating this literal can ever be None.
    pub fn may_return_none(&self) -> bool {
        match self {
            Self::Literal(AttrLiteral::None) => true,
            Self::Literal(_) => false,
            Self::Selector(box (alts, def)) => {
                def.as_ref().map_or(true, |x| x.may_return_none())
                    || alts.values().any(|x| x.may_return_none())
            }
            Self::Concat(_) => false,
        }
    }

    /// Traverses the coerced attribute and provides the traverser callbacks for all deps (those in select conditions
    /// are passed as configuration deps).
    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match self {
            CoercedAttr::Literal(v) => v.traverse(traversal),
            CoercedAttr::Selector(box (selector, default)) => {
                for (condition, value) in selector.iter() {
                    traversal.configuration_dep(condition)?;
                    value.traverse(traversal)?;
                }
                if let Some(v) = default {
                    v.traverse(traversal)?;
                }
                Ok(())
            }
            CoercedAttr::Concat(items) => {
                for item in items {
                    item.traverse(traversal)?;
                }
                Ok(())
            }
        }
    }

    /// If more than one select key matches, select the most specific.
    pub fn select_the_most_specific<'a>(
        ctx: &dyn AttrConfigurationContext,
        select_entries: &'a OrderedMap<TargetLabel, CoercedAttr>,
    ) -> anyhow::Result<Option<&'a CoercedAttr>> {
        let mut matching: Option<(&TargetLabel, &ConfigurationData, &CoercedAttr)> = None;
        for (k, v) in select_entries {
            matching = match (ctx.matches(k), matching) {
                (None, matching) => matching,
                (Some(conf), None) => Some((k, conf, v)),
                (Some(conf), Some((prev_k, prev_conf, prev_v))) => {
                    if conf.refines(prev_conf) {
                        Some((k, conf, v))
                    } else if prev_conf.refines(conf) {
                        Some((prev_k, prev_conf, prev_v))
                    } else {
                        return Err(SelectError::TwoKeysDoNotRefineEachOther(
                            prev_k.to_string(),
                            k.to_string(),
                        )
                        .into());
                    }
                }
            }
        }
        Ok(matching.map(|(_k, _conf, v)| v))
    }

    /// Returns the "configured" representation of the attribute in the provided context.
    /// This handles the resolution of the select() conditions and delegates to
    /// the actual attr type for handling any appropriate configuration-time
    /// processing.
    pub fn configure(&self, ctx: &dyn AttrConfigurationContext) -> anyhow::Result<ConfiguredAttr> {
        match self {
            CoercedAttr::Literal(v) => v.configure(ctx),
            CoercedAttr::Selector(box (selector, default)) => {
                if let Some(v) = Self::select_the_most_specific(ctx, selector)? {
                    return v.configure(ctx);
                }
                default
                    .as_ref()
                    .ok_or_else(|| {
                        SelectError::MissingDefault(
                            ctx.cfg().dupe(),
                            selector.keys().duped().collect(),
                        )
                    })?
                    .configure(ctx)
            }
            CoercedAttr::Concat(items) => {
                let singleton = items.len() == 1;
                let mut it = items.iter().map(|item| item.configure(ctx));
                let first = it.next().ok_or(SelectError::ConcatEmpty)??;
                if singleton {
                    Ok(first)
                } else {
                    first.concat(it)
                }
            }
        }
    }

    /// Checks if this attr matches the filter. For selectors and container-like things, will return true if any
    /// contained item matches the filter.
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        match self {
            CoercedAttr::Literal(v) => v.any_matches(filter),
            CoercedAttr::Selector(box (selector, default)) => {
                for value in selector.values() {
                    if value.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                if let Some(v) = default {
                    v.any_matches(filter)
                } else {
                    Ok(false)
                }
            }
            CoercedAttr::Concat(items) => {
                for item in items {
                    if item.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
        }
    }
}
