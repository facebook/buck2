/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::{HashMap, HashSet};

use anyhow::Context;
use buck2_core::target::TargetName;
use buck2_interpreter::extra::ExtraContext;
use gazebo::prelude::*;
use starlark::{
    eval::{Evaluator, ParametersParser, ParametersSpec},
    values::{docs::DocString, Value},
};
use thiserror::Error;

use crate::{
    attrs::{coerced_attr::CoercedAttr, OrderedMap, OrderedMapEntry},
    interpreter::{
        module_internals::ModuleInternals,
        rule_defs::attr::{Attribute, CoercedValue},
    },
    nodes::{
        attr_internal::{attr_is_configurable, internal_attrs, NAME_ATTRIBUTE_FIELD},
        attr_values::AttrValues,
        AttributeError, AttributeId,
    },
};

/// AttributeSpec holds the specification for a rules attributes as defined in the rule() call. This
/// is split into a mapping of "attribute name" -> "attribute id". The Attributes are stored in a vec
/// that can then be indexed using the name->id mapping (the id is really just an index into this vec).
///
/// For its attribute values, a TargetNode will hold a sorted Vec<(AttributeId, CoercedAttr)> that will have values
/// only for the values that are explicitly set. Default values need to be looked up through the AttributeSpec.
#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) struct AttributeSpec {
    pub(crate) indices: OrderedMap<String, AttributeId>,
    pub(crate) attributes: Vec<Attribute>,
}

#[derive(Debug, Error)]
pub(crate) enum AttributeSpecError {
    #[error("User provided attribute `{0}` overrides internal attribute")]
    InternalAttributeRedefined(String),
    #[error("Duplicate attribute `{0}`")]
    DuplicateAttribute(String),
    #[error("Rule definition has no attribute `{0}`")]
    UnknownAttribute(String),
}

impl AttributeSpec {
    pub(crate) fn from(attributes: Vec<(String, Attribute)>) -> anyhow::Result<Self> {
        let internal_attrs = internal_attrs();

        let mut indices = OrderedMap::with_capacity(attributes.len() + internal_attrs.len());
        let mut instances = Vec::with_capacity(attributes.len());
        let mut internal_attr_names = HashSet::new();
        for (name, instance) in internal_attrs {
            let index_in_attribute_spec = indices.len();
            internal_attr_names.insert(name);
            if indices
                .insert(
                    name.to_owned(),
                    AttributeId {
                        index_in_attribute_spec,
                    },
                )
                .is_some()
            {
                unreachable!("duplicate internal attr: '{}'", name);
            }
            instances.push(instance);
        }

        for (name, instance) in attributes.into_iter() {
            let index_in_attribute_spec = indices.len();
            match indices.entry(name) {
                OrderedMapEntry::Vacant(e) => {
                    e.insert(AttributeId {
                        index_in_attribute_spec,
                    });
                }
                OrderedMapEntry::Occupied(e) => {
                    let name = e.key();
                    if internal_attr_names.contains(name.as_str()) {
                        return Err(anyhow::anyhow!(
                            AttributeSpecError::InternalAttributeRedefined(name.to_owned())
                        ));
                    } else {
                        return Err(anyhow::anyhow!(AttributeSpecError::DuplicateAttribute(
                            name.to_owned()
                        )));
                    }
                }
            }
            instances.push(instance);
        }

        Ok(Self {
            indices,
            attributes: instances,
        })
    }

    pub(crate) fn get_attribute(&self, id: AttributeId) -> &Attribute {
        &self.attributes[id.index_in_attribute_spec]
    }

    /// Parses params extracting the TargetName and the attribute values to store in the TargetNode.
    pub(crate) fn parse_params<'v>(
        &self,
        mut param_parser: ParametersParser<'v, '_>,
        param_count: usize,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<(TargetName, AttrValues)> {
        let mut name = None;
        let mut attr_values = AttrValues::with_capacity(param_count);

        for (attr_name, attr_idx) in &self.indices {
            let configurable = attr_is_configurable(attr_name);

            let attr_idx = *attr_idx;
            let attribute = self.get_attribute(attr_idx);
            let user_value = match attribute.default {
                Some(_) => param_parser.next_opt(attr_name)?,
                None => {
                    let required_value: Value = param_parser.next(attr_name)?;
                    if attr_name == NAME_ATTRIBUTE_FIELD {
                        match required_value.unpack_str() {
                            Some(value) => name = Some(TargetName::new(value)?),
                            None => {
                                return Err(AttributeError::TargetNameNotString(
                                    required_value.to_repr(),
                                )
                                .into());
                            }
                        }
                    }
                    Some(required_value)
                }
            };

            if let Some(v) = user_value {
                let internals = ModuleInternals::from_context(eval)?;

                // TODO(cjhopman): We should get name out first, then coerce the rest.
                let coerced = attribute
                    .coerce(
                        attr_name,
                        configurable,
                        internals.attr_coercion_context(),
                        Some(v),
                    )
                    .with_context(|| {
                        if let Some(name) = &name {
                            format!(
                                "when coercing attributes of {}:{}",
                                internals.package(),
                                name
                            )
                        } else {
                            format!(
                                "when coercing attributes of <unknown target> in package `{}`",
                                internals.package()
                            )
                        }
                    })?;
                match coerced {
                    CoercedValue::Custom(v) => {
                        attr_values.push_sorted(attr_idx, v);
                    }
                    CoercedValue::Default => {}
                }
            }
        }

        let target_name = name.expect("name to have been one of the parameters");
        attr_values.shrink_to_fit();
        Ok((target_name, attr_values))
    }

    /// Returns an iterator over all of the attribute (name, value) pairs.
    pub(crate) fn attrs<'v>(
        &'v self,
        attr_values: &'v AttrValues,
    ) -> impl Iterator<Item = (&'v str, &'v CoercedAttr)> {
        let mut pos = 0;
        let mut entry: Option<&(AttributeId, CoercedAttr)> = attr_values.get_by_index(0);

        self.indices.iter().map(move |(name, idx)| match &entry {
            Some((entry_idx, entry_attr)) if entry_idx == idx => {
                pos += 1;
                entry = attr_values.get_by_index(pos);
                (name.as_str(), entry_attr)
            }
            _ => {
                let default: &CoercedAttr = self.get_attribute(*idx).default.as_ref().unwrap();
                (name.as_str(), default)
            }
        })
    }

    pub(crate) fn attr_or_none<'v>(
        &'v self,
        attr_values: &'v AttrValues,
        key: &str,
    ) -> Option<&'v CoercedAttr> {
        if let Some(idx) = self.indices.get(key) {
            if let Some(attr) = attr_values.get(*idx) {
                return Some(attr);
            }

            self.get_attribute(*idx).default.as_deref()
        } else {
            None
        }
    }

    pub(crate) fn attr<'v>(
        &'v self,
        attr_values: &'v AttrValues,
        key: &str,
    ) -> anyhow::Result<Option<&'v CoercedAttr>> {
        if let Some(idx) = self.indices.get(key) {
            if let Some(attr) = attr_values.get(*idx) {
                return Ok(Some(attr));
            }

            Ok(self.get_attribute(*idx).default.as_deref())
        } else {
            Err(AttributeSpecError::UnknownAttribute(key.to_owned()).into())
        }
    }

    /// Returns a starlark Parameters for the rule callable.
    pub(crate) fn signature(&self, rule_name: String) -> ParametersSpec<Value<'_>> {
        let mut signature = ParametersSpec::with_capacity(rule_name, self.indices.len());
        signature.no_more_positional_args();
        for (name, idx) in &self.indices {
            let attribute = self.get_attribute(*idx);
            match attribute.default {
                Some(_) => signature.optional(name),
                None => signature.required(name),
            };
        }
        signature.finish()
    }

    pub(crate) fn starlark_types(&self) -> Vec<String> {
        self.attributes.map(|a| a.starlark_type())
    }

    pub(crate) fn docstrings(&self) -> HashMap<String, Option<DocString>> {
        self.indices
            .iter()
            .map(|(name, idx)| (name.clone(), self.get_attribute(*idx).docstring()))
            .collect()
    }
}
