/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::Context;
use buck2_core::target::TargetName;
use buck2_interpreter::extra::ExtraContext;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use gazebo::prelude::*;
use starlark::eval::ParametersParser;
use starlark::eval::ParametersSpec;
use starlark::values::docs::DocString;
use starlark::values::Value;
use thiserror::Error;

use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::OrderedMap;
use crate::attrs::OrderedMapEntry;
use crate::interpreter::module_internals::ModuleInternals;
use crate::interpreter::rule_defs::attr::Attribute;
use crate::interpreter::rule_defs::attr::CoercedValue;
use crate::nodes::attr_internal::attr_is_configurable;
use crate::nodes::attr_internal::internal_attrs;
use crate::nodes::attr_internal::NAME_ATTRIBUTE_FIELD;
use crate::nodes::attr_values::AttrValues;
use crate::nodes::unconfigured::AttrInspectOptions;
use crate::nodes::AttributeId;

/// AttributeSpec holds the specification for a rules attributes as defined in the rule() call. This
/// is split into a mapping of "attribute name" -> "attribute id". The Attributes are stored in a vec
/// that can then be indexed using the name->id mapping (the id is really just an index into this vec).
///
/// For its attribute values, a TargetNode will hold a sorted Vec<(AttributeId, CoercedAttr)> that will have values
/// only for the values that are explicitly set. Default values need to be looked up through the AttributeSpec.
#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) struct AttributeSpec {
    // TODO(nga): either "map" or "ordered" is redundant here:
    //   `AttributeId` in `indices` is always equal to the index of the entry in ordered map.
    indices: OrderedMap<String, AttributeId>,
    attributes: Vec<Attribute>,
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

    pub(crate) fn attr_specs(&self) -> impl Iterator<Item = (&str, AttributeId, &Attribute)> {
        self.indices.iter().map(|(name, id)| {
            (
                name.as_str(),
                *id,
                &self.attributes[id.index_in_attribute_spec],
            )
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
        internals: &ModuleInternals,
    ) -> anyhow::Result<(TargetName, AttrValues)> {
        let mut attr_values = AttrValues::with_capacity(param_count);

        let mut indices = self.attr_specs();
        let name = match indices.next() {
            Some((name_name, attr_idx, _attr))
                if name_name == NAME_ATTRIBUTE_FIELD && attr_idx.index_in_attribute_spec == 0 =>
            {
                let name: &str = param_parser.next(NAME_ATTRIBUTE_FIELD)?;

                attr_values.push_sorted(
                    attr_idx,
                    CoercedAttr::Literal(AttrLiteral::String(name.to_owned())),
                );

                TargetName::new(name)?
            }
            _ => panic!("First attribute is `name`, it is known"),
        };

        for (attr_name, attr_idx, attribute) in indices {
            let configurable = attr_is_configurable(attr_name);

            let user_value: Option<Value> = match attribute.default {
                Some(_) => param_parser.next_opt(attr_name)?,
                None => Some(param_parser.next(attr_name)?),
            };

            if let Some(v) = user_value {
                let coerced = attribute
                    .coerce(
                        attr_name,
                        configurable,
                        internals.attr_coercion_context(),
                        Some(v),
                    )
                    .with_context(|| {
                        format!(
                            "when coercing attributes of {}:{}",
                            internals.package(),
                            name,
                        )
                    })?;
                match coerced {
                    CoercedValue::Custom(v) => {
                        attr_values.push_sorted(attr_idx, v);
                    }
                    CoercedValue::Default => {}
                }
            }
        }

        attr_values.shrink_to_fit();
        Ok((name, attr_values))
    }

    /// Returns an iterator over all of the attribute (name, value) pairs.
    pub(crate) fn attrs<'v>(
        &'v self,
        attr_values: &'v AttrValues,
        opts: AttrInspectOptions,
    ) -> impl Iterator<Item = (&'v str, &'v CoercedAttr)> {
        let mut pos = 0;
        let mut entry: Option<&(AttributeId, CoercedAttr)> = attr_values.get_by_index(0);

        self.attr_specs()
            .filter_map(move |(name, idx, attr)| match &entry {
                Some((entry_idx, entry_attr)) if *entry_idx == idx => {
                    pos += 1;
                    entry = attr_values.get_by_index(pos);
                    if opts.include_defined() {
                        Some((name, entry_attr))
                    } else {
                        None
                    }
                }
                _ => {
                    let default: &CoercedAttr = attr.default.as_ref().unwrap();
                    if opts.include_default() {
                        Some((name, default))
                    } else {
                        None
                    }
                }
            })
    }

    fn known_attr_or_none<'v>(
        &'v self,
        idx: AttributeId,
        attr_values: &'v AttrValues,
        opts: AttrInspectOptions,
    ) -> Option<&'v CoercedAttr> {
        if let Some(attr) = attr_values.get(idx) {
            if opts.include_defined() {
                return Some(attr);
            } else {
                return None;
            }
        }

        if opts.include_default() {
            return self.get_attribute(idx).default.as_deref();
        }
        None
    }

    pub(crate) fn attr_or_none<'v>(
        &'v self,
        attr_values: &'v AttrValues,
        key: &str,
        opts: AttrInspectOptions,
    ) -> Option<&'v CoercedAttr> {
        if let Some(idx) = self.indices.get(key) {
            self.known_attr_or_none(*idx, attr_values, opts)
        } else {
            None
        }
    }

    pub(crate) fn attr<'v>(
        &'v self,
        attr_values: &'v AttrValues,
        key: &str,
        opts: AttrInspectOptions,
    ) -> anyhow::Result<Option<&'v CoercedAttr>> {
        if let Some(idx) = self.indices.get(key) {
            Ok(self.known_attr_or_none(*idx, attr_values, opts))
        } else {
            Err(AttributeSpecError::UnknownAttribute(key.to_owned()).into())
        }
    }

    /// Returns a starlark Parameters for the rule callable.
    pub(crate) fn signature(&self, rule_name: String) -> ParametersSpec<Value<'_>> {
        let mut signature = ParametersSpec::with_capacity(rule_name, self.indices.len());
        signature.no_more_positional_args();
        for (name, _idx, attribute) in self.attr_specs() {
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
        self.attr_specs()
            .map(|(name, _idx, attr)| (name.to_owned(), attr.docstring()))
            .collect()
    }
}

pub(crate) mod testing {
    use crate::attrs::OrderedMap;
    use crate::interpreter::rule_defs::attr::Attribute;
    use crate::nodes::attr_spec::AttributeSpec;
    use crate::nodes::AttributeId;

    impl AttributeSpec {
        pub(crate) fn testing_new(
            indices: OrderedMap<String, AttributeId>,
            attributes: Vec<Attribute>,
        ) -> AttributeSpec {
            AttributeSpec {
                indices,
                attributes,
            }
        }
    }
}
