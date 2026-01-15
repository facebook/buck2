/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use pagable::Pagable;
use serde::Deserialize;
use serde::Serialize;
use starlark_map::ordered_map::OrderedMap;
use starlark_map::small_map;

use super::anon_target_attr_validation::AnonRuleAttrValidation;
use crate::attrs::attr::Attribute;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::coerced_attr_full::CoercedAttrFull;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::attrs::spec::internal::INCOMING_TRANSITION_ATTRIBUTE;
use crate::attrs::spec::internal::common_internal_attrs;
use crate::attrs::spec::internal::is_internal_attr;
use crate::attrs::values::AttrValues;
use crate::rule::RuleIncomingTransition;

pub mod internal;

/// Identifies an attribute within a rule.
#[derive(
    Debug,
    Clone,
    dupe::Dupe,
    Copy,
    Eq,
    Hash,
    PartialEq,
    Ord,
    PartialOrd,
    Allocative,
    Serialize,
    Deserialize,
    Pagable
)]
pub struct AttributeId(u16); // Index in the attribute spec

impl AttributeId {
    const MAX_INDEX: u16 = u16::MAX;
}

/// AttributeSpec holds the specification for a rules attributes as defined in the rule() call. This
/// is split into a mapping of "attribute name" -> "attribute id". The Attributes are stored in a vec
/// that can then be indexed using the name->id mapping (the id is really just an index into this vec).
///
/// For its attribute values, a TargetNode will hold a sorted Vec<(AttributeId, CoercedAttr)> that will have values
/// only for the values that are explicitly set. Default values need to be looked up through the AttributeSpec.
#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative)]
pub struct AttributeSpec {
    attributes: OrderedMap<Box<str>, Attribute>,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub(crate) enum AttributeSpecError {
    #[error("User provided attribute `{0}` overrides internal attribute")]
    InternalAttributeRedefined(String),
    #[error("Duplicate attribute `{0}`")]
    DuplicateAttribute(String),
    #[error("Rule definition has no attribute `{0}`")]
    UnknownAttribute(String),
    #[error("Too many attributes: {0} > {}", AttributeId::MAX_INDEX)]
    TooManyAttributes(usize),
}

impl AttributeSpec {
    fn new(attributes: OrderedMap<Box<str>, Attribute>) -> buck2_error::Result<AttributeSpec> {
        if attributes.len() > AttributeId::MAX_INDEX as usize {
            return Err(AttributeSpecError::TooManyAttributes(attributes.len()).into());
        }
        Ok(AttributeSpec { attributes })
    }

    pub fn from(
        attributes: Vec<(String, Attribute)>,
        is_anon: bool,
        cfg: &RuleIncomingTransition,
    ) -> buck2_error::Result<Self> {
        let internal_attrs = common_internal_attrs();

        let mut instances: OrderedMap<Box<str>, Attribute> =
            OrderedMap::with_capacity(attributes.len());
        for (name, instance) in internal_attrs {
            let prev = instances.insert((*name).into(), instance.clone());
            if prev.is_some() {
                unreachable!("duplicate internal attr: '{}'", name);
            }
        }

        if cfg == &RuleIncomingTransition::FromAttribute {
            instances.insert(
                INCOMING_TRANSITION_ATTRIBUTE.name.into(),
                (INCOMING_TRANSITION_ATTRIBUTE.attr)(),
            );
        }

        for (name, instance) in attributes.into_iter() {
            if is_anon {
                instance.coercer().validate_for_anon_rule()?;
            }

            if is_internal_attr(&name) {
                return Err(AttributeSpecError::InternalAttributeRedefined(name.to_owned()).into());
            }

            match instances.entry(name.into_boxed_str()) {
                small_map::Entry::Vacant(e) => {
                    e.insert(instance);
                }
                small_map::Entry::Occupied(e) => {
                    let name: &str = e.key();
                    return Err(AttributeSpecError::DuplicateAttribute(name.to_owned()).into());
                }
            }
        }

        AttributeSpec::new(instances)
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.attributes.len()
    }

    pub fn attr_specs(&self) -> impl ExactSizeIterator<Item = (&str, AttributeId, &Attribute)> {
        self.attributes
            .iter()
            .enumerate()
            .map(|(index_in_attribute_spec, (name, attribute))| {
                (
                    &**name,
                    AttributeId(index_in_attribute_spec as u16),
                    attribute,
                )
            })
    }

    fn attribute_by_id(&self, id: AttributeId) -> &Attribute {
        self.attributes.get_index(id.0 as usize).unwrap().1
    }

    fn attribute_name_by_id(&self, id: AttributeId) -> &str {
        self.attributes.get_index(id.0 as usize).unwrap().0
    }

    pub(crate) fn attribute_id_by_name(&self, name: &str) -> Option<AttributeId> {
        self.attributes
            .get_index_of(name)
            .map(|index_in_attribute_spec| AttributeId(index_in_attribute_spec as u16))
    }

    pub fn attribute(&self, name: &str) -> Option<&Attribute> {
        self.attributes.get(name)
    }

    /// Returns an iterator over all of the attribute (name, value) pairs.
    pub fn attrs<'v>(
        &'v self,
        attr_values: &'v AttrValues,
        opts: AttrInspectOptions,
    ) -> impl Iterator<Item = CoercedAttrFull<'v>> {
        let mut pos = 0;
        let mut entry: Option<(AttributeId, &CoercedAttr)> = attr_values.get_by_index(0);

        self.attr_specs()
            .filter_map(move |(name, idx, attr)| match entry {
                Some((entry_idx, entry_attr)) if entry_idx == idx => {
                    pos += 1;
                    entry = attr_values.get_by_index(pos);
                    Some(CoercedAttrFull {
                        name,
                        attr,
                        value: entry_attr,
                    })
                }
                _ => {
                    let default: &CoercedAttr = match attr.default() {
                        Some(default) => default,
                        None => panic!("no default for attribute `{name}`"),
                    };
                    if opts.include_default() {
                        Some(CoercedAttrFull {
                            name,
                            attr,
                            value: default,
                        })
                    } else {
                        None
                    }
                }
            })
    }

    pub fn known_attr_or_none<'v>(
        &'v self,
        idx: AttributeId,
        attr_values: &'v AttrValues,
        opts: AttrInspectOptions,
    ) -> Option<CoercedAttrFull<'v>> {
        let value = if let Some(attr) = attr_values.get(idx) {
            attr
        } else if opts.include_default() {
            self.attribute_by_id(idx).default()?
        } else {
            return None;
        };

        Some(CoercedAttrFull {
            name: self.attribute_name_by_id(idx),
            attr: self.attribute_by_id(idx),
            value,
        })
    }

    pub fn attr_or_none<'v>(
        &'v self,
        attr_values: &'v AttrValues,
        name: &str,
        opts: AttrInspectOptions,
    ) -> Option<CoercedAttrFull<'v>> {
        if let Some(idx) = self.attribute_id_by_name(name) {
            self.known_attr_or_none(idx, attr_values, opts)
        } else {
            None
        }
    }

    pub fn attr<'v>(
        &'v self,
        attr_values: &'v AttrValues,
        key: &str,
        opts: AttrInspectOptions,
    ) -> buck2_error::Result<Option<CoercedAttrFull<'v>>> {
        if let Some(idx) = self.attribute_id_by_name(key) {
            Ok(self.known_attr_or_none(idx, attr_values, opts))
        } else {
            Err(AttributeSpecError::UnknownAttribute(key.to_owned()).into())
        }
    }
}

pub(crate) mod testing {

    use starlark_map::ordered_map::OrderedMap;

    use crate::attrs::attr::Attribute;
    use crate::attrs::spec::AttributeSpec;
    use crate::rule::RuleIncomingTransition;

    impl AttributeSpec {
        pub fn testing_new(attributes: OrderedMap<String, Attribute>) -> AttributeSpec {
            AttributeSpec::from(
                attributes.into_iter().collect(),
                false,
                &RuleIncomingTransition::None,
            )
            .unwrap()
        }
    }
}
