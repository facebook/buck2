/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::collections::ordered_map::OrderedMap;
use starlark_map::small_map;

use crate::attrs::attr::Attribute;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::id::AttributeId;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::attrs::internal::internal_attrs;
use crate::attrs::values::AttrValues;

/// AttributeSpec holds the specification for a rules attributes as defined in the rule() call. This
/// is split into a mapping of "attribute name" -> "attribute id". The Attributes are stored in a vec
/// that can then be indexed using the name->id mapping (the id is really just an index into this vec).
///
/// For its attribute values, a TargetNode will hold a sorted Vec<(AttributeId, CoercedAttr)> that will have values
/// only for the values that are explicitly set. Default values need to be looked up through the AttributeSpec.
#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct AttributeSpec {
    attributes: OrderedMap<String, Attribute>,
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum AttributeSpecError {
    #[error("User provided attribute `{0}` overrides internal attribute")]
    InternalAttributeRedefined(String),
    #[error("Duplicate attribute `{0}`")]
    DuplicateAttribute(String),
    #[error("Rule definition has no attribute `{0}`")]
    UnknownAttribute(String),
}

impl AttributeSpec {
    pub fn from(attributes: Vec<(String, Attribute)>) -> anyhow::Result<Self> {
        let internal_attrs = internal_attrs();

        let mut instances: OrderedMap<String, Attribute> =
            OrderedMap::with_capacity(attributes.len());
        for (name, instance) in internal_attrs {
            let prev = instances.insert((*name).to_owned(), instance.clone());
            if prev.is_some() {
                unreachable!("duplicate internal attr: '{}'", name);
            }
        }

        for (name, instance) in attributes.into_iter() {
            match instances.entry(name) {
                small_map::Entry::Vacant(e) => {
                    e.insert(instance);
                }
                small_map::Entry::Occupied(e) => {
                    let name = e.key();
                    if internal_attrs.contains_key(name.as_str()) {
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
        }

        Ok(AttributeSpec {
            attributes: instances,
        })
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
                    name.as_str(),
                    AttributeId {
                        index_in_attribute_spec,
                    },
                    attribute,
                )
            })
    }

    fn attribute_by_id(&self, id: AttributeId) -> &Attribute {
        self.attributes
            .get_index(id.index_in_attribute_spec)
            .unwrap()
            .1
    }

    fn attribute_id_by_name(&self, name: &str) -> Option<AttributeId> {
        self.attributes
            .get_index_of(name)
            .map(|index_in_attribute_spec| AttributeId {
                index_in_attribute_spec,
            })
    }

    pub fn attribute(&self, name: &str) -> Option<&Attribute> {
        self.attributes.get(name)
    }

    /// Returns an iterator over all of the attribute (name, value) pairs.
    pub fn attrs<'v>(
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

    pub fn known_attr_or_none<'v>(
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
            return self.attribute_by_id(idx).default.as_deref();
        }
        None
    }

    pub fn attr_or_none<'v>(
        &'v self,
        attr_values: &'v AttrValues,
        key: &str,
        opts: AttrInspectOptions,
    ) -> Option<&'v CoercedAttr> {
        if let Some(idx) = self.attribute_id_by_name(key) {
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
    ) -> anyhow::Result<Option<&'v CoercedAttr>> {
        if let Some(idx) = self.attribute_id_by_name(key) {
            Ok(self.known_attr_or_none(idx, attr_values, opts))
        } else {
            Err(AttributeSpecError::UnknownAttribute(key.to_owned()).into())
        }
    }
}

pub(crate) mod testing {

    use buck2_core::collections::ordered_map::OrderedMap;

    use crate::attrs::attr::Attribute;
    use crate::attrs::spec::AttributeSpec;

    impl AttributeSpec {
        pub(crate) fn testing_new(attributes: OrderedMap<String, Attribute>) -> AttributeSpec {
            AttributeSpec { attributes }
        }
    }
}
