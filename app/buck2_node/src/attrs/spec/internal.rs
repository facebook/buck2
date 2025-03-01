/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Builtin note attributes.

use std::sync::Arc;

use once_cell::sync::Lazy;
use starlark_map::ordered_map::OrderedMap;

use crate::attrs::attr::Attribute;
use crate::attrs::attr_type::any::AnyAttrType;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configurable::AttrIsConfigurable;
use crate::attrs::values::TargetModifiersValue;
use crate::metadata::map::MetadataMap;
use crate::visibility::VisibilitySpecification;
use crate::visibility::WithinViewSpecification;

// TODO(cjhopman): figure out something better for these default attributes that we need to interpret
// internally. There's currently a lot of awkwardness involved: accessing the value, needing to create
// the repr string, setting defaults. Some of that is just about making it easier to work with the
// coerced attrs and some of it is about a nicer structure for defining these attributes and
// accessing them off nodes.

pub struct InternalAttribute {
    pub name: &'static str,
    attr: fn() -> Attribute,
    is_configurable: AttrIsConfigurable,
}

pub const NAME_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "name",
    attr: || Attribute::new(None, "name of the target", AttrType::string()),
    is_configurable: AttrIsConfigurable::No,
};

pub(crate) const DEFAULT_TARGET_PLATFORM_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "default_target_platform",
    attr: || {
        Attribute::new(
            Some(Arc::new(CoercedAttr::None)),
            "specifies the default target platform, used when no platforms are specified on the command line",
            AttrType::option(AttrType::label()),
        )
    },
    is_configurable: AttrIsConfigurable::No,
};

/// buck1 used "compatible_with" for this. in buck2, we have two "compatible with" concepts, both
/// target and exec compatibility and so we are switching to "target_compatible_with". For now we'll accept
/// either form for target compatibility (but not both).
pub const TARGET_COMPATIBLE_WITH_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "target_compatible_with",
    attr: || {
        Attribute::new(
            Some(Arc::new(AnyAttrType::empty_list())),
            "a list of constraints that are required to be satisfied for this target to be compatible with a configuration",
            AttrType::list(AttrType::configuration_dep(
                ConfigurationDepKind::CompatibilityAttribute,
            )),
        )
    },
    is_configurable: AttrIsConfigurable::Yes,
};

pub const LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "compatible_with",
    attr: || {
        Attribute::new(
            Some(Arc::new(AnyAttrType::empty_list())),
            "a list of constraints that are required to be satisfied for this target to be compatible with a configuration",
            AttrType::list(AttrType::configuration_dep(
                ConfigurationDepKind::CompatibilityAttribute,
            )),
        )
    },
    // `compatible_with` is not configurable in Buck v1.
    is_configurable: AttrIsConfigurable::No,
};

pub const EXEC_COMPATIBLE_WITH_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "exec_compatible_with",
    attr: || {
        Attribute::new(
            Some(Arc::new(AnyAttrType::empty_list())),
            "a list of constraints that are required to be satisfied for this target to be compatible with an execution platform",
            AttrType::list(AttrType::configuration_dep(
                ConfigurationDepKind::CompatibilityAttribute,
            )),
        )
    },
    is_configurable: AttrIsConfigurable::Yes,
};

pub const VISIBILITY_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "visibility",
    attr: || {
        Attribute::new(
            Some(Arc::new(CoercedAttr::Visibility(
                VisibilitySpecification::DEFAULT,
            ))),
            "a list of visibility patterns restricting what targets can depend on this one",
            AttrType::visibility(),
        )
    },
    // visibility attributes aren't configurable so that we can cache them on targetnodes.
    is_configurable: AttrIsConfigurable::No,
};

pub const WITHIN_VIEW_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "within_view",
    attr: || {
        Attribute::new(
            Some(Arc::new(CoercedAttr::WithinView(
                WithinViewSpecification::PUBLIC,
            ))),
            "a list of visibility patterns restricting what this target can depend on",
            AttrType::within_view(),
        )
    },
    is_configurable: AttrIsConfigurable::No,
};

pub(crate) const METADATA_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "metadata",
    attr: || {
        Attribute::new(
            Some(Arc::new(CoercedAttr::Metadata(MetadataMap::default()))),
            "a key-value map of metadata associated with this target",
            AttrType::metadata(),
        )
    },
    is_configurable: AttrIsConfigurable::No,
};

pub(crate) const TARGET_MODIFIERS_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "modifiers",
    attr: || {
        Attribute::new(
            Some(Arc::new(CoercedAttr::TargetModifiers(
                TargetModifiersValue::new(serde_json::Value::Array(vec![])),
            ))),
            "an array of modifiers associated with this target",
            AttrType::target_modifiers(),
        )
    },
    is_configurable: AttrIsConfigurable::No,
};

pub(crate) const TESTS_ATTRIBUTE: InternalAttribute = InternalAttribute {
    name: "tests",
    attr: || {
        Attribute::new(
            Some(Arc::new(AnyAttrType::empty_list())),
            "a list of targets that provide tests for this one",
            AttrType::list(AttrType::label()),
        )
    },
    is_configurable: AttrIsConfigurable::Yes,
};

const INTERNAL_ATTRS: [InternalAttribute; 10] = [
    NAME_ATTRIBUTE,
    DEFAULT_TARGET_PLATFORM_ATTRIBUTE,
    TARGET_COMPATIBLE_WITH_ATTRIBUTE,
    LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE,
    EXEC_COMPATIBLE_WITH_ATTRIBUTE,
    VISIBILITY_ATTRIBUTE,
    WITHIN_VIEW_ATTRIBUTE,
    METADATA_ATTRIBUTE,
    TESTS_ATTRIBUTE,
    TARGET_MODIFIERS_ATTRIBUTE,
];

pub fn is_internal_attr(name: &str) -> bool {
    internal_attrs().contains_key(name)
}

pub fn internal_attrs() -> &'static OrderedMap<&'static str, Attribute> {
    static ATTRS: Lazy<OrderedMap<&'static str, Attribute>> = Lazy::new(|| {
        OrderedMap::from_iter(INTERNAL_ATTRS.iter().map(|attr| (attr.name, (attr.attr)())))
    });
    &ATTRS
}

pub fn attr_is_configurable(name: &str) -> AttrIsConfigurable {
    for a in &INTERNAL_ATTRS {
        if a.name == name {
            return a.is_configurable;
        }
    }
    AttrIsConfigurable::Yes
}
