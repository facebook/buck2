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

use buck2_core::provider::id::ProviderId;
use gazebo::dupe::Dupe;
use once_cell::sync::Lazy;

use crate::attrs::attr::Attribute;
use crate::attrs::attr_type::any::AnyAttrType;
use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configurable::AttrIsConfigurable;

// TODO(cjhopman): figure out something better for these default attributes that we need to interpret
// internally. There's currently a lot of awkwardness involved: accessing the value, needing to create
// the repr string, setting defaults. Some of that is just about making it easier to work with the
// coerced attrs and some of it is about a nicer structure for defining these attributes and
// accessing them off nodes.
// TODO(cjhopman): these attributes should be marked as "unconfigurable" or "unselectable" or something
// since we need to be able to read them in their unconfigured form.
pub const NAME_ATTRIBUTE_FIELD: &str = "name";
pub const DEFAULT_TARGET_PLATFORM_ATTRIBUTE_FIELD: &str = "default_target_platform";

pub const TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD: &str = "target_compatible_with";
/// buck1 used "compatible_with" for this. in buck2, we have two "compatible with" concepts, both
/// target and exec compatibility and so we are switching to "target_compatible_with". For now we'll accept
/// either form for target compatibility (but not both).
pub const LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD: &str = "compatible_with";
pub const EXEC_COMPATIBLE_WITH_ATTRIBUTE_FIELD: &str = "exec_compatible_with";

pub const VISIBILITY_ATTRIBUTE_FIELD: &str = "visibility";

pub const TESTS_ATTRIBUTE_FIELD: &str = "tests";

fn name_attribute() -> Attribute {
    Attribute::new_internal(None, "name of the target".to_owned(), AttrType::string())
}

pub fn internal_attrs_platform_info_provider_id() -> &'static Arc<ProviderId> {
    static PLATFORM_INFO_PROVIDER_ID: Lazy<Arc<ProviderId>> = Lazy::new(|| {
        // Hardcode provider name, because we do not depend on providers here.
        Arc::new(ProviderId {
            path: None,
            name: "PlatformInfo".to_owned(),
        })
    });
    &PLATFORM_INFO_PROVIDER_ID
}

fn default_target_platform_attribute() -> Attribute {
    Attribute::new_internal(
        Some(Arc::new(CoercedAttr::Literal(
            AttrLiteral::None,
        ))),
        "specifies the default target platform, used when no platforms are specified on the command line".to_owned(),
        AttrType::option(AttrType::dep(vec![
            internal_attrs_platform_info_provider_id().dupe(),
        ])),
    )
}

fn target_compatible_with_attribute() -> Attribute {
    let entry_type = AttrType::configuration_dep();
    Attribute::new_internal(
        Some(Arc::new(AnyAttrType::empty_list(entry_type.dupe()),
        )),
        "a list of constraints that are required to be satisfied for this target to be compatible with a configuration".to_owned(),
        AttrType::list(entry_type),
    )
}

fn exec_compatible_with_attribute() -> Attribute {
    let entry_type = AttrType::configuration_dep();
    Attribute::new_internal(
        Some(Arc::new(AnyAttrType::empty_list(entry_type.dupe()),
        )),
        "a list of constraints that are required to be satisfied for this target to be compatible with an execution platform".to_owned(),
        AttrType::list(entry_type),
    )
}

fn visibility_attribute() -> Attribute {
    // TODO(cjhopman): We currently just use strings here and then do custom validation and conversion. Maybe we should have an attribute type for this.
    let entry_type = AttrType::string();
    Attribute::new_internal(
        Some(Arc::new(AnyAttrType::empty_list(entry_type.dupe()))),
        "a list of visibility patterns restricting what targets can depend on this one".to_owned(),
        AttrType::list(entry_type),
    )
}

fn tests_attribute() -> Attribute {
    let entry_type = AttrType::label();
    Attribute::new_internal(
        Some(Arc::new(AnyAttrType::empty_list(entry_type.dupe()))),
        "a list of targets that provide tests for this one".to_owned(),
        AttrType::list(entry_type),
    )
}

pub fn internal_attrs() -> Vec<(&'static str, Attribute)> {
    vec![
        (NAME_ATTRIBUTE_FIELD, name_attribute()),
        (
            DEFAULT_TARGET_PLATFORM_ATTRIBUTE_FIELD,
            default_target_platform_attribute(),
        ),
        (
            TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
            target_compatible_with_attribute(),
        ),
        (
            LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
            target_compatible_with_attribute(),
        ),
        (
            EXEC_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
            exec_compatible_with_attribute(),
        ),
        (VISIBILITY_ATTRIBUTE_FIELD, visibility_attribute()),
        (TESTS_ATTRIBUTE_FIELD, tests_attribute()),
    ]
}

pub fn attr_is_configurable(name: &str) -> AttrIsConfigurable {
    // `compatible_with` is not configurable in Buck v1.
    if name == NAME_ATTRIBUTE_FIELD
        || name == LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD
        || name == DEFAULT_TARGET_PLATFORM_ATTRIBUTE_FIELD
        // visibility attributes aren't configurable so that we can cache them on targetnodes.
        || name == VISIBILITY_ATTRIBUTE_FIELD
    {
        AttrIsConfigurable::No
    } else {
        AttrIsConfigurable::Yes
    }
}
