/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use pagable::Pagable;

use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::display::AttrDisplayWithContextExt;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Pagable, Allocative)]
enum AttributeDefault {
    No,
    Yes(Arc<CoercedAttr>),
    DefaultOnly(Arc<CoercedAttr>),
}

/// Starlark compatible container for results from e.g. `attrs.string()`
#[derive(Clone, Debug, Eq, PartialEq, Hash, Pagable, Allocative)]
pub struct Attribute {
    /// The default value. If None, the value is not optional and must be provided by the user
    default: AttributeDefault,
    /// Documentation for what the attribute actually means
    doc: String,
    /// The coercer to take this parameter's value from Starlark value -> an
    /// internal representation
    coercer: AttrType,
}

impl Attribute {
    pub fn new(default: Option<Arc<CoercedAttr>>, doc: &str, coercer: AttrType) -> Self {
        Attribute {
            default: match default {
                Some(x) => AttributeDefault::Yes(x),
                None => AttributeDefault::No,
            },
            doc: doc.to_owned(),
            coercer,
        }
    }

    pub fn new_default_only(default: Arc<CoercedAttr>, doc: &str, coercer: AttrType) -> Self {
        Attribute {
            default: AttributeDefault::DefaultOnly(default),
            doc: doc.to_owned(),
            coercer,
        }
    }

    pub fn coercer(&self) -> &AttrType {
        &self.coercer
    }

    pub fn is_default_only(&self) -> bool {
        matches!(self.default, AttributeDefault::DefaultOnly(_))
    }

    pub fn default(&self) -> Option<&Arc<CoercedAttr>> {
        match &self.default {
            AttributeDefault::Yes(x) => Some(x),
            AttributeDefault::DefaultOnly(x) => Some(x),
            AttributeDefault::No => None,
        }
    }

    pub fn doc(&self) -> &str {
        &self.doc
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.coercer.fmt_with_default(
            f,
            self.default()
                .map(|x| x.as_display_no_ctx().to_string())
                .as_deref(),
        )
    }
}

/// Attribute which may be either a custom value supplied by the user, or missing/None to indicate use the default.
#[derive(Eq, PartialEq)]
pub enum CoercedValue {
    Custom(CoercedAttr),
    Default,
}
