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
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use dupe::Dupe;
use pagable::Pagable;
use starlark_map::Hashed;
use starlark_map::small_set::SmallSet;

use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::traversal::CoercedAttrTraversal;

#[derive(Clone, Debug, Hash, Eq, PartialEq, Pagable, Allocative)]
enum AttributeDefault {
    No,
    Yes(Arc<CoercedAttr>),
    YesWithAllowed {
        default: Arc<CoercedAttr>,
        allowed_deps: Hashed<SmallSet<TargetLabel>>,
    },
    // N.B. DefaultOnly attributes are not checked for within_view, so we don't have to track allowed_deps for them
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
    pub fn new_const(default: Option<Arc<CoercedAttr>>, doc: &str, coercer: AttrType) -> Self {
        Self::new(default, doc, coercer).expect("Attribute::new_const failed")
    }

    pub fn new(
        default: Option<Arc<CoercedAttr>>,
        doc: &str,
        coercer: AttrType,
    ) -> buck2_error::Result<Self> {
        Ok(Attribute {
            default: match default {
                Some(default) => {
                    let allowed_deps = collect_default_deps(&default, &coercer)?;
                    if allowed_deps.is_empty() {
                        AttributeDefault::Yes(default)
                    } else {
                        AttributeDefault::YesWithAllowed {
                            default,
                            allowed_deps: allowed_deps.hashed(),
                        }
                    }
                }
                None => AttributeDefault::No,
            },
            doc: doc.to_owned(),
            coercer,
        })
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
            AttributeDefault::Yes(x) | AttributeDefault::YesWithAllowed { default: x, .. } => {
                Some(x)
            }
            AttributeDefault::DefaultOnly(x) => Some(x),
            AttributeDefault::No => None,
        }
    }

    pub fn default_allowed_deps(&self) -> Option<&SmallSet<TargetLabel>> {
        match &self.default {
            AttributeDefault::YesWithAllowed { allowed_deps, .. } => Some(&**allowed_deps),
            AttributeDefault::Yes(_) | AttributeDefault::DefaultOnly(_) | AttributeDefault::No => {
                None
            }
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

fn collect_default_deps(
    default: &Arc<CoercedAttr>,
    attr_type: &AttrType,
) -> buck2_error::Result<SmallSet<TargetLabel>> {
    struct CollectDefaultsTraversal<'a> {
        deps: &'a mut SmallSet<TargetLabel>,
    }

    // N.B. the traversal here needs to match the `dep` cases that check_within_view checks
    impl<'a> CoercedAttrTraversal<'a> for CollectDefaultsTraversal<'a> {
        fn dep(&mut self, dep: &ProvidersLabel) -> buck2_error::Result<()> {
            self.deps.insert(dep.target().dupe());
            Ok(())
        }

        fn configuration_dep(
            &mut self,
            dep: &ProvidersLabel,
            t: ConfigurationDepKind,
        ) -> buck2_error::Result<()> {
            match t {
                // Skip some configuration deps
                ConfigurationDepKind::CompatibilityAttribute
                | ConfigurationDepKind::DefaultTargetPlatform
                | ConfigurationDepKind::SelectKey => (),
                ConfigurationDepKind::ConfiguredDepPlatform | ConfigurationDepKind::Transition => {
                    self.deps.insert(dep.target().dupe());
                }
            }
            Ok(())
        }

        fn input(&mut self, _input: SourcePathRef) -> buck2_error::Result<()> {
            Ok(())
        }
    }

    let mut default_deps = SmallSet::new();
    default.traverse(
        attr_type,
        None,
        &mut CollectDefaultsTraversal {
            deps: &mut default_deps,
        },
    )?;
    Ok(default_deps)
}
