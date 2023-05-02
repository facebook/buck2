/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Formatter;
use std::ops::Deref;

use allocative::Allocative;
use buck2_util::arc_str::ArcSlice;
use gazebo::prelude::SliceExt;
use serde_json::to_value;
use serde_json::Value;

use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::attr_type::AttrType;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::json::ToJsonWithContext;

#[derive(Debug, Hash, Eq, PartialEq, Allocative)]
pub struct ListAttrType {
    pub inner: AttrType,
}

impl ListAttrType {
    pub(crate) fn new(inner: AttrType) -> Self {
        Self { inner }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attrs.list({}{})", self.inner, arg)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Allocative, Default)]
pub struct ListLiteral<C: Eq>(pub ArcSlice<C>);

impl<C: Eq> Deref for ListLiteral<C> {
    type Target = ArcSlice<C>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<C: Eq + AttrDisplayWithContext> AttrDisplayWithContext for ListLiteral<C> {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, v) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, ",")?;
            }
            AttrDisplayWithContext::fmt(v, ctx, f)?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl<C: Eq> FromIterator<C> for ListLiteral<C> {
    fn from_iter<T: IntoIterator<Item = C>>(iter: T) -> Self {
        ListLiteral(ArcSlice::from_iter(iter))
    }
}

impl<C: Eq + AnyMatches> AnyMatches for ListLiteral<C> {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        for v in self.0.iter() {
            if v.any_matches(filter)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl<C: Eq + ToJsonWithContext> ToJsonWithContext for ListLiteral<C> {
    fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<Value> {
        Ok(to_value(self.try_map(|c| c.to_json(ctx))?)?)
    }
}
