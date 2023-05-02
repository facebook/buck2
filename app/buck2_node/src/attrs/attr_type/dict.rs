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
use serde_json::Value;

use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::attr_type::AttrType;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::json::ToJsonWithContext;

#[derive(Debug, Hash, Eq, PartialEq, Allocative)]
pub struct DictAttrType {
    pub key: AttrType,
    pub value: AttrType,
    pub sorted: bool,
}

impl DictAttrType {
    pub fn new(key: AttrType, value: AttrType, sorted: bool) -> Self {
        Self { key, value, sorted }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(
            f,
            "attrs.dict({}, {}, sorted={}{})",
            self.key,
            self.value,
            if self.sorted { "True" } else { "False" },
            arg
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Allocative, Default)]
pub struct DictLiteral<C: Eq>(pub ArcSlice<(C, C)>);

impl<C: Eq> Deref for DictLiteral<C> {
    type Target = ArcSlice<(C, C)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<C: Eq + AttrDisplayWithContext> AttrDisplayWithContext for DictLiteral<C> {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (i, (k, v)) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, ",")?;
            }
            write!(f, "{}: {}", k.as_display(ctx), v.as_display(ctx))?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl<C: Eq> FromIterator<(C, C)> for DictLiteral<C> {
    fn from_iter<T: IntoIterator<Item = (C, C)>>(iter: T) -> Self {
        DictLiteral(ArcSlice::from_iter(iter))
    }
}

impl<C: Eq + AnyMatches> AnyMatches for DictLiteral<C> {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        for (k, v) in self.0.iter() {
            if k.any_matches(filter)? || v.any_matches(filter)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl<C: Eq + ToJsonWithContext> ToJsonWithContext for DictLiteral<C> {
    fn to_json(&self, ctx: &AttrFmtContext) -> anyhow::Result<Value> {
        let mut res: serde_json::Map<String, serde_json::Value> =
            serde_json::Map::with_capacity(self.len());
        for (k, v) in self.iter() {
            res.insert(
                k.to_json(ctx)?.as_str().unwrap().to_owned(),
                v.to_json(ctx)?,
            );
        }
        Ok(res.into())
    }
}
