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
use std::fmt::Formatter;
use std::ops::Deref;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_util::arc_str::ArcSlice;
use display_container::fmt_container;
use pagable::Pagable;
use serde::Serializer;
use serde_json::Value;
use serde_json::to_value;
use strong_hash::StrongHash;

use crate::attrs::arena::kinds;
use crate::attrs::arena::semantics::Configure;
use crate::attrs::arena::semantics::Fmt;
use crate::attrs::arena::semantics::MatchesAny;
use crate::attrs::arena::semantics::Semantics;
use crate::attrs::arena::semantics::SerializeKind;
use crate::attrs::arena::semantics::ToJson;
use crate::attrs::arena::semantics::Traverse;
use crate::attrs::arena::semantics::TraverseConfigured;
use crate::attrs::arena::semantics::any_matches_seq;
use crate::attrs::arena::semantics::fmt_seq;
use crate::attrs::arena::semantics::inner_type;
use crate::attrs::arena::semantics::json_seq;
use crate::attrs::arena::semantics::mismatch;
use crate::attrs::arena::semantics::ser_seq;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::json::ToJsonWithContext;

#[derive(Debug, Hash, Pagable, Eq, PartialEq, Allocative, StrongHash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash, Allocative, Pagable, StrongHash)]
#[pagable(bound = "C: Eq + Pagable")]
pub struct ListLiteral<C: Eq>(pub ArcSlice<C>);

impl<C: Eq> Default for ListLiteral<C> {
    fn default() -> Self {
        Self(ArcSlice::new([]))
    }
}

impl<C: Eq> Deref for ListLiteral<C> {
    type Target = ArcSlice<C>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<C: Eq + AttrDisplayWithContext> AttrDisplayWithContext for ListLiteral<C> {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_container(f, "[", "]", self.0.iter().map(|v| v.as_display(ctx)))
    }
}

impl<C: Eq> FromIterator<C> for ListLiteral<C> {
    fn from_iter<T: IntoIterator<Item = C>>(iter: T) -> Self {
        ListLiteral(ArcSlice::from_iter(iter))
    }
}

impl<C: Eq + AnyMatches> AnyMatches for ListLiteral<C> {
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        for v in self.0.iter() {
            if v.any_matches(filter)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl<C: Eq + ToJsonWithContext> ToJsonWithContext for ListLiteral<C> {
    fn to_json(&self, ctx: &AttrFmtContext) -> buck2_error::Result<Value> {
        Ok(to_value(
            self.iter()
                .map(|c| c.to_json(ctx))
                .collect::<Result<Vec<_>, _>>()?,
        )?)
    }
}

impl Semantics for kinds::List {
    const CONCAT: Option<&'static str> = Some("list");

    fn traverse<'a>(
        (items,): Self::Parts<'a>,
        Traverse { v, t, pkg, tr }: Traverse<'_, 'a>,
    ) -> buck2_error::Result<()> {
        let inner = inner_type(t);
        let item_t = match &inner.0.inner {
            AttrTypeInner::Any(_) => inner,
            AttrTypeInner::List(ty) => &ty.inner,
            _ => return Err(mismatch(v, t)),
        };
        for item in items.iter() {
            item.traverse(item_t, pkg, tr)?;
        }
        Ok(())
    }

    fn configure(
        (items,): Self::Parts<'_>,
        Configure { v, t, ctx, name }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        let inner = inner_type(t);
        let item_t = match &inner.0.inner {
            AttrTypeInner::Any(_) => AttrType::any_ref(),
            AttrTypeInner::List(ty) => &ty.inner,
            _ => return mismatch(v, t).into(),
        };
        ResultMaybeCompatible::Compatible(ConfiguredAttr::List(ListLiteral(
            items
                .iter()
                .map(|item| item.configure(item_t, ctx, name))
                .collect::<ResultMaybeCompatible<Vec<_>>>()?
                .into(),
        )))
    }

    fn traverse_configured(
        (items,): Self::Parts<'_>,
        TraverseConfigured {
            v,
            t,
            pkg,
            ctx,
            tr,
            name,
        }: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        let item_t = match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => return ResultMaybeCompatible::Compatible(()),
            AttrTypeInner::List(ty) => &ty.inner,
            _ => return mismatch(v, t).into(),
        };
        for item in items.iter() {
            item.traverse_configured(item_t, pkg, ctx, tr, name)?;
        }
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json(
        (items,): Self::Parts<'_>,
        ToJson { ctx, .. }: ToJson<'_, '_>,
    ) -> buck2_error::Result<serde_json::Value> {
        json_seq(items, ctx)
    }

    fn fmt((items,): Self::Parts<'_>, Fmt { ctx, f, .. }: Fmt<'_, '_, '_>) -> std::fmt::Result {
        fmt_seq(items, "[", "]", ctx, f)
    }

    fn any_matches(
        (items,): Self::Parts<'_>,
        MatchesAny { filter, .. }: MatchesAny<'_, '_>,
    ) -> buck2_error::Result<bool> {
        any_matches_seq(items, filter)
    }

    fn serialize<S: Serializer>(
        (items,): Self::Parts<'_>,
        SerializeKind { ctx, ser, .. }: SerializeKind<'_, '_, S>,
    ) -> Result<S::Ok, S::Error> {
        ser_seq(items, ctx, ser)
    }
}
