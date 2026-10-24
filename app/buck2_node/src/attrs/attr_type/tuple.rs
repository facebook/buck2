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
use std::fmt::Debug;
use std::fmt::Formatter;
use std::ops::Deref;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_error::internal_error;
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

#[derive(Clone, Debug, Eq, PartialEq, Hash, Pagable, Allocative, StrongHash)]
pub struct TupleAttrType {
    pub xs: Vec<AttrType>,
}

impl TupleAttrType {
    pub fn new(xs: Vec<AttrType>) -> Self {
        Self { xs }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attrs.tuple(")?;
        for (i, x) in self.xs.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{x}")?;
        }
        write!(f, "{arg})")
    }
}

#[derive(
    Debug, Clone, Eq, PartialEq, Hash, Allocative, Default, Pagable, StrongHash
)]
pub struct TupleLiteral<C: Eq>(pub ArcSlice<C>);

impl<C: Eq> Deref for TupleLiteral<C> {
    type Target = ArcSlice<C>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<C: Eq + AttrDisplayWithContext> AttrDisplayWithContext for TupleLiteral<C> {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_container(f, "(", ")", self.0.iter().map(|v| v.as_display(ctx)))
    }
}

impl<C: Eq> FromIterator<C> for TupleLiteral<C> {
    fn from_iter<T: IntoIterator<Item = C>>(iter: T) -> Self {
        TupleLiteral(ArcSlice::from_iter(iter))
    }
}

impl<C: Eq + AnyMatches> AnyMatches for TupleLiteral<C> {
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

impl<C: Eq + ToJsonWithContext> ToJsonWithContext for TupleLiteral<C> {
    fn to_json(&self, ctx: &AttrFmtContext) -> buck2_error::Result<Value> {
        Ok(to_value(
            self.iter()
                .map(|c| c.to_json(ctx))
                .collect::<Result<Vec<_>, _>>()?,
        )?)
    }
}

impl Semantics for kinds::Tuple {
    fn traverse<'a>(
        (items,): Self::Parts<'a>,
        Traverse { v, t, pkg, tr }: Traverse<'_, 'a>,
    ) -> buck2_error::Result<()> {
        let inner = inner_type(t);
        match &inner.0.inner {
            AttrTypeInner::Any(_) => {
                for item in items.iter() {
                    item.traverse(inner, pkg, tr)?;
                }
            }
            AttrTypeInner::Tuple(ty) => {
                if items.len() != ty.xs.len() {
                    return Err(internal_error!("Inconsistent number of elements in tuple"));
                }
                for (item, item_t) in items.iter().zip(&ty.xs) {
                    item.traverse(item_t, pkg, tr)?;
                }
            }
            _ => return Err(mismatch(v, t)),
        }
        Ok(())
    }

    fn configure(
        (items,): Self::Parts<'_>,
        Configure { v, t, ctx, name }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        let configured = match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => items
                .iter()
                .map(|item| item.configure(AttrType::any_ref(), ctx, name))
                .collect::<ResultMaybeCompatible<Vec<_>>>()?
                .into(),
            AttrTypeInner::Tuple(ty) => {
                if items.len() != ty.xs.len() {
                    return internal_error!("Inconsistent number of elements in tuple").into();
                }
                items
                    .iter()
                    .zip(&ty.xs)
                    .map(|(item, item_t)| item.configure(item_t, ctx, name))
                    .collect::<ResultMaybeCompatible<_>>()?
            }
            _ => return mismatch(v, t).into(),
        };
        ResultMaybeCompatible::Compatible(ConfiguredAttr::Tuple(TupleLiteral(configured)))
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
        match &inner_type(t).0.inner {
            AttrTypeInner::Any(_) => {}
            AttrTypeInner::Tuple(ty) => {
                if items.len() != ty.xs.len() {
                    return internal_error!("Inconsistent number of elements in tuple").into();
                }
                for (item, item_t) in items.iter().zip(&ty.xs) {
                    item.traverse_configured(item_t, pkg, ctx, tr, name)?;
                }
            }
            _ => return mismatch(v, t).into(),
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
        fmt_seq(items, "(", ")", ctx, f)
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
