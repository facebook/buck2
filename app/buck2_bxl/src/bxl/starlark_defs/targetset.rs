/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;

use allocative::Allocative;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::eval::set::TargetSetExt;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::typing::Ty;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;

use crate::bxl::starlark_defs::alloc_node::AllocNode;

pub(crate) trait NodeLike = QueryTarget + std::fmt::Debug + Eq + Dupe + AllocNode + Allocative;

#[derive(Debug, Display, Clone)]
#[derive(NoSerialize, Allocative)] // TODO maybe this should be
/// The StarlarkValue implementation for TargetSet to expose it to starlark.
pub(crate) struct StarlarkTargetSet<Node: QueryTarget>(pub(crate) TargetSet<Node>);

// TODO(nga): derive it.
unsafe impl<'a, Node: QueryTarget + 'static> ProvidesStaticType<'a> for StarlarkTargetSet<Node> {
    type StaticType = Self;
}

impl<Node: QueryTarget + AllocNode> StarlarkTargetSet<Node> {
    pub(crate) fn iter<'a, 'v>(&'a self, heap: &'v Heap) -> impl Iterator<Item = Value<'v>> + 'a
    where
        'v: 'a,
    {
        self.0
            .iter()
            .map(|target_node| target_node.dupe().alloc(heap))
    }
}

impl<Node: QueryTarget> Freeze for StarlarkTargetSet<Node> {
    type Frozen = StarlarkTargetSet<Node>;

    fn freeze(self, _freezer: &starlark::values::Freezer) -> anyhow::Result<Self::Frozen> {
        Ok(self)
    }
}

impl<'v, Node: NodeLike> StarlarkTypeRepr for &'v StarlarkTargetSet<Node> {
    fn starlark_type_repr() -> Ty {
        StarlarkTargetSet::<Node>::starlark_type_repr()
    }
}

impl<'v, Node: NodeLike> UnpackValue<'v> for &'v StarlarkTargetSet<Node> {
    fn unpack_value(x: Value<'v>) -> Option<Self> {
        StarlarkTargetSet::from_value(x)
    }
}

impl<'v, Node: NodeLike> AllocValue<'v> for StarlarkTargetSet<Node> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

#[starlark_value(type = "target_set")]
impl<'v, Node: NodeLike> StarlarkValue<'v> for StarlarkTargetSet<Node> {
    type Canonical = Self;

    fn iterate_collect(&self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(self.iter(heap).collect())
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = i32::unpack_value_err(index)?;
        if let Ok(i) = usize::try_from(i) {
            if let Some(node) = self.0.get_index(i) {
                return Ok(node.dupe().alloc(heap));
            }
        }
        Err(anyhow::anyhow!(ValueError::IndexOutOfBound(i)))
    }

    fn length(&self) -> anyhow::Result<i32> {
        Ok(self.0.len().try_into()?)
    }

    fn add(&self, other: Value<'v>, heap: &'v Heap) -> Option<anyhow::Result<Value<'v>>> {
        let other = other.downcast_ref::<Self>()?;
        let union = self.0.union(&other.0);
        Some(Ok(heap.alloc(Self(union))))
    }

    fn sub(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let Some(other) = other.downcast_ref::<Self>() else {
            return ValueError::unsupported_with(self, "-", other);
        };
        let difference = self.0.difference(&other.0)?;
        Ok(heap.alloc(Self(difference)))
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match other.downcast_ref::<StarlarkTargetSet<Node>>() {
            Some(other) => Ok(self.0 == other.0),
            None => Ok(false),
        }
    }

    fn bit_and(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let Some(other) = other.downcast_ref::<Self>() else {
            return ValueError::unsupported_with(self, "&", other);
        };
        let intersect = self.0.intersect(&other.0)?;
        Ok(heap.alloc(Self(intersect)))
    }
}

impl<Node: QueryTarget> From<TargetSet<Node>> for StarlarkTargetSet<Node> {
    fn from(v: TargetSet<Node>) -> Self {
        Self(v)
    }
}

impl<Node: QueryTarget> Deref for StarlarkTargetSet<Node> {
    type Target = TargetSet<Node>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<Node: NodeLike> StarlarkTargetSet<Node> {
    pub(crate) fn from_value<'v>(x: Value<'v>) -> Option<&'v Self> {
        ValueLike::downcast_ref::<Self>(x)
    }
}
