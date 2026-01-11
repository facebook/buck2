/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::convert::Infallible;
use std::ops::Deref;

use allocative::Allocative;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::typing::Ty;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;

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
    pub(crate) fn iter<'a, 'v>(&'a self, heap: Heap<'v>) -> impl Iterator<Item = Value<'v>> + 'a
    where
        'v: 'a,
    {
        self.0
            .iter()
            .map(move |target_node| target_node.dupe().alloc(heap))
    }
}

impl<Node: QueryTarget> Freeze for StarlarkTargetSet<Node> {
    type Frozen = StarlarkTargetSet<Node>;

    fn freeze(self, _freezer: &starlark::values::Freezer) -> FreezeResult<Self::Frozen> {
        Ok(self)
    }
}

impl<'v, Node: NodeLike> StarlarkTypeRepr for &'v StarlarkTargetSet<Node> {
    type Canonical = Self;

    fn starlark_type_repr() -> Ty {
        StarlarkTargetSet::<Node>::starlark_type_repr()
    }
}

impl<'v, Node: NodeLike> UnpackValue<'v> for &'v StarlarkTargetSet<Node> {
    type Error = Infallible;

    fn unpack_value_impl(x: Value<'v>) -> Result<Option<Self>, Self::Error> {
        Ok(StarlarkTargetSet::from_value(x))
    }
}

impl<'v, Node: NodeLike> AllocValue<'v> for StarlarkTargetSet<Node> {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

#[starlark_value(type = "target_set")]
impl<'v, Node: NodeLike> StarlarkValue<'v> for StarlarkTargetSet<Node> {
    type Canonical = Self;

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_target_set_methods)
    }

    fn iterate_collect(&self, heap: Heap<'v>) -> starlark::Result<Vec<Value<'v>>> {
        Ok(self.iter(heap).collect())
    }

    fn at(&self, index: Value<'v>, heap: Heap<'v>) -> starlark::Result<Value<'v>> {
        let i = i32::unpack_value_err(index)?;
        if let Ok(i) = usize::try_from(i) {
            if let Some(node) = self.0.get_index(i) {
                return Ok(node.dupe().alloc(heap));
            }
        }
        Err(ValueError::IndexOutOfBound(i).into())
    }

    fn length(&self) -> starlark::Result<i32> {
        self.0.len().try_into().map_err(starlark::Error::new_other)
    }

    fn add(&self, other: Value<'v>, heap: Heap<'v>) -> Option<starlark::Result<Value<'v>>> {
        let other = other.downcast_ref::<Self>()?;
        let union = self.0.union(&other.0);
        Some(Ok(heap.alloc(Self(union))))
    }

    fn sub(&self, other: Value<'v>, heap: Heap<'v>) -> starlark::Result<Value<'v>> {
        let Some(other) = other.downcast_ref::<Self>() else {
            return ValueError::unsupported_with(self, "-", other);
        };
        let difference = self.0.difference(&other.0)?;
        Ok(heap.alloc(Self(difference)))
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        match other.downcast_ref::<StarlarkTargetSet<Node>>() {
            Some(other) => Ok(self.0 == other.0),
            None => Ok(false),
        }
    }

    fn bit_and(&self, other: Value<'v>, heap: Heap<'v>) -> starlark::Result<Value<'v>> {
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

impl<Node: QueryTarget> FromIterator<Node> for StarlarkTargetSet<Node> {
    fn from_iter<Iter: IntoIterator<Item = Node>>(iter: Iter) -> Self {
        let targets = TargetSet::from_iter(iter);
        Self(targets)
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

/// A set-like object for managing buck2 target nodes.
///
/// It can be obtained from several functions in [`bxl.Context`](../Context) and
/// [`bxl.UqueryContext`](../UqueryContext)/[`bxl.CqueryContext`](../CqueryContext)/[`bxl.AqueryContext`](../AqueryContext),
/// among other places.
///
/// It can be either [`ConfiguredTargetSet`](../ConfiguredTargetSet) or [`UnconfiguredTargetSet`](../UnconfiguredTargetSet) which respectively contain either [`ConfiguredTargetNode`](../ConfiguredTargetNode) or [`UnconfiguredTargetNode`](../UnconfiguredTargetNode).
///
/// It provides common set operations for target nodes.
/// It supports iteration, indexing, addition (union), subtraction (difference), equality comparison, and intersection operations.
///
/// Operations:
/// * `+`  : Union of two TargetSets
/// * `-`  : Difference between two TargetSets
/// * `==` : Equality comparison
/// * `&` : Intersection of two TargetSets
/// * `[]` : Index access
/// * `len()`: Number of targets in set
/// * `iter()`: Iteration over targets
/// * constructor: [`bxl.ctarget_set()`](../#ctarget_set) for `ConfiguredTargetSet` and [`bxl.utarget_set()`](../#utarget_set) for `UnconfiguredTargetSet`
///
/// Example:
/// ```python
/// # Combine sets
/// all_targets = targets1 + targets2  # Union
///
/// # Remove targets
/// remaining = targets1 - targets2    # Difference
///
/// # Check if sets are equal
/// if targets1 == targets2:
///     print("Sets contain same targets")
///
/// # Iterate through targets
/// for target in targets1:
///    print(target)
///
///  # Get target by index
/// first_target = targets1[0]
///
/// # Get number of targets
/// count = len(targets1)
///
/// # Intersection of sets
/// common = targets1 & targets2
/// ```
#[starlark_module]
fn starlark_target_set_methods(builder: &mut MethodsBuilder) {}
