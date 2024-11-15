/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::configured::ConfiguredTargetNodeRef;
use starlark::values::structs::AllocStruct;
use starlark::values::structs::StructRef;
use starlark::values::ValueOfUnchecked;

use crate::attrs::resolve::configured_attr::ConfiguredAttrExt;
use crate::attrs::resolve::ctx::AttrResolutionContext;

/// Prepare `ctx.attrs` for rule impl.
pub(crate) fn node_to_attrs_struct<'v>(
    node: ConfiguredTargetNodeRef,
    ctx: &dyn AttrResolutionContext<'v>,
) -> buck2_error::Result<ValueOfUnchecked<'v, StructRef<'static>>> {
    let attrs_iter = node.attrs(AttrInspectOptions::All);
    let mut resolved_attrs = Vec::with_capacity(attrs_iter.size_hint().0);
    for a in attrs_iter {
        resolved_attrs.push((a.name, a.value.resolve_single(node.label().pkg(), ctx)?));
    }
    Ok(ctx
        .heap()
        .alloc_typed_unchecked(AllocStruct(resolved_attrs))
        .cast())
}
