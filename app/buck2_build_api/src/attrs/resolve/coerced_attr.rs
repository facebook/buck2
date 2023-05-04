/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use starlark::values::dict::Dict;
use starlark::values::list::AllocList;
use starlark::values::tuple::AllocTuple;
use starlark::values::Heap;
use starlark::values::Value;
use starlark_map::small_map::SmallMap;

use crate::attrs::resolve::attr_literal::ResolveError;

#[derive(Debug, thiserror::Error)]
enum CoercedAttrResolveError {
    #[error("Attribute cannot be converted to Starlark value: `{0}`")]
    AttrCannotBeConvertedToValue(String),
}

pub(crate) trait CoercedAttrResolveExt {
    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>>;
}

impl CoercedAttrResolveExt for CoercedAttr {
    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self {
            x @ (CoercedAttr::Concat(..) | CoercedAttr::Selector(..)) => {
                // It is possible to convert selects back to Starlark objects,
                // but there's no need to do it for now (and probably never will).
                Err(CoercedAttrResolveError::AttrCannotBeConvertedToValue(
                    x.as_display_no_ctx().to_string(),
                )
                .into())
            }
            CoercedAttr::None => Ok(Value::new_none()),
            CoercedAttr::Bool(b) => Ok(Value::new_bool(b.0)),
            CoercedAttr::Int(i) => Ok(Value::new_int(*i)),
            CoercedAttr::String(s) | CoercedAttr::EnumVariant(s) => {
                Ok(heap.alloc_str(s).to_value())
            }
            CoercedAttr::List(l) => {
                let mut v = Vec::with_capacity(l.len());
                for e in l.iter() {
                    v.push(e.to_value(heap)?);
                }
                Ok(heap.alloc(AllocList(v)))
            }
            CoercedAttr::Tuple(l) => {
                let mut v = Vec::with_capacity(l.len());
                for e in l.iter() {
                    v.push(e.to_value(heap)?);
                }
                Ok(heap.alloc(AllocTuple(v)))
            }
            CoercedAttr::Dict(d) => {
                let mut m = SmallMap::with_capacity(d.len());
                for (k, v) in d.iter() {
                    m.insert_hashed(k.to_value(heap)?.get_hashed()?, v.to_value(heap)?);
                }
                Ok(heap.alloc(Dict::new(m)))
            }
            x => {
                // For now this function is used to convert attributes to Starlark values
                // for transition rules which access attributes.
                //
                // For regular deps this function should fail.
                //
                // For configuration deps, this function should resolve attributes to providers,
                // but it is not implemented yet.
                Err(
                    ResolveError::AttrCannotBeConvertedToValue(x.as_display_no_ctx().to_string())
                        .into(),
                )
            }
        }
    }
}
