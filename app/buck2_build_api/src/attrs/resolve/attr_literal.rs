/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use starlark::collections::SmallMap;
use starlark::values::dict::Dict;
use starlark::values::list::AllocList;
use starlark::values::tuple::AllocTuple;
use starlark::values::Heap;
use starlark::values::Value;

use crate::attrs::resolve::coerced_attr::CoercedAttrResolveExt;

static_assertions::assert_eq_size!(AttrLiteral, [usize; 3]);

pub(crate) trait UnconfiguredAttrLiteralExt {
    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>>;
}

impl UnconfiguredAttrLiteralExt for AttrLiteral {
    fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self {
            AttrLiteral::None => Ok(Value::new_none()),
            AttrLiteral::Bool(b) => Ok(Value::new_bool(b.0)),
            AttrLiteral::Int(i) => Ok(Value::new_int(*i)),
            AttrLiteral::String(s) | AttrLiteral::EnumVariant(s) => {
                Ok(heap.alloc_str(s).to_value())
            }
            AttrLiteral::List(l) => {
                let mut v = Vec::with_capacity(l.len());
                for e in l.iter() {
                    v.push(e.to_value(heap)?);
                }
                Ok(heap.alloc(AllocList(v)))
            }
            AttrLiteral::Tuple(l) => {
                let mut v = Vec::with_capacity(l.len());
                for e in l.iter() {
                    v.push(e.to_value(heap)?);
                }
                Ok(heap.alloc(AllocTuple(v)))
            }
            AttrLiteral::Dict(d) => {
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

#[derive(thiserror::Error, Debug)]
pub(crate) enum ResolveError {
    #[error("Attribute cannot be converted to Starlark value: `{0}`")]
    AttrCannotBeConvertedToValue(String),
}
