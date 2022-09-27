/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::coerced_attr::CoercedAttr;
use starlark::values::Heap;
use starlark::values::Value;

use crate::attrs::resolve::attr_literal::UnconfiguredAttrLiteralExt;

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
            CoercedAttr::Literal(v) => v.to_value(heap),
            x @ (CoercedAttr::Concat(..) | CoercedAttr::Selector(..)) => {
                // It is possible to convert selects back to Starlark objects,
                // but there's no need to do it for now (and probably never will).
                Err(CoercedAttrResolveError::AttrCannotBeConvertedToValue(x.to_string()).into())
            }
        }
    }
}
