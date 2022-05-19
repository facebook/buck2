/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{cmp::Ordering, fmt};

use anyhow::anyhow;
use starlark::values::{dict::Dict, Value};

use crate::{
    attrs::{
        attr_type::{attr_literal::CoercionError, AttrType},
        AttrCoercionContext, AttrLiteral, CoercedAttr,
    },
    interpreter::rule_defs::attr::AttrIsConfigurable,
};

#[derive(Debug, Hash, Eq, PartialEq)]
pub(crate) struct DictAttrType {
    key: AttrType,
    value: AttrType,
    sorted: bool,
}

impl DictAttrType {
    pub fn new(key: AttrType, value: AttrType, sorted: bool) -> Self {
        Self { key, value, sorted }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(
            f,
            "attr.dict({}, {}, sorted={}{})",
            self.key, self.value, self.sorted, arg
        )
    }
}

impl DictAttrType {
    pub(crate) fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        if let Some(dict) = Dict::from_value(value) {
            let mut res = Vec::with_capacity(dict.len());
            if self.sorted {
                // First sort the values
                let mut items = dict.iter().collect::<Vec<_>>();
                // If two things are incompatible, just return Eq. The resulting order is undefined, but safely undefined.
                items.sort_by(|a, b| a.0.compare(b.0).unwrap_or(Ordering::Equal));

                for (k, v) in items {
                    res.push((
                        self.key.coerce(configurable, ctx, k)?,
                        self.value.coerce(configurable, ctx, v)?,
                    ));
                }
            } else {
                for (k, v) in dict.iter() {
                    res.push((
                        self.key.coerce(configurable, ctx, k)?,
                        self.value.coerce(configurable, ctx, v)?,
                    ));
                }
            }
            Ok(AttrLiteral::Dict(res))
        } else {
            Err(anyhow!(CoercionError::type_error(Dict::TYPE, value,)))
        }
    }

    pub(crate) fn starlark_type(&self) -> String {
        format!(
            "{{{}: {}}}",
            self.key.starlark_type(),
            self.value.starlark_type()
        )
    }
}
