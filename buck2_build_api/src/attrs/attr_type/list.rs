/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use anyhow::anyhow;
use gazebo::prelude::*;
use starlark::values::{list::List, tuple::Tuple, Value};

use crate::{
    attrs::{
        attr_type::{attr_literal::CoercionError, AttrType},
        AttrCoercionContext, AttrLiteral, CoercedAttr,
    },
    interpreter::rule_defs::attr::AttrIsConfigurable,
};

#[derive(Debug, Hash, Eq, PartialEq)]
pub(crate) struct ListAttrType {
    inner: AttrType,
}

impl ListAttrType {
    pub(crate) fn new(inner: AttrType) -> Self {
        Self { inner }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attr.list({}{})", self.inner, arg)
    }
}

impl ListAttrType {
    pub(super) fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        if let Some(list) = List::from_value(value) {
            Ok(AttrLiteral::List(
                list.content()
                    .try_map(|v| (self.inner).coerce(configurable, ctx, *v))?
                    .into_boxed_slice(),
                self.inner.dupe(),
            ))
        } else if let Some(list) = Tuple::from_value(value) {
            Ok(AttrLiteral::List(
                list.content()
                    .try_map(|v| (self.inner).coerce(configurable, ctx, *v))?
                    .into_boxed_slice(),
                self.inner.dupe(),
            ))
        } else {
            Err(anyhow!(CoercionError::type_error(List::TYPE, value,)))
        }
    }

    pub(super) fn starlark_type(&self) -> String {
        format!("[{}]", self.inner.starlark_type())
    }
}
