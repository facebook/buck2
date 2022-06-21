/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::{self};

use itertools::Itertools;
use starlark::values::Value;

use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::AttrType;
use crate::attrs::AttrCoercionContext;
use crate::attrs::CoercedAttr;
use crate::interpreter::rule_defs::attr::AttrIsConfigurable;

#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) struct OneOfAttrType {
    xs: Vec<AttrType>,
}

impl OneOfAttrType {
    pub(crate) fn new(xs: Vec<AttrType>) -> Self {
        Self { xs }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attr.one_of(")?;
        for (i, x) in self.xs.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            x.fmt(f)?;
        }
        write!(f, "{})", arg)
    }
}

impl OneOfAttrType {
    pub(crate) fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        let mut errs = Vec::new();
        // Bias towards the start of the list - try and use success/failure from first in preference
        for x in &self.xs {
            match x.coerce_item(configurable, ctx, value) {
                Ok(v) => return Ok(v),
                Err(e) => errs.push(e),
            }
        }
        Err(CoercionError::one_of_many(errs))
    }

    pub(crate) fn any_supports_concat(&self) -> bool {
        self.xs.iter().any(AttrType::supports_concat)
    }

    pub(crate) fn starlark_type(&self) -> String {
        format!("[{}]", self.xs.iter().map(|x| x.starlark_type()).join(", "))
    }
}
