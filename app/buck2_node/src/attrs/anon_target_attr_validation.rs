/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum AnonRuleAttrError {
    #[error("Attr type `{0}` is not supported for anon rules")]
    NotSupported(String),
}

/// Basic validation that anon target attr types defined in the anon_rule are supported.
pub trait AnonRuleAttrValidation {
    fn validate_for_anon_rule(&self) -> buck2_error::Result<()>;
}

impl AnonRuleAttrValidation for AttrType {
    fn validate_for_anon_rule(&self) -> buck2_error::Result<()> {
        match &self.0.inner {
            AttrTypeInner::Any(_) => Ok(()),
            AttrTypeInner::Bool(_) => Ok(()),
            AttrTypeInner::Int(_) => Ok(()),
            AttrTypeInner::Dict(x) => {
                x.key.validate_for_anon_rule()?;
                x.value.validate_for_anon_rule()
            }
            AttrTypeInner::List(x) => x.inner.validate_for_anon_rule(),
            AttrTypeInner::Tuple(x) => {
                for o in x.xs.iter() {
                    o.validate_for_anon_rule()?
                }
                Ok(())
            }
            AttrTypeInner::OneOf(x) => {
                for o in x.xs.iter() {
                    o.validate_for_anon_rule()?
                }
                Ok(())
            }
            AttrTypeInner::Option(x) => x.inner.validate_for_anon_rule(),
            AttrTypeInner::String(_) => Ok(()),
            AttrTypeInner::Enum(_) => Ok(()),
            AttrTypeInner::Dep(_) => Ok(()),
            AttrTypeInner::Source(_) => Ok(()),
            AttrTypeInner::Arg(_) => Ok(()),
            AttrTypeInner::Label(_) => Ok(()),
            _ => Err(AnonRuleAttrError::NotSupported(self.to_string()).into()),
        }
    }
}
