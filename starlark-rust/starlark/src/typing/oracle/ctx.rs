/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use dupe::Dupe;

use crate::codemap::CodeMap;
use crate::codemap::Span;
use crate::typing::error::TypingError;
use crate::typing::Ty;
use crate::typing::TyFunction;
use crate::typing::TyName;
use crate::typing::TypingAttr;
use crate::typing::TypingOracle;

#[derive(Debug, thiserror::Error)]
enum TypingOracleCtxError {
    #[error("Expected type `{require}` but got `{got}`")]
    IncompatibleType { got: String, require: String },
}

/// Oracle reference with utility methods.
///
/// This type is stateless.
#[derive(Clone, Copy, Dupe)]
pub struct TypingOracleCtx<'a> {
    pub(crate) oracle: &'a dyn TypingOracle,
    pub(crate) codemap: &'a CodeMap,
}

impl<'a> TypingOracle for TypingOracleCtx<'a> {
    fn attribute(&self, ty: &Ty, attr: TypingAttr) -> Option<Result<Ty, ()>> {
        self.oracle.attribute(ty, attr)
    }

    fn as_function(&self, ty: &TyName) -> Option<Result<TyFunction, ()>> {
        self.oracle.as_function(ty)
    }

    fn subtype(&self, require: &TyName, got: &TyName) -> bool {
        self.oracle.subtype(require, got)
    }
}

impl<'a> TypingOracleCtx<'a> {
    pub(crate) fn mk_error(&self, span: Span, err: impl Into<anyhow::Error>) -> TypingError {
        TypingError::new(err.into(), span, self.codemap)
    }

    pub(crate) fn validate_type(
        &self,
        got: &Ty,
        require: &Ty,
        span: Span,
    ) -> Result<(), TypingError> {
        if !got.intersects(require, Some(*self)) {
            Err(self.mk_error(
                span,
                TypingOracleCtxError::IncompatibleType {
                    got: got.to_string(),
                    require: require.to_string(),
                },
            ))
        } else {
            Ok(())
        }
    }
}
