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

use crate::codemap::CodeMap;
use crate::codemap::Span;

/// Error with location.
#[derive(Debug, derive_more::Display)]
pub struct EvalException(
    /// Error is guaranteed to have a diagnostic
    crate::Error,
);

impl EvalException {
    /// Error must be `Diagnostic`.
    #[cold]
    pub fn unchecked_new(error: crate::Error) -> EvalException {
        EvalException(error)
    }

    #[cold]
    pub fn into_error(self) -> crate::Error {
        self.0
    }

    #[cold]
    pub fn new(error: anyhow::Error, span: Span, codemap: &CodeMap) -> EvalException {
        EvalException(crate::Error::new_spanned(
            crate::ErrorKind::Other(error),
            span,
            codemap,
        ))
    }

    pub fn _testing_loc(err: &crate::Error) -> crate::codemap::ResolvedFileSpan {
        match err.span() {
            Some(d) => d.resolve(),
            None => panic!("Expected error with diagnostic, got {:#?}", err),
        }
    }
}
