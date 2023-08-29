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
use crate::diagnostic::Diagnostic;

/// Error with location.
#[derive(Debug, derive_more::Display)]
pub struct EvalException(
    /// Error is `Diagnostic`, but stored as `anyhow::Error` for smaller size.
    anyhow::Error,
);

impl EvalException {
    /// Error must be `Diagnostic`.
    #[cold]
    pub fn unchecked_new(error: anyhow::Error) -> EvalException {
        EvalException(error)
    }

    #[cold]
    pub fn into_anyhow(self) -> anyhow::Error {
        self.0
    }

    #[cold]
    pub fn new(error: anyhow::Error, span: Span, codemap: &CodeMap) -> EvalException {
        EvalException(Diagnostic::new(error, span, codemap))
    }

    pub fn _testing_loc(mut err: &anyhow::Error) -> crate::codemap::ResolvedFileSpan {
        if let Some(eval_exc) = err.downcast_ref::<EvalException>() {
            err = &eval_exc.0;
        }
        match err.downcast_ref::<Diagnostic>() {
            Some(d) => d.span.as_ref().unwrap().resolve(),
            None => panic!("Expected Diagnostic, got {:#?}", err),
        }
    }
}
