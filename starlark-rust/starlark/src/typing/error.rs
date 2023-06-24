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

use std::fmt::Display;

use crate::codemap::CodeMap;
use crate::codemap::Span;
use crate::eval::compiler::EvalException;

/// Internal error, bug in the typechecker.
pub(crate) struct InternalError(EvalException);

impl InternalError {
    #[cold]
    pub(crate) fn msg(message: impl Display, span: Span, codemap: &CodeMap) -> InternalError {
        InternalError(EvalException::new(
            anyhow::anyhow!("{} (internal error)", message),
            span,
            codemap,
        ))
    }

    #[cold]
    pub(crate) fn into_anyhow(self) -> anyhow::Error {
        self.0.into_anyhow()
    }
}

/// Errors used in typechecker API. Error has a span.
pub struct TypingError(EvalException);

impl TypingError {
    #[cold]
    pub(crate) fn msg(message: impl Display, span: Span, codemap: &CodeMap) -> TypingError {
        TypingError(EvalException::new(
            anyhow::Error::msg(message.to_string()),
            span,
            codemap,
        ))
    }

    #[cold]
    pub(crate) fn new(error: anyhow::Error, span: Span, codemap: &CodeMap) -> TypingError {
        TypingError(EvalException::new(error, span, codemap))
    }

    #[cold]
    pub(crate) fn into_anyhow(self) -> anyhow::Error {
        self.0.into_anyhow()
    }

    #[cold]
    pub(crate) fn _into_eval_exception(self) -> EvalException {
        self.0
    }
}
