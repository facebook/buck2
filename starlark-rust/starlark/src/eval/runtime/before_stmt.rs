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

//! Configuration of `BeforeStmt` instrumentation of bytecode.

use crate::{codemap::FileSpanRef, eval::Evaluator};

/// Configuration of `BeforeStmt` instrumentation of bytecode.
#[derive(Default)]
pub(crate) struct BeforeStmt<'a> {
    /// Functions to run before each statement.
    pub(crate) before_stmt: Vec<&'a dyn for<'v> Fn(FileSpanRef, &mut Evaluator<'v, 'a>)>,
    /// Explicitly request generation of `BeforeStmt` instructions
    /// even if no `before_stmt` functions are registered.
    /// This is needed when compiling dependencies of a file to be profiled.
    pub(crate) instrument: bool,
}

impl<'a> BeforeStmt<'a> {
    pub(crate) fn enabled(&self) -> bool {
        self.instrument || !self.before_stmt.is_empty()
    }
}
