/*
 * Copyright 2018 The Starlark in Rust Authors.
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

//! The AST of Starlark as [`AstModule`], along with a [`parse`](AstModule::parse) function.

pub use dialect::Dialect;
pub use dialect::DialectTypes;
pub use module::AstModule;
pub use parser::AstLoad;

pub(crate) mod ast;
pub(crate) mod cursors;
mod dialect;
#[cfg(test)]
mod grammar_tests;
pub(crate) mod grammar_util;
pub(crate) mod lexer;
#[cfg(test)]
mod lexer_tests;
pub(crate) mod module;
pub(crate) mod parser;
pub(crate) mod payload_map;
pub(crate) mod state;
#[cfg(test)]
mod testcases;
pub(crate) mod top_level_stmts;
pub(crate) mod type_expr;
pub(crate) mod uniplate;
pub(crate) mod validate;

#[allow(clippy::all)]
// Things we explicitly turn on need to be explicitly turned off
#[allow(clippy::inefficient_to_string)]
#[allow(clippy::trivially_copy_pass_by_ref)]
#[allow(clippy::too_many_arguments)]
#[allow(clippy::cloned_instead_of_copied)]
#[allow(unused_extern_crates)]
#[allow(unused_braces)]
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_use_dupe))]
mod grammar {
    include!(concat!(env!("OUT_DIR"), "/syntax/grammar.rs"));
}
