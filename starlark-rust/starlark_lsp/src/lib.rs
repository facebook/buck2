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

//! The server that allows IDEs to evaluate and interpret starlark code according
//! to the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/).

// Lints that don't necessarily make sense
#[allow(clippy::needless_lifetimes)]
#[allow(clippy::type_complexity)]
mod bind;
pub mod completion;
mod definition;
pub(crate) mod docs;
pub mod error;
mod exported;
pub(crate) mod inspect;
pub(crate) mod loaded;
pub mod server;
mod symbols;
#[cfg(test)]
mod test;
