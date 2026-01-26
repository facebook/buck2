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

//! Error types for pagable serialization/deserialization.

use thiserror::Error;

use crate::pagable::vtable_registry::DeserTypeId;

/// Errors that can occur during pagable serialization/deserialization.
#[derive(Debug, Error)]
pub enum PagableError {
    /// The type was not registered in the vtable registry.
    #[error("Type `{type_id}` was not registered for deserialization.")]
    TypeNotRegistered {
        /// The type identifier that was not found.
        type_id: DeserTypeId,
    },
}

impl From<PagableError> for crate::Error {
    fn from(e: PagableError) -> Self {
        crate::Error::new_kind(crate::ErrorKind::Other(anyhow::Error::new(e)))
    }
}
