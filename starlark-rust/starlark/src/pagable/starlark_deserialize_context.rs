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

//! Implementation of StarlarkDeserializeContext.

use pagable::PagableDeserializer;

use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;

/// Concrete implementation of StarlarkDeserializeContext.
///
/// Wraps a PagableDeserializer and provides the context needed for
/// deserializing Starlark values.
pub struct StarlarkDeserializerImpl<'a, 'de> {
    pagable: &'a mut dyn PagableDeserializer<'de>,
}

impl<'a, 'de> StarlarkDeserializerImpl<'a, 'de> {
    /// Create a new deserializer context wrapping the given pagable deserializer.
    pub fn new(pagable: &'a mut dyn PagableDeserializer<'de>) -> Self {
        Self { pagable }
    }
}

impl<'de> StarlarkDeserializeContext<'de> for StarlarkDeserializerImpl<'_, 'de> {
    fn pagable(&mut self) -> &mut dyn PagableDeserializer<'de> {
        self.pagable
    }
}
