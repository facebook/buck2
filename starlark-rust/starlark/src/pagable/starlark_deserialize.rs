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

//! Trait for Starlark values that can be deserialized from pagable format.

use pagable::PagableDeserializer;

/// Trait for Starlark values that can be deserialized.
///
/// This trait is used during arena deserialization to reconstruct each value
/// in the heap. The deserialization is done via the vtable, allowing for
/// dynamic dispatch during heap reconstruction.
pub trait StarlarkDeserialize: Sized {
    /// Deserialize this value using the provided context.
    ///
    /// The context provides access to the underlying pagable deserializer.
    fn starlark_deserialize(ctx: &mut dyn StarlarkDeserializeContext<'_>) -> crate::Result<Self>;
}

/// Context for deserialization - wraps PagableDeserializer.
///
/// This trait provides the interface that StarlarkDeserialize implementations
/// use to deserialize their data. It abstracts over the underlying deserialization
/// mechanism.
pub trait StarlarkDeserializeContext<'de> {
    /// Get mutable access to the underlying pagable deserializer.
    fn pagable(&mut self) -> &mut dyn PagableDeserializer<'de>;
}
