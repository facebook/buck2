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

//! Support for serialization and deserialization of Starlark values (pagable).
//!
//! This module provides the infrastructure needed to serialize and deserialize
//! Starlark heaps and values.
//!
//! ## Challenge: Type Inventory Registration
//!
//! During deserialization, we need to know which deserialization function to use
//! for each Starlark value type. This is solved by using the `inventory` crate
//! to register a mapping from type identifiers to vtables at compile time.
//!
//! The type identifier is `std::any::type_name<T>()` for the Rust type that
//! defines the Starlark value.

#[cfg(feature = "pagable")]
pub(crate) mod error;
#[cfg(feature = "pagable")]
pub(crate) mod vtable_registry;

pub(crate) mod vtable_register;
