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

//! Defines a runtime Starlark value ([`Value`]) and traits for defining custom values ([`StarlarkValue`]).
//!
//! This module contains code for working with Starlark values:
//!
//! * Most code dealing with Starlark will use [`Value`], as it represents the fundamental values used in
//!   Starlark. When frozen, they become [`FrozenValue`].
//! * Values are garbage-collected, so a given [`Value`] lives on a [`Heap`].
//! * Rust values (e.g. [`String`], [`Vec`]) can be added to the [`Heap`] with [`AllocValue`],
//!   and deconstructed from a [`Value`] with [`UnpackValue`]
//!   (or specialised methods like [`unpack_str`](Value::unpack_str)).
//! * To define your own Rust data type that can live in a [`Value`] it must implement the [`StarlarkValue`]
//!   trait.
//! * All the nested modules represent the built-in Starlark values. These are all defined using [`StarlarkValue`],
//!   so may serve as interesting inspiration for writing your own values, in addition to occuring in Starlark programs.

pub use gazebo::any::AnyLifetime;
pub use gazebo::any::ProvidesStaticType;
pub use gazebo::coerce::Coerce;
pub use starlark_derive::starlark_attrs;
pub use starlark_derive::Freeze;
pub use starlark_derive::NoSerialize;
pub use starlark_derive::StarlarkAttrs;
pub use starlark_derive::Trace;

pub use crate::values::alloc_value::AllocFrozenValue;
pub use crate::values::alloc_value::AllocValue;
pub use crate::values::error::ValueError;
pub use crate::values::freeze::Freeze;
pub use crate::values::frozen_ref::FrozenRef;
pub use crate::values::layout::heap::heap_type::Freezer;
pub use crate::values::layout::heap::heap_type::FrozenHeap;
pub use crate::values::layout::heap::heap_type::FrozenHeapRef;
pub use crate::values::layout::heap::heap_type::Heap;
pub use crate::values::layout::heap::heap_type::Tracer;
pub use crate::values::layout::identity::ValueIdentity;
pub use crate::values::layout::static_string::constant_string;
pub use crate::values::layout::static_string::StarlarkStrNRepr;
pub use crate::values::layout::typed::string::FrozenStringValue;
pub use crate::values::layout::typed::string::StringValue;
pub use crate::values::layout::typed::string::StringValueLike;
pub use crate::values::layout::typed::FrozenValueTyped;
pub use crate::values::layout::typed::ValueTyped;
pub use crate::values::layout::value::FrozenValue;
pub use crate::values::layout::value::Value;
pub use crate::values::layout::value::ValueLike;
pub use crate::values::owned::OwnedFrozenValue;
pub use crate::values::owned::OwnedFrozenValueTyped;
pub use crate::values::trace::Trace;
pub use crate::values::traits::ComplexValue;
pub use crate::values::traits::StarlarkValue;
pub use crate::values::types::any;
pub use crate::values::types::array;
pub use crate::values::types::bool;
pub use crate::values::types::dict;
pub use crate::values::types::enumeration;
pub use crate::values::types::float;
pub use crate::values::types::function;
pub use crate::values::types::int;
pub use crate::values::types::list;
pub use crate::values::types::none;
pub use crate::values::types::range;
pub use crate::values::types::record;
pub use crate::values::types::regex;
pub use crate::values::types::string;
pub use crate::values::types::structs;
pub use crate::values::types::tuple;
pub use crate::values::unpack::UnpackValue;
pub use crate::values::unpack::ValueOf;

#[macro_use]
mod comparison;

// Submodules
mod alloc_value;
pub(crate) mod basic;
pub(crate) mod display;
pub mod docs;
pub(crate) mod error;
mod freeze;
pub(crate) mod frozen_ref;
mod index;
pub(crate) mod iter;
pub(crate) mod layout;
pub(crate) mod num;
mod owned;
pub(crate) mod recursive_repr_or_json_guard;
mod stack_guard;
mod trace;
mod traits;
pub(crate) mod types;
pub(crate) mod typing;
mod unpack;
