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

pub use gazebo::{
    any::{AnyLifetime, ProvidesStaticType},
    coerce::Coerce,
};
pub use starlark_derive::{starlark_attrs, Freeze, NoSerialize, StarlarkAttrs, Trace};

pub use crate::values::{
    alloc_value::{AllocFrozenValue, AllocValue},
    error::ValueError,
    freeze::Freeze,
    frozen_ref::FrozenRef,
    layout::{
        heap::{Freezer, FrozenHeap, FrozenHeapRef, Heap, Tracer},
        identity::ValueIdentity,
        static_string::{constant_string, StarlarkStrNRepr},
        typed::{
            string::{FrozenStringValue, StringValue, StringValueLike},
            FrozenValueTyped, ValueTyped,
        },
        value::{FrozenValue, Value, ValueLike},
    },
    owned::{OwnedFrozenValue, OwnedFrozenValueTyped},
    trace::Trace,
    traits::{ComplexValue, StarlarkValue},
    types::{
        any, array, bool, dict, enumeration, float, function, int, list, none, range, record,
        regex, string, structs, tuple,
    },
    unpack::{UnpackValue, ValueOf},
};

#[macro_use]
mod comparison;

// Submodules
mod alloc_value;
pub(crate) mod basic;
pub mod display;
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
