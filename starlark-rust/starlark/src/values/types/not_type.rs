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

//! Marker for a [`StarlarkValue`] implementation which does not represent a type.
//! Used in proc macro, not needed to be used directly, should go away eventually,
//! when `StarlarkValue` implementations are required to provide `Canonical` type.

use allocative::Allocative;
use starlark_derive::starlark_value;
use starlark_derive::NoSerialize;
use starlark_derive::ProvidesStaticType;

use crate as starlark;
use crate::values::StarlarkValue;

/// Marker for a [`StarlarkValue`] implementation which is not a type.
#[derive(
    ProvidesStaticType,
    Debug,
    derive_more::Display,
    NoSerialize,
    Allocative
)]
#[display(fmt = "not_type")]
pub struct NotType;

#[starlark_value(type = "not_type")]
impl<'v> StarlarkValue<'v> for NotType {}
