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

//! A type [`StarlarkAny`] which can cheaply wrap any Rust value into a [`Value`].
//!
//! This is intended to be a low cost way to quickly wrap Rust types without much boilerplate.
//! For more advanced uses you should define an instance of [`StarlarkValue`].
//!
//! To use this type, usually you will return a [`StarlarkAny`] from a module function,
//! and consume it in another. As an example, we can cheaply wrap the
//! [`Duration`](std::time::Duration) type.
//!
//! ```
//! #![feature(box_syntax)]
//! #[macro_use]
//! extern crate starlark;
//! # fn main() {
//! use starlark::assert::Assert;
//! use starlark::environment::GlobalsBuilder;
//! use starlark::values::Value;
//! use starlark::values::any::StarlarkAny;
//! use std::fmt;
//! use std::fmt::Display;
//! use std::time::Instant;
//!
//! #[derive(Debug)]
//! struct MyInstant(Instant);
//!
//! impl Display for MyInstant {
//!     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//!         write!(f, "{:?}", self)
//!     }
//! }
//!
//! #[starlark_module]
//! fn globals(builder: &mut GlobalsBuilder) {
//!     fn start() -> anyhow::Result<StarlarkAny<MyInstant>> {
//!         Ok(StarlarkAny::new(MyInstant(Instant::now())))
//!     }
//!
//!     fn elapsed(x: Value) -> anyhow::Result<String> {
//!         Ok(StarlarkAny::<MyInstant>::get(x).unwrap().0.elapsed().as_secs_f64().to_string())
//!     }
//! }
//!
//! let mut a = Assert::new();
//! a.globals_add(globals);
//! a.pass(r#"
//! duration = start()
//! y = 100
//! for x in range(100):
//!     y += x
//! print(elapsed(duration))
//! "#);
//! # }
//! ```

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use gazebo::any::ProvidesStaticType;

use crate as starlark;
use crate::values::AllocValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Value;
use crate::values::ValueLike;

/// A type that can be passed around as a Starlark [`Value`], but in most
/// ways is uninteresting/opaque to Starlark. Constructed with
/// [`new`](StarlarkAny::new) and decomposed with [`get`](StarlarkAny::get).
#[derive(ProvidesStaticType, NoSerialize, Allocative)]
#[allocative(bound = "")]
pub struct StarlarkAny<T: Debug + Display + Send + Sync + 'static>(
    #[allocative(skip)] // TODO(nga): do not skip.
    pub  T,
);

impl<'v, T: Debug + Display + Send + Sync + 'static> StarlarkValue<'v> for StarlarkAny<T> {
    starlark_type!("any");
}

impl<'v, T: Debug + Display + Send + Sync + 'static> AllocValue<'v> for StarlarkAny<T> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

impl<T: Debug + Display + Send + Sync + 'static> Debug for StarlarkAny<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl<T: Debug + Display + Send + Sync + 'static> Display for StarlarkAny<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<T: Debug + Display + Send + Sync + 'static> StarlarkAny<T> {
    /// Create a new [`StarlarkAny`] value. Such a value can be allocated on a heap with
    /// `heap.alloc(StarlarkAny::new(x))`.
    pub fn new(x: T) -> Self {
        StarlarkAny(x)
    }

    /// Extract from a [`Value`] that contains a [`StarlarkAny`] underneath. Returns [`None`] if
    /// the value does not match the expected type.
    pub fn get<'v>(x: Value<'v>) -> Option<&'v T> {
        let x: &StarlarkAny<T> = x.downcast_ref()?;
        Some(&x.0)
    }
}
