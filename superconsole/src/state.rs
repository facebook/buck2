/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::any::TypeId;
use std::any::{self};
use std::collections::HashMap;

use crate::Error;

/// Each [`Component`](crate::Component) can refer to any piece of state to render itself.
/// Conceptually, since Components are stateless, this state is used to render them fresh at each step.
/// States should be given a unique type - this can be used to retrieve them at draw time.
#[derive(Default)]
pub struct State<'a>(HashMap<TypeId, &'a dyn Any>);

impl<'a> State<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(HashMap::with_capacity(capacity))
    }

    pub fn insert<T: 'static>(&mut self, value: &'a T) {
        self.0.insert(TypeId::of::<T>(), value as &dyn Any);
    }

    pub fn get<T: 'static>(&'a self) -> anyhow::Result<&'a T> {
        self.0
            .get(&TypeId::of::<T>())
            .and_then(|v| v.downcast_ref())
            .ok_or_else(|| Error::MissingState(any::type_name::<T>().to_owned()).into())
    }
}

#[macro_export]
macro_rules! state {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$($crate::state!(@single $rest)),*]));

    ($($value:expr,)+) => { $crate::state!($($value),+) };
    ($($value:expr),*) => {
        {
            let _cap = $crate::state!(@count $($value),*);
            let mut _map = $crate::State::with_capacity(_cap);
            $(
                _map.insert($value);
            )*
            _map
        }
    };
}
