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

use std::marker::PhantomData;
use std::ops::Deref;

use starlark_derive::Trace;

use crate as starlark;
use crate::values::list::ListRef;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::UnpackValue;
use crate::values::Value;

/// Like `ValueOf`, but only validates item types; does not construct or store a
/// vec. Use `to_vec` to get a Vec.
#[derive(Debug, Trace)]
pub struct ListOf<'v, V: UnpackValue<'v>> {
    value: Value<'v>,
    phantom: PhantomData<V>,
}

impl<'v, V: UnpackValue<'v>> ListOf<'v, V> {
    /// Collect the list elements into a `Vec`.
    pub fn to_vec(&self) -> Vec<V> {
        ListRef::from_value(self.value)
            .expect("already validated as a list")
            .iter()
            .map(|v| V::unpack_value(v).expect("already validated value"))
            .collect()
    }
}

impl<'v, V: UnpackValue<'v>> StarlarkTypeRepr for ListOf<'v, V> {
    fn starlark_type_repr() -> String {
        Vec::<V>::starlark_type_repr()
    }
}

impl<'v, V: UnpackValue<'v>> UnpackValue<'v> for ListOf<'v, V> {
    fn expected() -> String {
        format!("list of {}", V::expected())
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        let list = ListRef::from_value(value)?;
        if list.iter().all(|v| V::unpack_value(v).is_some()) {
            Some(ListOf {
                value,
                phantom: PhantomData,
            })
        } else {
            None
        }
    }
}

impl<'v, V: UnpackValue<'v>> Deref for ListOf<'v, V> {
    type Target = Value<'v>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
