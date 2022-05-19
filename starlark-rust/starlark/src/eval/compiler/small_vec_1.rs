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

//! Small `Vec`.

use std::{fmt::Debug, mem, slice};

/// A small vector.
#[derive(Clone)]
pub(crate) enum SmallVec1<T> {
    Empty,
    One(T),
    Many(Vec<T>),
}

impl<T: Debug> Debug for SmallVec1<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.as_slice()).finish()
    }
}

impl<T> SmallVec1<T> {
    pub(crate) fn as_slice(&self) -> &[T] {
        match self {
            SmallVec1::Empty => &[],
            SmallVec1::One(x) => slice::from_ref(x),
            SmallVec1::Many(xs) => xs,
        }
    }

    pub(crate) fn extend(&mut self, that: SmallVec1<T>) {
        *self = match (mem::replace(self, SmallVec1::Empty), that) {
            (SmallVec1::Empty, right) => right,
            (left, SmallVec1::Empty) => left,
            (SmallVec1::One(left), SmallVec1::One(right)) => SmallVec1::Many(vec![left, right]),
            (SmallVec1::One(left), SmallVec1::Many(mut right)) => {
                right.insert(0, left);
                SmallVec1::Many(right)
            }
            (SmallVec1::Many(mut left), SmallVec1::One(right)) => {
                left.push(right);
                SmallVec1::Many(left)
            }
            (SmallVec1::Many(mut left), SmallVec1::Many(right)) => {
                left.extend(right);
                SmallVec1::Many(left)
            }
        }
    }
}
