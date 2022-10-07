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

use crate::iter::def_double_ended_iter;
use crate::iter::def_iter;
use crate::small_map;

pub struct Iter<'a, T> {
    pub(crate) iter: small_map::Iter<'a, T, ()>,
}

pub struct IntoIter<T> {
    pub(crate) iter: small_map::IntoIter<T, ()>,
}

impl<'a, T> Clone for Iter<'a, T> {
    fn clone(&self) -> Iter<'a, T> {
        Iter {
            iter: self.iter.clone(),
        }
    }
}

impl<'a, T> Iter<'a, T> {
    #[inline]
    fn map((k, ()): (&'a T, &'a ())) -> &'a T {
        k
    }
}

impl<T> IntoIter<T> {
    #[inline]
    fn map((k, ()): (T, ())) -> T {
        k
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    def_iter!();
}

impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    def_double_ended_iter!();
}

impl<'a, T> ExactSizeIterator for Iter<'a, T> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    def_iter!();
}

impl<T> DoubleEndedIterator for IntoIter<T> {
    def_double_ended_iter!();
}

impl<T> ExactSizeIterator for IntoIter<T> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}
