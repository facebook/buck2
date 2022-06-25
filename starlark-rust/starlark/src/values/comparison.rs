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

use std::cmp::Ordering;
use std::hash::Hash;

use gazebo::prelude::*;
use itertools::Itertools;

use crate::collections::SmallMap;
use crate::small_map::Equivalent;

pub fn equals_slice<E, X1, X2>(
    xs: &[X1],
    ys: &[X2],
    f: impl Fn(&X1, &X2) -> Result<bool, E>,
) -> Result<bool, E> {
    Ok(eq_chain! {
        xs.len() == ys.len(),
        xs.iter().try_eq_by(ys, f)?,
    })
}

pub fn equals_small_map<E, K1: Eq, K2: Eq, V1, V2>(
    x: &SmallMap<K1, V1>,
    y: &SmallMap<K2, V2>,
    f: impl Fn(&V1, &V2) -> Result<bool, E>,
) -> Result<bool, E>
where
    K1: Equivalent<K2>,
{
    Ok(eq_chain! {
        x.len() == y.len(),
        x.iter_hashed().try_all(
            |(xk, xv)| y.get_hashed(xk).map_or(Ok(false), |yv| f(xv, yv))
        )?,
    })
}

pub fn compare_slice<E, X1, X2>(
    xs: &[X1],
    ys: &[X2],
    f: impl Fn(&X1, &X2) -> Result<Ordering, E>,
) -> Result<Ordering, E> {
    Ok(cmp_chain! {
        xs.len().cmp(&ys.len()),
        xs.iter().try_cmp_by(ys, f)?,
    })
}

pub fn compare_small_map<E, K, K2: Ord + Hash, V1, V2>(
    x: &SmallMap<K, V1>,
    y: &SmallMap<K, V2>,
    key: impl Fn(&K) -> K2,
    f: impl Fn(&V1, &V2) -> Result<Ordering, E>,
) -> Result<Ordering, E> {
    Ok(cmp_chain! {
        x.len().cmp(&y.len()),
        x.iter()
            .sorted_by_key(|(k, _)| key(k))
            .try_cmp_by(
                y.iter().sorted_by_key(|(k, _)| key(k)),
                |(xk, xv), (yk, yv)| Ok(cmp_chain! { key(xk).cmp(&key(yk)), f(xv, yv)? })
            )?
    })
}
