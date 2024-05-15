/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use itertools::Either;

use crate::impls::key::DiceKey;
use crate::HashSet;

/// It's very common for a parallel compute to record only a single dep and so we optimize
/// for that with this simple list.
#[derive(Allocative)]
pub(crate) enum SmallDepsList {
    None,
    One(DiceKey),
    #[allow(clippy::box_collection)]
    Many(Box<Vec<DiceKey>>),
}

impl SmallDepsList {
    pub(crate) fn new() -> SmallDepsList {
        SmallDepsList::None
    }

    pub(crate) fn insert(&mut self, k: DiceKey) {
        match self {
            SmallDepsList::None => *self = SmallDepsList::One(k),
            SmallDepsList::One(_) => {
                let v = std::mem::replace(self, SmallDepsList::None);
                let v = match v {
                    SmallDepsList::One(v) => v,
                    _ => unreachable!(),
                };
                *self = SmallDepsList::Many(Box::new(vec![v, k]));
            }
            SmallDepsList::Many(v) => v.push(k),
        }
    }

    pub(crate) fn reserve(&mut self, to_reserve: usize) -> &mut Vec<DiceKey> {
        match self {
            SmallDepsList::None => {
                *self = SmallDepsList::Many(Box::new(Vec::with_capacity(to_reserve)));
            }
            SmallDepsList::One(_) => {
                let v = std::mem::replace(self, SmallDepsList::None);
                let v = match v {
                    SmallDepsList::One(v) => v,
                    _ => unreachable!(),
                };
                let mut new = Vec::with_capacity(1 + to_reserve);
                new.push(v);
                *self = SmallDepsList::Many(Box::new(new));
            }
            SmallDepsList::Many(k) => k.reserve(to_reserve),
        }

        match self {
            SmallDepsList::Many(v) => &mut *v,
            _ => unreachable!(),
        }
    }

    pub(crate) fn len(&self) -> usize {
        match self {
            SmallDepsList::None => 0,
            SmallDepsList::One(_) => 1,
            SmallDepsList::Many(v) => v.len(),
        }
    }

    pub(crate) fn into_set(self) -> HashSet<DiceKey> {
        match self {
            SmallDepsList::None => HashSet::default(),
            SmallDepsList::One(v) => {
                let mut s = HashSet::default();
                s.insert(v);
                s
            }
            SmallDepsList::Many(v) => v.into_iter().collect(),
        }
    }

    pub(crate) fn iter_keys(&self) -> impl Iterator<Item = DiceKey> + '_ {
        match self {
            SmallDepsList::None => Either::Left(Option::<DiceKey>::None.into_iter()),
            SmallDepsList::One(v) => Either::Left(Option::<DiceKey>::Some(*v).into_iter()),
            SmallDepsList::Many(m) => Either::Right(m.iter().copied()),
        }
    }
}
