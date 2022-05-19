/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Cycle detection in DICE

use std::{
    fmt::Display,
    hash::{Hash, Hasher},
    str::FromStr,
    sync::Arc,
};

use gazebo::{cmp::PartialEqAny, prelude::*};
use indexmap::set::IndexSet;
use itertools::Itertools;
use thiserror::Error;

#[derive(Clone, Dupe, Copy, Debug)]
pub enum DetectCycles {
    Enabled,
    Disabled,
}

#[derive(Error, Debug)]
#[error("Invalid type of DetectCycles: `{0}`")]
pub struct InvalidType(String);

impl FromStr for DetectCycles {
    type Err = InvalidType;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "ENABLED" => Ok(DetectCycles::Enabled),
            "DISABLED" => Ok(DetectCycles::Disabled),
            _ => Err(InvalidType(s.to_owned())),
        }
    }
}

trait CycleKey: Display + Send + Sync {
    fn get_key_equality(&self) -> PartialEqAny;
    fn hash(&self, state: &mut dyn Hasher);
}

impl<T> CycleKey for T
where
    T: Display + Hash + Eq + Send + Sync + 'static,
{
    fn get_key_equality(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state)
    }
}

impl Hash for dyn CycleKey {
    fn hash<H: Hasher>(&self, mut state: &mut H) {
        self.hash(&mut state)
    }
}

impl PartialEq for dyn CycleKey {
    fn eq(&self, other: &Self) -> bool {
        self.get_key_equality() == other.get_key_equality()
    }
}

impl Eq for dyn CycleKey {}

pub(crate) struct CycleDetector {
    stack: IndexSet<Arc<dyn CycleKey>>,
}

impl CycleDetector {
    pub(crate) fn new() -> Self {
        Self {
            stack: IndexSet::new(),
        }
    }

    pub(crate) fn visit<K>(&self, key: &K) -> Self
    where
        K: Clone + Display + Eq + Hash + Send + Sync + 'static,
    {
        // quick and dirty cycle detection. we will have to make this more efficient
        // TODO(bobyf)
        let mut stack = self.stack.clone();
        if !stack.insert(Arc::new(key.clone())) {
            let cycle = stack.iter().join(",");

            panic!(
                "cycle detected: requested to compute key `{}`, but we already encountered it in the stack `[{}]`",
                key, cycle
            );
        }
        Self { stack }
    }
}

#[cfg(test)]
mod tests {
    use derive_more::Display;
    use gazebo::prelude::*;

    use crate::cycles::CycleDetector;

    #[derive(Clone, Dupe, Display, PartialEq, Eq, Hash)]
    struct K(usize);

    #[test]
    fn cycle_detection_when_no_cycles() {
        let detector = CycleDetector::new();
        let detector1 = detector.visit(&K(1));
        let detector12 = detector1.visit(&K(2));
        let detector123 = detector12.visit(&K(3));
        let _detector1234 = detector123.visit(&K(4));

        let detector13 = detector1.visit(&K(3));
        let _detector132 = detector13.visit(&K(2));
    }

    #[test]
    #[should_panic]
    fn cycle_detection_when_cycles() {
        let detector = CycleDetector::new();
        let detector = detector.visit(&K(1));
        let detector = detector.visit(&K(2));
        let detector = detector.visit(&K(3));
        let detector = detector.visit(&K(4));
        let _detector = detector.visit(&K(1));
    }
}
