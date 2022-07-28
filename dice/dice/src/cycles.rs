/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Cycle detection in DICE

use std::any::TypeId;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::str::FromStr;
use std::sync::Arc;

use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use indexmap::set::IndexSet;
use thiserror::Error;

use crate::DiceError;
use crate::DiceResult;
use crate::Key;

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

/// A `Key` that has been requested within Dice.
pub trait RequestedKey: Display + Debug + Send + Sync {
    fn get_key_equality(&self) -> PartialEqAny;
    fn hash(&self, state: &mut dyn Hasher);
    fn type_id(&self) -> TypeId;
}

impl dyn RequestedKey {
    pub fn is_key<K: Key>(&self) -> bool {
        TypeId::of::<K>() == self.type_id()
    }
}

impl<T> RequestedKey for T
where
    T: Display + Debug + Hash + Eq + Send + Sync + 'static,
{
    fn get_key_equality(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state)
    }

    fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}

impl Hash for dyn RequestedKey {
    fn hash<H: Hasher>(&self, mut state: &mut H) {
        self.hash(&mut state)
    }
}

impl PartialEq for dyn RequestedKey {
    fn eq(&self, other: &Self) -> bool {
        self.get_key_equality() == other.get_key_equality()
    }
}

impl Eq for dyn RequestedKey {}

pub(crate) struct CycleDetector {
    stack: IndexSet<Arc<dyn RequestedKey>>,
}

impl CycleDetector {
    pub(crate) fn new() -> Self {
        Self {
            stack: IndexSet::new(),
        }
    }

    pub(crate) fn visit<K>(&self, key: &K) -> DiceResult<Self>
    where
        K: Clone + Debug + Display + Eq + Hash + Send + Sync + 'static,
    {
        // quick and dirty cycle detection. we will have to make this more efficient
        // TODO(bobyf)
        let mut stack = self.stack.clone();
        if !stack.insert(Arc::new(key.clone())) {
            Err(DiceError::cycle(
                Arc::new(key.clone()),
                stack.iter().duped().collect(),
            ))
        } else {
            Ok(Self { stack })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use derive_more::Display;
    use gazebo::prelude::*;
    use indexmap::indexset;

    use crate::cycles::CycleDetector;
    use crate::DiceErrorImpl;
    use crate::RequestedKey;

    #[derive(Clone, Dupe, Display, Debug, PartialEq, Eq, Hash)]
    struct K(usize);

    #[test]
    fn cycle_detection_when_no_cycles() -> anyhow::Result<()> {
        let detector = CycleDetector::new();
        let detector1 = detector.visit(&K(1))?;
        let detector12 = detector1.visit(&K(2))?;
        let detector123 = detector12.visit(&K(3))?;
        let _detector1234 = detector123.visit(&K(4))?;

        let detector13 = detector1.visit(&K(3))?;
        let _detector132 = detector13.visit(&K(2))?;

        Ok(())
    }

    #[test]
    fn cycle_detection_when_cycles() -> anyhow::Result<()> {
        let detector = CycleDetector::new();
        let detector = detector.visit(&K(1))?;
        let detector = detector.visit(&K(2))?;
        let detector = detector.visit(&K(3))?;
        let detector = detector.visit(&K(4))?;

        match detector.visit(&K(1)) {
            Ok(_) => {
                panic!("should have cycle error")
            }
            Err(e) => match &*e.0 {
                DiceErrorImpl::Cycle {
                    cyclic_keys,
                    trigger,
                } => {
                    assert!(
                        (**trigger).get_key_equality() == K(1).get_key_equality(),
                        "expected trigger key to be `{}` but was `{}`",
                        K(1),
                        trigger
                    );
                    assert_eq!(
                        &*cyclic_keys,
                        &indexset![
                            Arc::new(K(1)) as Arc<dyn RequestedKey>,
                            Arc::new(K(2)) as Arc<dyn RequestedKey>,
                            Arc::new(K(3)) as Arc<dyn RequestedKey>,
                            Arc::new(K(4)) as Arc<dyn RequestedKey>
                        ]
                    )
                }
                _ => {
                    panic!("wrong error type")
                }
            },
        }

        Ok(())
    }
}
