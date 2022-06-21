/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anymap::any::Any;
use anymap::Map;
use gazebo::prelude::*;

use crate::incremental::introspection::EngineForIntrospection;
use crate::incremental::IncrementalComputeProperties;
use crate::IncrementalEngine;

/// A dynamically typed Map for DICE to map computations to their key, value
/// cache maps.
pub struct DiceMap {
    typed: Map<dyn Any + Sync + Send>,
    erased: Vec<Box<dyn EngineForIntrospection + Send + Sync + 'static>>,
}

impl DiceMap {
    pub(crate) fn new() -> Self {
        Self {
            typed: Map::new(),
            erased: Vec::new(),
        }
    }

    pub(crate) fn find_cache_opt<S>(&self) -> Option<Arc<IncrementalEngine<S>>>
    where
        S: IncrementalComputeProperties,
    {
        self.typed.get::<Arc<IncrementalEngine<S>>>().duped()
    }

    /// finds the computation cache for the given computation type
    pub(crate) fn find_cache<S>(
        &mut self,
        new: impl FnOnce() -> Arc<IncrementalEngine<S>>,
    ) -> Arc<IncrementalEngine<S>>
    where
        S: IncrementalComputeProperties,
    {
        if let Some(cache) = self.typed.get::<Arc<IncrementalEngine<S>>>() {
            cache.dupe()
        } else {
            let cache = new();
            self.typed.insert::<Arc<IncrementalEngine<S>>>(cache.dupe());
            self.erased.push(box cache.dupe());
            cache
        }
    }

    pub(crate) fn engines(&self) -> &[Box<dyn EngineForIntrospection + Send + Sync + 'static>] {
        self.erased.as_slice()
    }
}

#[cfg(test)]
mod tests {
    use async_trait::async_trait;
    use derive_more::Display;
    use gazebo::prelude::*;

    use crate::incremental::testing::IncrementalEngineExt;
    use crate::incremental::versions::MinorVersion;
    use crate::incremental::versions::VersionNumber;
    use crate::map::DiceMap;
    use crate::DetectCycles;
    use crate::Dice;
    use crate::DiceComputations;
    use crate::IncrementalEngine;
    use crate::Key;
    use crate::StoragePropertiesForKey;

    #[tokio::test]
    async fn test_find_caches() {
        #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
        #[display(fmt = "{:?}", self)]
        struct MyKey;
        #[derive(Clone, Dupe, Display, Debug, Eq, PartialEq)]
        #[display(fmt = "{:?}", self)]
        struct Bar;

        #[async_trait]
        impl Key for MyKey {
            type Value = Bar;

            async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
                panic!("value should be cached, not evaluated")
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                x == y
            }
        }

        let mut map = DiceMap::new();
        let dice = Dice::builder().build(DetectCycles::Enabled);
        let cache = map.find_cache(|| IncrementalEngine::new(StoragePropertiesForKey::new(&dice)));
        {
            cache.update(MyKey, VersionNumber::new(0), Bar);
            assert_eq!(
                cache
                    .get_cached(MyKey, VersionNumber::new(0), MinorVersion::testing_new(0))
                    .val(),
                &Bar
            )
        }
    }
}
