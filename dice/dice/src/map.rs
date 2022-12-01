/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use anymap::any::Any;
use anymap::Map;
use gazebo::prelude::*;

use crate::incremental::ErasedEngine;
use crate::incremental::IncrementalComputeProperties;
use crate::IncrementalEngine;

/// A dynamically typed Map for DICE to map computations to their key, value
/// cache maps.
#[derive(Allocative)]
pub(crate) struct DiceMap {
    #[allocative(skip)]
    typed: Map<dyn Any + Sync + Send>,
    erased: Vec<Arc<dyn ErasedEngine + Send + Sync + 'static>>,
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
            self.erased
                .push(cache.dupe() as Arc<dyn ErasedEngine + Send + Sync + 'static>);
            cache
        }
    }

    pub(crate) fn engines(&self) -> &[Arc<dyn ErasedEngine + Send + Sync + 'static>] {
        self.erased.as_slice()
    }

    pub(crate) fn key_count(&self) -> usize {
        self.erased
            .iter()
            .map(|e| e.introspect().len_for_introspection())
            .sum()
    }

    pub(crate) fn currently_running_key_count(&self) -> usize {
        self.erased
            .iter()
            .map(|e| e.introspect().currently_running_key_count())
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
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
        #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
        #[display(fmt = "{:?}", self)]
        struct MyKey;
        #[derive(Clone, Dupe, Display, Debug, Eq, PartialEq, Allocative)]
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
            cache.update_injected_value(MyKey, VersionNumber::new(0), Bar);
            assert_eq!(
                cache
                    .get_cached(MyKey, VersionNumber::new(0), MinorVersion::testing_new(0))
                    .val(),
                &Bar
            )
        }
    }
}
