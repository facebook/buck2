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

//! Generic interner for starlark strings.

use hashbrown::raw::RawTable;

use crate::collections::Hashed;
use crate::values::FrozenStringValue;

/// `[FrozenStringValue]` interner.
#[derive(Default)]
pub(crate) struct FrozenStringInterner {
    map: RawTable<FrozenStringValue>,
}

impl FrozenStringInterner {
    pub(crate) fn intern(
        &mut self,
        s: Hashed<&str>,
        alloc: impl FnOnce() -> FrozenStringValue,
    ) -> FrozenStringValue {
        match self
            .map
            .get(s.hash().promote(), |x| s == x.get_hashed_str())
        {
            Some(frozen_string) => *frozen_string,
            None => {
                let frozen_string = alloc();
                self.map.insert(s.hash().promote(), frozen_string, |x| {
                    x.get_hash().promote()
                });
                frozen_string
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::collections::Hashed;
    use crate::values::string::intern::interner::FrozenStringInterner;
    use crate::values::FrozenHeap;

    #[test]
    fn test_intern() {
        let heap1 = FrozenHeap::new();
        let heap2 = FrozenHeap::new();
        let mut intern = FrozenStringInterner::default();

        let xx1 = intern.intern(Hashed::new("xx"), || heap1.alloc_str("xx"));
        let xx2 = intern.intern(Hashed::new("xx"), || heap2.alloc_str("xx"));
        assert!(xx1.to_value().ptr_eq(xx2.to_value()));
    }
}
