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

/// Create a [`FrozenStringValue`](crate::values::FrozenStringValue).
#[macro_export]
macro_rules! const_frozen_string {
    ($s:expr) => {{
        $crate::values::constant_string($s).unwrap_or_else(|| {
            // `N <= 1` is unreachable here because it was handled by `constant_string`,
            // but we still have to put something in `static`.
            // `StarlarkStrNRepr::new` fails if `N <= 1`,
            // so for `N <= 1` we put dummy string there.
            const UNREACHABLE: bool = $s.len() <= 1;
            const N: usize = if UNREACHABLE { 2 } else { $s.len() };
            static X: $crate::values::StarlarkStrNRepr<N> =
                $crate::values::StarlarkStrNRepr::new(if UNREACHABLE { "xx" } else { $s });
            if UNREACHABLE {
                unreachable!()
            } else {
                X.erase()
            }
        })
    }};
}

#[cfg(test)]
mod tests {
    use crate::values::{FrozenHeap, Heap};

    #[test]
    fn test_const_frozen_string() {
        assert!(
            const_frozen_string!("a")
                .to_value()
                .ptr_eq(const_frozen_string!("a").to_value())
        );

        let heap = Heap::new();
        assert!(
            const_frozen_string!("a")
                .to_value()
                .ptr_eq(heap.alloc_str("a").to_value())
        );

        let frozen_heap = FrozenHeap::new();
        assert!(
            const_frozen_string!("a")
                .to_value()
                .ptr_eq(frozen_heap.alloc_str("a").to_value())
        );
    }
}
