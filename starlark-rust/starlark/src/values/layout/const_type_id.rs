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

use std::any::TypeId;

/// `TypeId` wrapper/provider until `const_type_id` feature is stabilized.
pub(crate) struct ConstTypeId {
    #[cfg(rust_nightly)]
    type_id: TypeId,
    #[cfg(not(rust_nightly))]
    type_id_fn: fn() -> TypeId,
}

impl ConstTypeId {
    pub(crate) const fn of<T: ?Sized + 'static>() -> ConstTypeId {
        ConstTypeId {
            #[cfg(rust_nightly)]
            type_id: TypeId::of::<T>(),
            #[cfg(not(rust_nightly))]
            type_id_fn: TypeId::of::<T>,
        }
    }

    #[inline]
    pub(crate) fn get(&self) -> TypeId {
        #[cfg(rust_nightly)]
        return self.type_id;
        #[cfg(not(rust_nightly))]
        return (self.type_id_fn)();
    }
}
