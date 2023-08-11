/*
 * Copyright 2018 The Starlark in Rust Authors.
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

use std::marker::PhantomData;

use crate::typing::Ty;
use crate::values::type_repr::StarlarkTypeRepr;

enum NonInstantiable {}

/// `StarlarkTypeRepr` for iterable types.
pub struct StarlarkIter<T: StarlarkTypeRepr>(PhantomData<T>, NonInstantiable);

impl<T: StarlarkTypeRepr> StarlarkTypeRepr for StarlarkIter<T> {
    fn starlark_type_repr() -> Ty {
        Ty::iter(T::starlark_type_repr())
    }
}
