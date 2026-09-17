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

use crate as starlark;
use crate::values::Freeze;
use crate::values::Value;

#[derive(Freeze)]
#[freeze(frozen_only)]
#[allow(dead_code)]
struct FrozenOnly<'v> {
    value: Value<'v>,
}

#[derive(Freeze)]
#[freeze(frozen_only)]
#[allow(dead_code)]
enum FrozenOnlyEnum<'v> {
    A(Value<'v>),
    B,
}

/// A type that is never frozen has no `Freeze` impl.
#[allow(dead_code)]
struct NotFreezable(u32);

/// `frozen_only` asks nothing of the fields: none of them is frozen.
#[derive(Freeze)]
#[freeze(frozen_only)]
#[allow(dead_code)]
struct FrozenOnlyFields<'v> {
    value: Value<'v>,
    plain: NotFreezable,
}

/// Nor of the type parameters, which pass through unchanged.
#[derive(Freeze)]
#[freeze(frozen_only)]
#[allow(dead_code)]
struct FrozenOnlyGeneric<'v, T> {
    value: Value<'v>,
    extra: T,
}

/// `Frozen<'fv>` is the type at `'fv`, which is what a handle field's freeze goes through.
#[test]
fn test_frozen_is_the_type_at_the_brand() {
    fn rebrand_generic<'a, 'b>(
        x: <FrozenOnlyGeneric<'a, NotFreezable> as Freeze<'a>>::Frozen<'b>,
    ) -> FrozenOnlyGeneric<'b, NotFreezable> {
        x
    }
    let _ = rebrand_generic;
    fn rebrand<'a, 'b>(x: <FrozenOnly<'a> as Freeze<'a>>::Frozen<'b>) -> FrozenOnly<'b> {
        x
    }
    fn rebrand_enum<'a, 'b>(
        x: <FrozenOnlyEnum<'a> as Freeze<'a>>::Frozen<'b>,
    ) -> FrozenOnlyEnum<'b> {
        x
    }
    let _ = rebrand;
    let _ = rebrand_enum;
}
