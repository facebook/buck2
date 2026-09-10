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
use crate::values::FreezeBranded;
use crate::values::Value;

#[derive(FreezeBranded)]
#[freeze_branded(frozen_only)]
#[allow(dead_code)]
struct FrozenOnly<'v> {
    value: Value<'v>,
}

#[derive(FreezeBranded)]
#[freeze_branded(frozen_only)]
#[allow(dead_code)]
enum FrozenOnlyEnum<'v> {
    A(Value<'v>),
    B,
}

/// `Frozen<'fv>` is the type at `'fv`, which is what a handle field's freeze goes through.
#[test]
fn test_frozen_is_the_type_at_the_brand() {
    fn rebrand<'a, 'b>(x: <FrozenOnly<'a> as FreezeBranded<'a>>::Frozen<'b>) -> FrozenOnly<'b> {
        x
    }
    fn rebrand_enum<'a, 'b>(
        x: <FrozenOnlyEnum<'a> as FreezeBranded<'a>>::Frozen<'b>,
    ) -> FrozenOnlyEnum<'b> {
        x
    }
    let _ = rebrand;
    let _ = rebrand_enum;
}
