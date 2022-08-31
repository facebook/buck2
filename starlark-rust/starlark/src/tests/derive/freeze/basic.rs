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
use crate::values::Freezer;
use crate::values::FrozenHeap;

#[derive(Freeze)]
struct TestStruct {
    s: String,
    #[freeze(identity)]
    s2: String,
}

#[derive(Freeze)]
struct TestAnonStruct(String, #[freeze(identity)] String);

#[test]
fn test_struct() -> anyhow::Result<()> {
    let t = TestStruct {
        s: "test".to_owned(),
        s2: "test2".to_owned(),
    };
    let freezer = Freezer::new(FrozenHeap::new());
    t.freeze(&freezer)?;
    Ok(())
}

#[test]
fn test_anon_struct() -> anyhow::Result<()> {
    let t = TestAnonStruct("test".to_owned(), "test2".to_owned());
    let freezer = Freezer::new(FrozenHeap::new());
    t.freeze(&freezer)?;
    Ok(())
}
