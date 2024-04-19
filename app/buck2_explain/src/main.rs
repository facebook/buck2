/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod explain_generated;
use flatbuffers::FlatBufferBuilder;

pub fn main() -> anyhow::Result<()> {
    use crate::explain_generated::explain::Bool;
    use crate::explain_generated::explain::BoolArgs;

    let mut builder = FlatBufferBuilder::with_capacity(1024);

    let _bool = Bool::create(&mut builder, &BoolArgs { value: true });

    Ok(())
}
