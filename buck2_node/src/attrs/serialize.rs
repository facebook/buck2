/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use serde::Serialize;
use serde::Serializer;

use crate::attrs::fmt_context::AttrFmtContext;

pub trait AttrSerializeWithContext {
    fn serialize_with_ctx<S>(&self, ctx: &AttrFmtContext, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer;
}

pub trait AttrSerializeWithContextExt: AttrSerializeWithContext {
    fn as_serialize<'a>(
        &'a self,
        ctx: &'a AttrFmtContext,
    ) -> AttrSerializeWithContextAsSerialize<'a, Self> {
        AttrSerializeWithContextAsSerialize { ctx, value: self }
    }
}

impl<T: AttrSerializeWithContext + ?Sized> AttrSerializeWithContextExt for T {}

pub struct AttrSerializeWithContextAsSerialize<'a, A: ?Sized> {
    ctx: &'a AttrFmtContext,
    value: &'a A,
}

impl<A: AttrSerializeWithContext + ?Sized> Serialize
    for AttrSerializeWithContextAsSerialize<'_, A>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.value.serialize_with_ctx(self.ctx, serializer)
    }
}
