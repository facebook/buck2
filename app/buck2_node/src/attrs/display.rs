/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use crate::attrs::fmt_context::AttrFmtContext;

/// Like `Display` but has package context.
pub trait AttrDisplayWithContext {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

pub trait AttrDisplayWithContextExt: AttrDisplayWithContext {
    fn as_display<'a>(
        &'a self,
        ctx: &'a AttrFmtContext,
    ) -> AttrDisplayWithContextAsDisplay<'a, Self> {
        AttrDisplayWithContextAsDisplay { ctx, value: self }
    }

    fn as_display_no_ctx(&self) -> AttrDisplayWithContextAsDisplay<Self> {
        self.as_display(&AttrFmtContext::NO_CONTEXT)
    }
}

impl<T: AttrDisplayWithContext + ?Sized> AttrDisplayWithContextExt for T {}

pub struct AttrDisplayWithContextAsDisplay<'a, A: ?Sized> {
    ctx: &'a AttrFmtContext,
    value: &'a A,
}

impl<'a, A: AttrDisplayWithContext + ?Sized> Display for AttrDisplayWithContextAsDisplay<'a, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(self.ctx, f)
    }
}
