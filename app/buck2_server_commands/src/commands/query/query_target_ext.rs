/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Formatter;

use buck2_query::query::environment::AttrFmtOptions;
use buck2_query::query::environment::QueryTarget;
use dupe::Dupe;

/// Extensions of `QueryTarget` needed in query commands.
pub(crate) trait QueryCommandTarget: QueryTarget {
    fn call_stack(&self) -> Option<String>;

    #[allow(dead_code)]
    fn attr_to_string_alternate(&self, _options: AttrFmtOptions, attr: &Self::Attr<'_>) -> String;

    fn attr_serialize<S: serde::Serializer>(
        &self,
        attr: &Self::Attr<'_>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>;

    fn attr_fmt(
        &self,
        fmt: &mut std::fmt::Formatter<'_>,
        _options: AttrFmtOptions,
        attr: &Self::Attr<'_>,
    ) -> std::fmt::Result;

    fn attr_display<'a, 'b>(
        &'a self,
        attr: &'a Self::Attr<'b>,
        options: AttrFmtOptions,
    ) -> AttrDisplay<'a, 'b, Self> {
        AttrDisplay(self, attr, options)
    }
}

pub struct AttrDisplay<'a, 'b, T: QueryCommandTarget>(&'a T, &'a T::Attr<'b>, AttrFmtOptions);
impl<'a, 'b, T: QueryCommandTarget> std::fmt::Display for AttrDisplay<'a, 'b, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.attr_fmt(f, self.2.dupe(), self.1)
    }
}
