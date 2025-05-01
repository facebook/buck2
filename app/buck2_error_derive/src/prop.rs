/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// This code is adapted from https://github.com/dtolnay/thiserror licensed under Apache-2.0 or MIT.

use syn::Member;

use crate::ast::Enum;
use crate::ast::Field;
use crate::ast::Struct;
use crate::ast::Variant;

impl Struct<'_> {
    pub(crate) fn source_field(&self) -> Option<&Field> {
        source_field(&self.fields)
    }
}

impl Enum<'_> {
    pub(crate) fn has_display(&self) -> bool {
        self.attrs.display.is_some()
            || self.attrs.transparent.is_some()
            || self
                .variants
                .iter()
                .any(|variant| variant.attrs.display.is_some())
            || self
                .variants
                .iter()
                .all(|variant| variant.attrs.transparent.is_some())
    }
}

impl Variant<'_> {
    pub(crate) fn source_field(&self) -> Option<&Field> {
        source_field(&self.fields)
    }
}

fn source_field<'a, 'b>(fields: &'a [Field<'b>]) -> Option<&'a Field<'b>> {
    for field in fields {
        if field.attrs.source.is_some() {
            return Some(field);
        }
    }
    for field in fields {
        match &field.member {
            Member::Named(ident) if ident == "source" => return Some(field),
            _ => {}
        }
    }
    None
}
