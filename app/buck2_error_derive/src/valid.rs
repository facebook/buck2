/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// This code is adapted from https://github.com/dtolnay/thiserror licensed under Apache-2.0 or MIT.

use syn::Error;
use syn::GenericArgument;
use syn::PathArguments;
use syn::Result;
use syn::Type;

use crate::ast::Enum;
use crate::ast::Field;
use crate::ast::Input;
use crate::ast::Struct;
use crate::ast::Variant;
use crate::attr::Attrs;

impl Input<'_> {
    pub(crate) fn validate(&self) -> Result<()> {
        match self {
            Input::Struct(input) => input.validate(),
            Input::Enum(input) => input.validate(),
        }
    }
}

impl Struct<'_> {
    fn validate(&self) -> Result<()> {
        check_non_field_attrs(&self.attrs)?;
        if let Some(transparent) = self.attrs.transparent {
            if self.fields.len() != 1 {
                return Err(Error::new_spanned(
                    transparent.original,
                    "#[error(transparent)] requires exactly one field",
                ));
            }
            if let Some(source) = self.fields.iter().find_map(|f| f.attrs.source) {
                return Err(Error::new_spanned(
                    source,
                    "transparent error struct can't contain #[source]",
                ));
            }
        }
        if self.attrs.tags.is_empty() {
            return Err(Error::new(
                self.ident.span(),
                "missing #[buck2(tag = ...)] attribute. All 'buck2_error' must have a tag (Common choices are 'Input', 'Tier0', and 'Environment'). 
                Full list: https://www.internalfb.com/code/fbsource/[4e97d95ed7f3145bf6828827e779eecaeb0c5712]/fbcode/buck2/app/buck2_data/error.proto?lines=42",
            ));
        }
        check_field_attrs(&self.fields)?;
        for field in &self.fields {
            field.validate()?;
        }
        Ok(())
    }
}

impl Enum<'_> {
    fn validate(&self) -> Result<()> {
        check_non_field_attrs(&self.attrs)?;
        let has_display = self.has_display();
        for variant in &self.variants {
            variant.validate()?;
            if has_display && variant.attrs.display.is_none() && variant.attrs.transparent.is_none()
            {
                return Err(Error::new_spanned(
                    variant.original,
                    "missing #[error(\"...\")] display attribute",
                ));
            }
            if self.attrs.tags.is_empty() && variant.attrs.tags.is_empty() {
                return Err(Error::new_spanned(
                    variant.original,
                    "missing #[buck2(tag = ...)] attribute. All 'buck2_error' must have a tag (Default choices are 'Input', 'Tier0', and 'Environment'). 
                    Full list: https://www.internalfb.com/code/fbsource/[4e97d95ed7f3145bf6828827e779eecaeb0c5712]/fbcode/buck2/app/buck2_data/error.proto?lines=42",
                ));
            }
        }
        Ok(())
    }
}

impl Variant<'_> {
    fn validate(&self) -> Result<()> {
        check_non_field_attrs(&self.attrs)?;
        if self.attrs.transparent.is_some() {
            if self.fields.len() != 1 {
                return Err(Error::new_spanned(
                    self.original,
                    "#[error(transparent)] requires exactly one field",
                ));
            }
            if let Some(source) = self.fields.iter().find_map(|f| f.attrs.source) {
                return Err(Error::new_spanned(
                    source,
                    "transparent variant can't contain #[source]",
                ));
            }
        }
        check_field_attrs(&self.fields)?;
        for field in &self.fields {
            field.validate()?;
        }
        Ok(())
    }
}

impl Field<'_> {
    fn validate(&self) -> Result<()> {
        if let Some(display) = &self.attrs.display {
            return Err(Error::new_spanned(
                display.original,
                "not expected here; the #[error(...)] attribute belongs on top of a struct or an enum variant",
            ));
        }
        Ok(())
    }
}

/// If we're checking the attrs on an enum variant, `parsed_earlier` are the attrs on the enum
/// itself.
fn check_non_field_attrs(attrs: &Attrs) -> Result<()> {
    if let Some(source) = &attrs.source {
        return Err(Error::new_spanned(
            source,
            "not expected here; the #[source] attribute belongs on a specific field",
        ));
    }
    if let Some(display) = &attrs.display {
        if attrs.transparent.is_some() {
            return Err(Error::new_spanned(
                display.original,
                "cannot have both #[error(transparent)] and a display attribute",
            ));
        }
    }
    Ok(())
}

fn check_field_attrs(fields: &[Field]) -> Result<()> {
    let mut source_field = None;
    for field in fields {
        if let Some(source) = field.attrs.source {
            if source_field.is_some() {
                return Err(Error::new_spanned(source, "duplicate #[source] attribute"));
            }
            source_field = Some(field);
        }
        if let Some(transparent) = field.attrs.transparent {
            return Err(Error::new_spanned(
                transparent.original,
                "#[error(transparent)] needs to go outside the enum or struct, not on an individual field",
            ));
        }
        if let Some(style) = field.attrs.tags.first() {
            return Err(Error::new(
                style.span(),
                "not expected here; the #[buck2(...)] attribute belongs on top of a struct or an enum variant",
            ));
        }
    }
    if let Some(source_field) = source_field {
        if contains_non_static_lifetime(source_field.ty) {
            return Err(Error::new_spanned(
                &source_field.original.ty,
                "non-static lifetimes are not allowed in the source of an error, because std::error::Error requires the source is dyn Error + 'static",
            ));
        }
    }
    Ok(())
}

fn contains_non_static_lifetime(ty: &Type) -> bool {
    match ty {
        Type::Path(ty) => {
            let bracketed = match &ty.path.segments.last().unwrap().arguments {
                PathArguments::AngleBracketed(bracketed) => bracketed,
                _ => return false,
            };
            for arg in &bracketed.args {
                match arg {
                    GenericArgument::Type(ty) if contains_non_static_lifetime(ty) => return true,
                    GenericArgument::Lifetime(lifetime) if lifetime.ident != "static" => {
                        return true;
                    }
                    _ => {}
                }
            }
            false
        }
        Type::Reference(ty) => ty
            .lifetime
            .as_ref()
            .map_or(false, |lifetime| lifetime.ident != "static"),
        _ => false, // maybe implement later if there are common other cases
    }
}
