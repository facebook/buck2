/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The `#[buck_settings_section]` attribute. See `buck2_settings` for what a
//! section is and how to declare one.

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::Attribute;
use syn::Expr;
use syn::ExprLit;
use syn::Fields;
use syn::GenericArgument;
use syn::Ident;
use syn::ItemStruct;
use syn::Lit;
use syn::LitInt;
use syn::LitStr;
use syn::Meta;
use syn::PathArguments;
use syn::Token;
use syn::Type;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::parse::Parser;
use syn::spanned::Spanned;

/// Declares a section of Buck settings.
///
/// ```ignore
/// /// What the section controls.
/// #[buck_settings_section(name = "hydration", version = 3, scope = daemon)]
/// pub struct Hydration {
///     /// What the setting does.
///     #[setting(default = false, overridable_in(CommandLine, LocalSettings))]
///     defer_field_reads: Option<bool>,
/// }
/// ```
///
/// Every field is an `Option<T>` holding what the settings layers set, `None`
/// when no layer did, and carries one `#[setting(...)]` with:
///
/// - `overridable_in(...)`, required: which override sources may set the
///   key, from `CommandLine` and `LocalSettings`. The repository file and
///   rollouts always may. `overridable_in()` makes the key an enforcement
///   control nothing local can change.
/// - `default = <expr>`: the value when no layer set the key, in both
///   internal and OSS builds. `internal_default` and `oss_default` set one
///   side. A key with both defaults gets an accessor returning `T`; a key
///   missing either gets one returning `Option<T>`, for keys whose unset
///   value means "fall back to buckconfig".
/// - `accessor = <name>`: the accessor's name when the field's name is taken
///   by a hand-written one.
///
/// The struct gets the serde and `Allocative` derives, an accessor per key,
/// and an implementation of `SettingsSection` whose metadata carries the doc
/// comments, types, defaults, and policies. `version` selects the rollout
/// stanza: bump it whenever the section's schema or semantics change, and ship
/// the bump together with a stanza for the new version that carries every key
/// of the previous one, since a binary selects its exact version only.
#[proc_macro_attribute]
pub fn buck_settings_section(attr: TokenStream, item: TokenStream) -> TokenStream {
    match expand(attr.into(), item.into()) {
        Ok(tokens) => tokens.into(),
        Err(error) => error.to_compile_error().into(),
    }
}

struct SectionArgs {
    name: LitStr,
    version: LitInt,
    scope: Ident,
}

fn parse_section_args(attr: proc_macro2::TokenStream) -> syn::Result<SectionArgs> {
    let parser = |input: ParseStream| -> syn::Result<SectionArgs> {
        let mut name = None;
        let mut version = None;
        let mut scope = None;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![=]>()?;
            match key.to_string().as_str() {
                "name" => name = Some(input.parse::<LitStr>()?),
                "version" => version = Some(input.parse::<LitInt>()?),
                "scope" => {
                    let value: Ident = input.parse()?;
                    if value != "daemon" && value != "invocation" {
                        return Err(syn::Error::new(
                            value.span(),
                            "expected `daemon` or `invocation`",
                        ));
                    }
                    scope = Some(value);
                }
                _ => {
                    return Err(syn::Error::new(
                        key.span(),
                        "expected `name`, `version` or `scope`",
                    ));
                }
            }
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }
        let missing = |what: &str| {
            syn::Error::new(
                Span::call_site(),
                format!("`#[buck_settings_section]` needs `{what} = ...`"),
            )
        };
        Ok(SectionArgs {
            name: name.ok_or_else(|| missing("name"))?,
            version: version.ok_or_else(|| missing("version"))?,
            scope: scope.ok_or_else(|| missing("scope"))?,
        })
    };
    parser.parse2(attr)
}

#[derive(Default)]
struct SettingArgs {
    internal_default: Option<Expr>,
    oss_default: Option<Expr>,
    overridable_in: Option<Vec<Ident>>,
    accessor: Option<Ident>,
}

fn parse_setting_args(attrs: &[Attribute], field: &Ident) -> syn::Result<SettingArgs> {
    let mut args = SettingArgs::default();
    let mut found = false;
    for attr in attrs {
        if !attr.path().is_ident("setting") {
            continue;
        }
        if found {
            return Err(syn::Error::new(
                attr.span(),
                "one `#[setting(...)]` per key",
            ));
        }
        found = true;
        // Explicit sides win over `default`, whatever the order they are written in.
        let mut internal_explicit = false;
        let mut oss_explicit = false;
        attr.parse_args_with(|input: ParseStream| {
            while !input.is_empty() {
                let key: Ident = input.parse()?;
                match key.to_string().as_str() {
                    "default" => {
                        input.parse::<Token![=]>()?;
                        let expr: Expr = input.parse()?;
                        if !internal_explicit {
                            args.internal_default = Some(expr.clone());
                        }
                        if !oss_explicit {
                            args.oss_default = Some(expr);
                        }
                    }
                    "internal_default" => {
                        input.parse::<Token![=]>()?;
                        args.internal_default = Some(input.parse()?);
                        internal_explicit = true;
                    }
                    "oss_default" => {
                        input.parse::<Token![=]>()?;
                        args.oss_default = Some(input.parse()?);
                        oss_explicit = true;
                    }
                    "overridable_in" => {
                        let content;
                        syn::parenthesized!(content in input);
                        let sources = content.parse_terminated(Ident::parse, Token![,])?;
                        for source in &sources {
                            if source != "CommandLine" && source != "LocalSettings" {
                                return Err(syn::Error::new(
                                    source.span(),
                                    "expected `CommandLine` or `LocalSettings`",
                                ));
                            }
                        }
                        args.overridable_in = Some(sources.into_iter().collect());
                    }
                    "accessor" => {
                        input.parse::<Token![=]>()?;
                        args.accessor = Some(input.parse()?);
                    }
                    other => {
                        return Err(syn::Error::new(
                            key.span(),
                            format!(
                                "unknown setting option `{other}`; expected `default`, \
                                `internal_default`, `oss_default`, `overridable_in` or `accessor`"
                            ),
                        ));
                    }
                }
                if !input.is_empty() {
                    input.parse::<Token![,]>()?;
                }
            }
            Ok(())
        })?;
    }
    if !found {
        return Err(syn::Error::new(
            field.span(),
            "every key needs `#[setting(overridable_in(...))]`",
        ));
    }
    if args.overridable_in.is_none() {
        return Err(syn::Error::new(
            field.span(),
            "`overridable_in(...)` is required; `overridable_in()` means no local override may \
            change the key",
        ));
    }
    Ok(args)
}

/// The text of the doc comments among `attrs`.
fn docs(attrs: &[Attribute]) -> String {
    let mut lines = Vec::new();
    for attr in attrs {
        if !attr.path().is_ident("doc") {
            continue;
        }
        if let Meta::NameValue(meta) = &attr.meta
            && let Expr::Lit(ExprLit {
                lit: Lit::Str(text),
                ..
            }) = &meta.value
        {
            lines.push(text.value().trim().to_owned());
        }
    }
    lines.join("\n")
}

fn option_inner(ty: &Type) -> Option<&Type> {
    let Type::Path(path) = ty else {
        return None;
    };
    let segment = path.path.segments.last()?;
    if segment.ident != "Option" {
        return None;
    }
    let PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return None;
    };
    if arguments.args.len() != 1 {
        return None;
    }
    match arguments.args.first()? {
        GenericArgument::Type(inner) => Some(inner),
        _ => None,
    }
}

fn expand(
    attr: proc_macro2::TokenStream,
    item: proc_macro2::TokenStream,
) -> syn::Result<proc_macro2::TokenStream> {
    let SectionArgs {
        name: section_name,
        version,
        scope,
    } = parse_section_args(attr)?;
    let mut item: ItemStruct = syn::parse2(item)?;
    let Fields::Named(fields) = &mut item.fields else {
        return Err(syn::Error::new(
            item.span(),
            "a settings section is a struct with named fields",
        ));
    };
    let section_doc = docs(&item.attrs);
    let struct_name = item.ident.clone();
    let scope = Ident::new(
        if scope == "daemon" {
            "Daemon"
        } else {
            "Invocation"
        },
        scope.span(),
    );

    let mut accessors = Vec::new();
    let mut metadata = Vec::new();
    let mut values = Vec::new();
    for field in fields.named.iter_mut() {
        let name = field.ident.clone().expect("named fields");
        let inner = option_inner(&field.ty)
            .ok_or_else(|| {
                syn::Error::new(
                    field.ty.span(),
                    "a key is declared as `Option<T>`; `None` is what no layer set",
                )
            })?
            .clone();
        let setting = parse_setting_args(&field.attrs, &name)?;
        field.attrs.retain(|attr| !attr.path().is_ident("setting"));
        let doc_attrs: Vec<Attribute> = field
            .attrs
            .iter()
            .filter(|attr| attr.path().is_ident("doc"))
            .cloned()
            .collect();
        let doc = docs(&field.attrs);
        let name_str = name.to_string();
        let accessor = setting.accessor.clone().unwrap_or_else(|| name.clone());
        let sources = setting.overridable_in.expect("checked when parsing");

        let as_option = |expr: &Option<Expr>| match expr {
            Some(expr) => quote!(::core::option::Option::Some(#expr)),
            None => quote!(::core::option::Option::None),
        };
        let as_thunk = |expr: &Option<Expr>| match expr {
            Some(expr) => quote!(|| {
                let value: #inner = #expr;
                ::core::option::Option::Some(
                    ::serde_json::to_value(value).expect("setting defaults serialize as JSON"),
                )
            }),
            None => quote!(|| ::core::option::Option::None),
        };
        let internal_thunk = as_thunk(&setting.internal_default);
        let oss_thunk = as_thunk(&setting.oss_default);
        let (returns, body) = match (&setting.internal_default, &setting.oss_default) {
            (Some(internal), Some(oss)) => (
                quote!(#inner),
                quote! {
                    match ::core::clone::Clone::clone(&self.#name) {
                        ::core::option::Option::Some(value) => value,
                        ::core::option::Option::None => {
                            if cfg!(fbcode_build) { #internal } else { #oss }
                        }
                    }
                },
            ),
            (internal, oss) => {
                let internal = as_option(internal);
                let oss = as_option(oss);
                (
                    quote!(::core::option::Option<#inner>),
                    quote! {
                        ::core::clone::Clone::clone(&self.#name)
                            .or_else(|| if cfg!(fbcode_build) { #internal } else { #oss })
                    },
                )
            }
        };
        accessors.push(quote! {
            #(#doc_attrs)*
            pub fn #accessor(&self) -> #returns {
                #body
            }
        });
        metadata.push(quote! {
            ::buck2_settings::metadata::SettingKeyMetadata {
                name: #name_str,
                doc: #doc,
                type_name: stringify!(#inner),
                overridable_in: &[#(::buck2_settings::metadata::OverrideSource::#sources),*],
                internal_default: #internal_thunk,
                oss_default: #oss_thunk,
            }
        });
        values.push(quote! {
            ::buck2_settings::metadata::SettingValue {
                name: #name_str,
                set: ::serde_json::to_value(&self.#name).expect("settings serialize as JSON"),
                resolved: ::serde_json::to_value(&self.#accessor())
                    .expect("settings serialize as JSON"),
            }
        });
    }

    Ok(quote! {
        #[derive(
            Clone,
            Debug,
            Default,
            PartialEq,
            Eq,
            ::serde::Serialize,
            ::serde::Deserialize,
            ::allocative::Allocative
        )]
        #[serde(default, deny_unknown_fields)]
        #item

        impl #struct_name {
            #(#accessors)*
        }

        impl ::buck2_settings::metadata::SettingsSection for #struct_name {
            const METADATA: ::buck2_settings::metadata::SectionMetadata =
                ::buck2_settings::metadata::SectionMetadata {
                    section_name: #section_name,
                    section_version: #version,
                    scope: ::buck2_settings::metadata::SectionScope::#scope,
                    doc: #section_doc,
                    settings: &[#(#metadata),*],
                };

            fn values(&self) -> ::std::vec::Vec<::buck2_settings::metadata::SettingValue> {
                ::std::vec![#(#values),*]
            }
        }
    })
}
