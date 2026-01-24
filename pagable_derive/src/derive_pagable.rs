/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use proc_macro2::Ident;
use proc_macro2::Span;
use quote::ToTokens;
use quote::quote_spanned;
use syn::Attribute;
use syn::Data;
use syn::DataEnum;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Fields;
use syn::GenericParam;
use syn::Generics;
use syn::LitStr;
use syn::Token;
use syn::Type;
use syn::Variant;
use syn::parse::ParseStream;
use syn::spanned::Spanned;

pub(crate) fn derive_pagable(
    input: proc_macro::TokenStream,
    generate_serialize: bool,
    generate_deserialize: bool,
) -> proc_macro::TokenStream {
    match derive_pagable_impl(input.into(), generate_serialize, generate_deserialize) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

pub fn map_to_tokens<T>(
    iter: impl IntoIterator<Item = T>,
    f: impl Fn(T) -> syn::Result<proc_macro2::TokenStream>,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    iter.into_iter()
        .map(f)
        .collect::<syn::Result<Vec<proc_macro2::TokenStream>>>()
}

struct SerializeGenerator;
impl SerializeGenerator {
    fn gen_field(field: &ParsedField) -> syn::Result<proc_macro2::TokenStream> {
        let ty = &field.ty;
        let ident = &field.var_name;
        let ident_str = ident.to_string();
        if field.attrs.skip || field.attrs.discard.is_some() {
            Ok(quote_spanned! {field.span=>})
        } else if field.attrs.flatten_serde {
            Ok(quote_spanned! {field.span=>
                pagable::__internal::anyhow::Context::with_context(
                    <#ty as pagable::__internal::serde::ser::Serialize>::serialize(#ident, serializer.serde()),
                    || format!("serializing serde flattened field {}", #ident_str),
                )?
            })
        } else {
            Ok(quote_spanned! {field.span=>
                pagable::__internal::anyhow::Context::with_context(
                    <#ty as pagable::PagableSerialize>::pagable_serialize(#ident, serializer),
                    || format!("serializing field {}", #ident_str),
                )?

            })
        }
    }

    fn gen_unpack(v: &ParsedFields) -> syn::Result<proc_macro2::TokenStream> {
        match &v.data {
            ParsedFieldsData::Unit => Ok(quote_spanned! {v.span=>}),
            ParsedFieldsData::Named(fields) => {
                let names = fields.iter().map(|f| &f.var_name);
                Ok(quote_spanned! {v.span=>{#(#names),*} })
            }
            ParsedFieldsData::Unnamed(fields) => {
                let names = fields.iter().map(|f| &f.var_name);
                Ok(quote_spanned! {v.span=>(#(#names),*) })
            }
        }
    }

    fn gen_visit_fields(v: &ParsedFields) -> syn::Result<proc_macro2::TokenStream> {
        let visit_fields = match &v.data {
            ParsedFieldsData::Unit => {
                return Ok(quote_spanned! {v.span=>});
            }
            ParsedFieldsData::Named(fields) => map_to_tokens(fields, Self::gen_field)?,
            ParsedFieldsData::Unnamed(fields) => map_to_tokens(fields, Self::gen_field)?,
        };
        Ok(quote_spanned! {v.span=>#(#visit_fields;)*})
    }

    fn gen_struct(v: &ParsedStruct) -> syn::Result<proc_macro2::TokenStream> {
        let unpack = Self::gen_unpack(&v.fields)?;
        let visit_fields = Self::gen_visit_fields(&v.fields)?;
        Ok(quote_spanned! {v.span=>
            let Self #unpack = self;
            #visit_fields;
        })
    }

    fn gen_enum_variant(v: &ParsedEnumVariant) -> syn::Result<proc_macro2::TokenStream> {
        let index = v.index;
        let name = &v.name;
        let unpack = Self::gen_unpack(&v.fields)?;
        let visit_fields = Self::gen_visit_fields(&v.fields)?;
        Ok(quote_spanned! {v.span=>
            Self::#name #unpack => {
                <usize as pagable::__internal::serde::Serialize>::serialize(&#index, serializer.serde())?;
                #visit_fields;
            }
        })
    }

    fn gen_enum(v: &ParsedEnum) -> syn::Result<proc_macro2::TokenStream> {
        if v.cases.is_empty() {
            Ok(quote_spanned! {v.span=>})
        } else {
            let cases = map_to_tokens(&v.cases, Self::gen_enum_variant)?;
            Ok(quote_spanned! {v.span=>
                match self {
                    #(#cases),*
                }
            })
        }
    }

    fn gen_visit_body(parsed: &ParsedType) -> syn::Result<proc_macro2::TokenStream> {
        match &parsed.data {
            ParsedTypeData::Struct(s) => Self::gen_struct(s),
            ParsedTypeData::Enum(e) => Self::gen_enum(e),
        }
    }
}

struct DeserializeGenerator;

impl DeserializeGenerator {
    fn gen_field(field: &ParsedField) -> syn::Result<proc_macro2::TokenStream> {
        let ty = &field.ty;
        let ident_str = field.var_name.to_string();

        if field.attrs.skip {
            Ok(quote_spanned! {field.span=>})
        } else if let Some(discard) = &field.attrs.discard {
            let discard = discard.parse::<proc_macro2::TokenStream>()?;
            Ok(quote_spanned! {field.span=>#discard})
        } else if field.attrs.flatten_serde {
            Ok(quote_spanned! {field.span=>
                pagable::__internal::anyhow::Context::with_context(
                    <#ty as pagable::__internal::serde::de::Deserialize>::deserialize(deserializer.serde()),
                    || format!("deserializing serde flattened field {}", #ident_str),
                )?
            })
        } else {
            Ok(quote_spanned! {field.span=>
                pagable::__internal::anyhow::Context::with_context(
                    <#ty as pagable::PagableDeserialize>::pagable_deserialize(deserializer),
                    || format!("deserializing field {}", #ident_str),
                )?
            })
        }
    }

    fn gen_initializer(v: &ParsedFields) -> syn::Result<proc_macro2::TokenStream> {
        match &v.data {
            ParsedFieldsData::Unit => Ok(quote_spanned! {v.span=>}),
            ParsedFieldsData::Named(fields) => {
                let names = fields.iter().map(|f| &f.var_name);
                let visit_fields = map_to_tokens(fields, Self::gen_field)?;
                Ok(quote_spanned! {v.span=>
                    {#(#names: #visit_fields),*}
                })
            }
            ParsedFieldsData::Unnamed(fields) => {
                let visit_fields = map_to_tokens(fields, Self::gen_field)?;
                Ok(quote_spanned! {v.span=>
                     (#(#visit_fields),*)
                })
            }
        }
    }

    fn gen_struct(v: &ParsedStruct) -> syn::Result<proc_macro2::TokenStream> {
        let initializer = Self::gen_initializer(&v.fields)?;
        Ok(quote_spanned! {v.span=>Self #initializer})
    }

    fn gen_enum_variant(v: &ParsedEnumVariant) -> syn::Result<proc_macro2::TokenStream> {
        let index = v.index;
        let name = &v.name;
        let initializer = Self::gen_initializer(&v.fields)?;
        Ok(quote_spanned! {v.span=>#index => Self::#name #initializer})
    }

    fn gen_enum(v: &ParsedEnum) -> syn::Result<proc_macro2::TokenStream> {
        if v.cases.is_empty() {
            Ok(quote_spanned! {v.span=>})
        } else {
            let cases = map_to_tokens(&v.cases, Self::gen_enum_variant)?;
            Ok(quote_spanned! {v.span=>
                {
                    let index = <usize as pagable::__internal::serde::Deserialize>::deserialize(deserializer.serde())?;
                    match index {
                        #(#cases,)*
                        _ => unreachable!()
                    }
                }
            })
        }
    }

    fn gen_visit_body(parsed: &ParsedType) -> syn::Result<proc_macro2::TokenStream> {
        match &parsed.data {
            ParsedTypeData::Struct(s) => Self::gen_struct(s),
            ParsedTypeData::Enum(e) => Self::gen_enum(e),
        }
    }
}

fn impl_generics(
    generics: &Generics,
    attrs: &PagableAttrs,
) -> syn::Result<(proc_macro2::TokenStream, proc_macro2::TokenStream)> {
    if let Some(bound) = &attrs.bound {
        if !bound.is_empty() {
            let span = attrs.span.unwrap_or_else(Span::call_site);
            let bound = bound.parse::<proc_macro2::TokenStream>()?;
            return Ok((
                quote_spanned! { span => < #bound > },
                quote_spanned! { span => <'de, #bound > },
            ));
        }
    }

    let mut impl_generics = Vec::new();
    for p in &generics.params {
        impl_generics.push(match p {
            GenericParam::Type(tp) => {
                let mut tp = tp.clone();
                if attrs.bound.is_none() && !attrs.skip && attrs.discard.is_none() {
                    tp.bounds.push(syn::parse2(quote_spanned! { tp.span() =>
                        pagable::Pagable
                    })?);
                }

                tp.default = None;
                tp.to_token_stream()
            }
            GenericParam::Lifetime(l) => l.to_token_stream(),
            GenericParam::Const(c) => c.to_token_stream(),
        });
    }
    if impl_generics.is_empty() {
        Ok((
            quote_spanned! { generics.span() => },
            quote_spanned! { generics.span() => <'de>},
        ))
    } else {
        Ok((
            quote_spanned! { generics.span() => <#(#impl_generics),*> },
            quote_spanned! { generics.span() => <'de, #(#impl_generics),*> },
        ))
    }
}

fn derive_pagable_impl(
    input: proc_macro2::TokenStream,
    generate_serialize: bool,
    generate_deserialize: bool,
) -> syn::Result<proc_macro2::TokenStream> {
    let input: DeriveInput = syn::parse2(input)?;
    let name = &input.ident;
    let name_str = name.to_string();
    let (_impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let attrs = extract_attrs(&input.attrs)?;
    let (ser_impl_generics, de_impl_generics) = impl_generics(&input.generics, &attrs)?;

    if attrs.skip {
        return Err(syn::Error::new(
            input.ident.span(),
            "skip was set on struct level",
        ));
    }

    let parsed = parse_body(&input)?;

    let serialize_body = if !generate_serialize {
        quote_spanned! { input.span() =>
        }
    } else {
        let body = SerializeGenerator::gen_visit_body(&parsed)?;
        quote_spanned! {input.span()=>
            #[allow(clippy::redundant_closure_call, unused, clippy::todo)]
            impl #ser_impl_generics pagable::PagableSerialize for #name #type_generics #where_clause {
                    fn pagable_serialize(&self, serializer: &mut dyn pagable::PagableSerializer) -> pagable::__internal::anyhow::Result<()> {
                        #body
                        Ok(())
                    }

            }
        }
    };

    let deserialize_body = if !generate_deserialize {
        quote_spanned! { input.span() =>
        }
    } else {
        let body = DeserializeGenerator::gen_visit_body(&parsed)?;
        quote_spanned! {input.span()=>
            #[allow(clippy::redundant_closure_call, unused, clippy::todo)]
            impl #de_impl_generics pagable::PagableDeserialize<'de> for #name #type_generics #where_clause {
                fn pagable_deserialize<D: pagable::PagableDeserializer<'de> + ?Sized>(deserializer: &mut D) -> pagable::Result<Self> {
                    let res : pagable::Result<Self> = (|| {Ok(#body)})();
                    pagable::__internal::anyhow::Context::with_context(res, || format!("deserializing type {}", #name_str))
                }
            }
        }
    };

    Ok(quote_spanned! {input.span()=>
        #serialize_body
        #deserialize_body
    })
}

#[allow(unused)]
struct ParsedType {
    span: Span,
    data: ParsedTypeData,
}

enum ParsedTypeData {
    Struct(ParsedStruct),
    Enum(ParsedEnum),
}

struct ParsedStruct {
    span: Span,
    fields: ParsedFields,
}

struct ParsedFields {
    span: Span,
    data: ParsedFieldsData,
}

enum ParsedFieldsData {
    Unit,
    Named(Vec<ParsedField>),
    Unnamed(Vec<ParsedField>),
}

struct ParsedEnum {
    span: Span,
    cases: Vec<ParsedEnumVariant>,
}

struct ParsedEnumVariant {
    span: Span,
    index: usize,
    name: Ident,
    fields: ParsedFields,
}

struct ParsedField {
    span: Span,
    attrs: PagableAttrs,
    var_name: Ident,
    ty: Type,
}

fn parse_body(input: &DeriveInput) -> syn::Result<ParsedType> {
    let data = match &input.data {
        Data::Struct(s) => ParsedTypeData::Struct(parse_struct(input.span(), s)?),
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(input, "unions are not supported"));
        }
        Data::Enum(e) => ParsedTypeData::Enum(parse_enum(input.span(), e)?),
    };
    Ok(ParsedType {
        span: input.span(),
        data,
    })
}

fn parse_struct(span: Span, input: &DataStruct) -> syn::Result<ParsedStruct> {
    let data = parse_fields(&input.fields)?;
    Ok(ParsedStruct { span, fields: data })
}

fn parse_enum(span: Span, input: &DataEnum) -> syn::Result<ParsedEnum> {
    let cases = input
        .variants
        .iter()
        .enumerate()
        .map(|(i, v)| parse_enum_variant(i, v))
        .collect::<syn::Result<_>>()?;
    Ok(ParsedEnum { span, cases })
}

fn parse_enum_variant(index: usize, input: &Variant) -> syn::Result<ParsedEnumVariant> {
    let data = parse_fields(&input.fields)?;
    Ok(ParsedEnumVariant {
        span: input.span(),
        index,
        name: input.ident.clone(),
        fields: data,
    })
}

fn parse_fields(input: &Fields) -> syn::Result<ParsedFields> {
    let data = match &input {
        Fields::Named(fields_named) => ParsedFieldsData::Named(
            fields_named
                .named
                .iter()
                .map(|f| {
                    <Result<_, syn::Error>>::Ok(ParsedField {
                        span: f.span(),
                        attrs: extract_attrs(&f.attrs)?,
                        var_name: f
                            .ident
                            .clone()
                            .ok_or_else(|| syn::Error::new_spanned(f, "unnamed field in named"))?,
                        ty: f.ty.clone(),
                    })
                })
                .collect::<syn::Result<Vec<_>>>()?,
        ),
        Fields::Unnamed(fields_unnamed) => ParsedFieldsData::Unnamed(
            fields_unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    <Result<_, syn::Error>>::Ok(ParsedField {
                        span: f.span(),
                        attrs: extract_attrs(&f.attrs)?,
                        var_name: Ident::new(&format!("f{}", i), f.span()),
                        ty: f.ty.clone(),
                    })
                })
                .collect::<syn::Result<Vec<_>>>()?,
        ),
        Fields::Unit => ParsedFieldsData::Unit,
    };
    Ok(ParsedFields {
        span: input.span(),
        data,
    })
}

#[derive(Default)]
struct PagableAttrs {
    span: Option<Span>,
    skip: bool,
    flatten_serde: bool,
    bound: Option<String>,
    discard: Option<String>,
}

/// Parse an `#[pagable(...)]` annotation.
fn extract_attrs(attrs: &[Attribute]) -> syn::Result<PagableAttrs> {
    syn::custom_keyword!(skip);
    syn::custom_keyword!(stash);
    syn::custom_keyword!(bound);
    syn::custom_keyword!(discard);
    syn::custom_keyword!(flatten_serde);

    let mut opts = PagableAttrs::default();

    for attr in attrs.iter() {
        if !attr.path().is_ident("pagable") {
            continue;
        }

        opts.span = Some(attr.span());

        attr.parse_args_with(|input: ParseStream| {
            loop {
                if input.parse::<skip>().is_ok() {
                    if opts.skip {
                        return Err(input.error("`skip` was set twice"));
                    }
                    opts.skip = true;
                } else if input.parse::<discard>().is_ok() {
                    input.parse::<Token![=]>()?;
                    let discard = input.parse::<LitStr>()?;
                    if opts.discard.is_some() {
                        return Err(input.error("`discard` was set twice"));
                    }
                    opts.discard = Some(discard.value());
                } else if input.parse::<stash>().is_ok() {
                    if opts.skip {
                        return Err(input.error("`skip` was set twice"));
                    }
                    opts.skip = true;
                } else if input.parse::<flatten_serde>().is_ok() {
                    if opts.flatten_serde {
                        return Err(input.error("`flatten_serde` was set twice"));
                    }
                    opts.flatten_serde = true;
                } else if input.parse::<bound>().is_ok() {
                    input.parse::<Token![=]>()?;
                    let bound = input.parse::<LitStr>()?;
                    if opts.bound.is_some() {
                        return Err(input.error("`bound` was set twice"));
                    }
                    opts.bound = Some(bound.value());
                }

                if input.is_empty() {
                    return Ok(());
                }

                input.parse::<Token![,]>()?;
            }
        })?;
    }

    Ok(opts)
}
