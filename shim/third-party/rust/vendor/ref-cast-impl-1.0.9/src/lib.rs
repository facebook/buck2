#![allow(clippy::needless_pass_by_value, clippy::if_not_else)]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2, TokenTree};
use quote::quote;
use syn::parse::{Nothing, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    parse_macro_input, token, Data, DeriveInput, Error, Expr, Field, Path, Result, Token, Type,
};

#[proc_macro_derive(RefCast, attributes(trivial))]
pub fn derive_ref_cast(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand(input)
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

fn expand(input: DeriveInput) -> Result<TokenStream2> {
    check_repr(&input)?;

    let name = &input.ident;
    let name_str = name.to_string();
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let fields = fields(&input)?;
    let from = only_field_ty(fields)?;
    let trivial = trivial_fields(fields)?;

    let assert_trivial_fields = if !trivial.is_empty() {
        Some(quote! {
            if false {
                #(
                    ::ref_cast::private::assert_trivial::<#trivial>();
                )*
            }
        })
    } else {
        None
    };

    Ok(quote! {
        impl #impl_generics ::ref_cast::RefCast for #name #ty_generics #where_clause {
            type From = #from;

            #[inline]
            fn ref_cast(_from: &Self::From) -> &Self {
                #assert_trivial_fields
                #[cfg(debug_assertions)]
                {
                    #[allow(unused_imports)]
                    use ::ref_cast::private::LayoutUnsized;
                    ::ref_cast::private::assert_layout::<Self, Self::From>(
                        #name_str,
                        ::ref_cast::private::Layout::<Self>::SIZE,
                        ::ref_cast::private::Layout::<Self::From>::SIZE,
                        ::ref_cast::private::Layout::<Self>::ALIGN,
                        ::ref_cast::private::Layout::<Self::From>::ALIGN,
                    );
                }
                unsafe {
                    &*(_from as *const Self::From as *const Self)
                }
            }

            #[inline]
            fn ref_cast_mut(_from: &mut Self::From) -> &mut Self {
                #[cfg(debug_assertions)]
                {
                    #[allow(unused_imports)]
                    use ::ref_cast::private::LayoutUnsized;
                    ::ref_cast::private::assert_layout::<Self, Self::From>(
                        #name_str,
                        ::ref_cast::private::Layout::<Self>::SIZE,
                        ::ref_cast::private::Layout::<Self::From>::SIZE,
                        ::ref_cast::private::Layout::<Self>::ALIGN,
                        ::ref_cast::private::Layout::<Self::From>::ALIGN,
                    );
                }
                unsafe {
                    &mut *(_from as *mut Self::From as *mut Self)
                }
            }
        }
    })
}

fn check_repr(input: &DeriveInput) -> Result<()> {
    let mut has_repr = false;
    let mut errors = None;
    let mut push_error = |error| match &mut errors {
        Some(errors) => Error::combine(errors, error),
        None => errors = Some(error),
    };

    for attr in &input.attrs {
        if attr.path.is_ident("repr") {
            if let Err(error) = attr.parse_args_with(|input: ParseStream| {
                while !input.is_empty() {
                    let path = input.call(Path::parse_mod_style)?;
                    if path.is_ident("transparent") || path.is_ident("C") {
                        has_repr = true;
                    } else if path.is_ident("packed") {
                        // ignore
                    } else {
                        let meta_item_span = if input.peek(token::Paren) {
                            let group: TokenTree = input.parse()?;
                            quote!(#path #group)
                        } else if input.peek(Token![=]) {
                            let eq_token: Token![=] = input.parse()?;
                            let value: Expr = input.parse()?;
                            quote!(#path #eq_token #value)
                        } else {
                            quote!(#path)
                        };
                        let msg = if path.is_ident("align") {
                            "aligned repr on struct that implements RefCast is not supported"
                        } else {
                            "unrecognized repr on struct that implements RefCast"
                        };
                        push_error(Error::new_spanned(meta_item_span, msg));
                    }
                    if !input.is_empty() {
                        input.parse::<Token![,]>()?;
                    }
                }
                Ok(())
            }) {
                push_error(error);
            }
        }
    }

    if !has_repr {
        let mut requires_repr = Error::new(
            Span::call_site(),
            "RefCast trait requires #[repr(transparent)]",
        );
        if let Some(errors) = errors {
            requires_repr.combine(errors);
        }
        errors = Some(requires_repr);
    }

    match errors {
        None => Ok(()),
        Some(errors) => Err(errors),
    }
}

type Fields = Punctuated<Field, Token![,]>;

fn fields(input: &DeriveInput) -> Result<&Fields> {
    use syn::Fields;

    match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => Ok(&fields.named),
            Fields::Unnamed(fields) => Ok(&fields.unnamed),
            Fields::Unit => Err(Error::new(
                Span::call_site(),
                "RefCast does not support unit structs",
            )),
        },
        Data::Enum(_) => Err(Error::new(
            Span::call_site(),
            "RefCast does not support enums",
        )),
        Data::Union(_) => Err(Error::new(
            Span::call_site(),
            "RefCast does not support unions",
        )),
    }
}

fn only_field_ty(fields: &Fields) -> Result<&Type> {
    let is_trivial = decide_trivial(fields)?;
    let mut only_field = None;

    for field in fields {
        if !is_trivial(field)? {
            if only_field.take().is_some() {
                break;
            }
            only_field = Some(&field.ty);
        }
    }

    only_field.ok_or_else(|| {
        Error::new(
            Span::call_site(),
            "RefCast requires a struct with a single field",
        )
    })
}

fn trivial_fields(fields: &Fields) -> Result<Vec<&Type>> {
    let is_trivial = decide_trivial(fields)?;
    let mut trivial = Vec::new();

    for field in fields {
        if is_trivial(field)? {
            trivial.push(&field.ty);
        }
    }

    Ok(trivial)
}

fn decide_trivial(fields: &Fields) -> Result<fn(&Field) -> Result<bool>> {
    for field in fields {
        if is_explicit_trivial(field)? {
            return Ok(is_explicit_trivial);
        }
    }
    Ok(is_implicit_trivial)
}

#[allow(clippy::unnecessary_wraps)] // match signature of is_explicit_trivial
fn is_implicit_trivial(field: &Field) -> Result<bool> {
    match &field.ty {
        Type::Tuple(ty) => Ok(ty.elems.is_empty()),
        Type::Path(ty) => Ok(ty.path.segments.last().unwrap().ident == "PhantomData"),
        _ => Ok(false),
    }
}

fn is_explicit_trivial(field: &Field) -> Result<bool> {
    for attr in &field.attrs {
        if attr.path.is_ident("trivial") {
            syn::parse2::<Nothing>(attr.tokens.clone())?;
            return Ok(true);
        }
    }
    Ok(false)
}
