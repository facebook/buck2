//! Procedural macro implementing `#[derive(Enum)]`
//!
//! This is supposed to used with `enum-map` crate, which provides the
//! actual usage documentation.

#![recursion_limit = "128"]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

use std::iter;

use syn::spanned::Spanned;
use syn::{Data, DataEnum, DeriveInput, Fields, Ident, Variant};

fn generate_enum_code(name: Ident, data_enum: DataEnum) -> proc_macro2::TokenStream {
    let enum_count = data_enum.variants.len();
    let mut has_discriminants = false;

    for variant in &data_enum.variants {
        let Variant {
            fields,
            discriminant,
            ..
        } = variant;
        match fields {
            Fields::Unit => (),
            _ => {
                return syn::Error::new(fields.span(), "#[derive(Enum)] requires C style enum")
                    .to_compile_error();
            }
        }

        if discriminant.is_some() {
            has_discriminants = true;
        }
    }

    let variants_names_a = data_enum.variants.iter().map(|variant| &variant.ident);
    let variants_names_b = data_enum.variants.iter().map(|variant| &variant.ident);
    let repeat_name_a = iter::repeat(&name);
    let repeat_name_b = repeat_name_a.clone();
    let counter = 0..enum_count;

    let to_usize = if enum_count == 0 || has_discriminants {
        let variants_names = data_enum.variants.iter().map(|variant| &variant.ident);
        let repeat_name = repeat_name_a.clone();
        let counter = counter.clone();

        quote! {
            match self {
                #(
                    #repeat_name::#variants_names => #counter,
                )*
            }
        }
    } else {
        quote! { self as usize }
    };

    quote! {
        #[automatically_derived]
        impl<V> ::enum_map::Enum<V> for #name {
            type Array = [V; #enum_count];
            const POSSIBLE_VALUES: usize = #enum_count;

            #[inline]
            fn slice(array: &Self::Array) -> &[V] {
                array
            }

            #[inline]
            fn slice_mut(array: &mut Self::Array) -> &mut [V] {
                array
            }

            #[inline]
            fn from_usize(value: usize) -> Self {
                match value {
                    #(
                        #counter => #repeat_name_a::#variants_names_a,
                    )*
                    _ => unreachable!()
                }
            }

            #[inline]
            fn to_usize(self) -> usize {
                #to_usize
            }

            #[inline]
            fn from_function<F: FnMut(Self) -> V>(mut _f: F) -> Self::Array {
                [#(
                    _f(#repeat_name_b::#variants_names_b),
                )*]
            }
        }
    }
}

/// Procedural derive generating `enum_map::Enum` implementation.
///
/// # Examples
///
/// ```
/// # extern crate enum_map;
/// use enum_map::Enum;
///
/// #[derive(Enum)]
/// enum A {
///     B,
///     C,
///     D,
/// }
///
/// assert_eq!(Enum::<()>::to_usize(A::C), 1);
/// ```
#[proc_macro_derive(Enum)]
pub fn derive_enum_map(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();

    let result = match input.data {
        Data::Enum(data_enum) => generate_enum_code(input.ident, data_enum),
        _ => quote!(compile_error! {"#[derive(Enum)] is only defined for enums"}),
    };

    result.into()
}
