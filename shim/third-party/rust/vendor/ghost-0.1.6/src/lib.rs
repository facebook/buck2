//! [![github]](https://github.com/dtolnay/ghost)&ensp;[![crates-io]](https://crates.io/crates/ghost)&ensp;[![docs-rs]](https://docs.rs/ghost)
//!
//! [github]: https://img.shields.io/badge/github-8da0cb?style=for-the-badge&labelColor=555555&logo=github
//! [crates-io]: https://img.shields.io/badge/crates.io-fc8d62?style=for-the-badge&labelColor=555555&logo=rust
//! [docs-rs]: https://img.shields.io/badge/docs.rs-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs
//!
//! <br>
//!
//! **Define your own PhantomData and similarly behaved unit types.**
//!
//! # Background
//!
//! [`PhantomData`] as defined by the Rust standard library is magical in that
//! the same type is impossible to define in ordinary Rust code. It is defined
//! in the standard library like this:
//!
//! [`PhantomData`]: https://doc.rust-lang.org/std/marker/struct.PhantomData.html
//!
//! ```
//! # const IGNORE: &str = stringify! {
//! #[lang = "phantom_data"]
//! pub struct PhantomData<T: ?Sized>;
//! # };
//! ```
//!
//! The `#[lang = "..."]` attribute indicates that this is a [lang item], a
//! special case known to the compiler. It is the only type permitted to carry
//! an unused type parameter.
//!
//! [lang item]: https://manishearth.github.io/blog/2017/01/11/rust-tidbits-what-is-a-lang-item/
//!
//! If we try to define an equivalent unit struct with type parameter, the
//! compiler rejects that.
//!
//! ```compile_fail
//! struct MyPhantom<T: ?Sized>;
//! ```
//!
//! ```text
//! error[E0392]: parameter `T` is never used
//!  --> src/main.rs:1:18
//!   |
//! 1 | struct MyPhantom<T: ?Sized>;
//!   |                  ^ unused type parameter
//!   |
//!   = help: consider removing `T` or using a marker such as `std::marker::PhantomData`
//! ```
//!
//! This crate provides a `#[phantom]` attribute that makes it possible to
//! define unit structs with generic parameters.
//!
//! # Examples
//!
//! ```
//! use ghost::phantom;
//!
//! #[phantom]
//! struct MyPhantom<T: ?Sized>;
//!
//! fn main() {
//!     // Proof that MyPhantom behaves like PhantomData.
//!     let _: MyPhantom<u8> = MyPhantom::<u8>;
//!     assert_eq!(0, std::mem::size_of::<MyPhantom<u8>>());
//! }
//!
//! // Proof that MyPhantom is not just a re-export of PhantomData.
//! // If it were a re-export, these would be conflicting impls.
//! trait Trait {}
//! impl<T> Trait for std::marker::PhantomData<T> {}
//! impl<T> Trait for MyPhantom<T> {}
//!
//! // Proof that MyPhantom is local to the current crate.
//! impl<T> MyPhantom<T> {
//! }
//! ```
//!
//! The implementation accepts where-clauses, lifetimes, multiple generic
//! parameters, and derives. Here is a contrived invocation that demonstrates
//! everything at once:
//!
//! ```
//! use ghost::phantom;
//!
//! #[phantom]
//! #[derive(Copy, Clone, Default, Hash, PartialOrd, Ord, PartialEq, Eq, Debug)]
//! struct Crazy<'a, V: 'a, T> where &'a V: IntoIterator<Item = T>;
//!
//! fn main() {
//!     let _ = Crazy::<'static, Vec<String>, &'static String>;
//!
//!     // Lifetime elision.
//!     let crazy = Crazy::<Vec<String>, &String>;
//!     println!("{:?}", crazy);
//! }
//! ```
//!
//! # Variance
//!
//! The `#[phantom]` attribute accepts attributes on individual generic
//! parameters (both lifetime and type parameters) to make them contravariant or
//! invariant. The default is covariance.
//!
//! - `#[contra]` — contravariant generic parameter
//! - `#[invariant]` — invariant generic parameter
//!
//! The implications of variance are explained in more detail by the [Subtyping
//! chapter] of the Rustonomicon.
//!
//! [Subtyping chapter]: https://doc.rust-lang.org/nomicon/subtyping.html
//!
//! ```
//! use ghost::phantom;
//!
//! #[phantom]
//! struct ContravariantLifetime<#[contra] 'a>;
//!
//! fn f<'a>(arg: ContravariantLifetime<'a>) -> ContravariantLifetime<'static> {
//!     // This coercion is only legal because the lifetime parameter is
//!     // contravariant. If it were covariant (the default) or invariant,
//!     // this would not compile.
//!     arg
//! }
//!
//! #[phantom]
//! struct Demo<A, #[contra] B, #[invariant] C>;
//! #
//! # fn main() {}
//! ```
//!
//! # Use cases
//!
//! Entirely up to your imagination. Just to name one, how about a typed
//! registry library that admits the following syntax for iterating over values
//! registered of a particular type:
//!
//! ```no_run
//! # use ghost::phantom;
//! #
//! # #[phantom]
//! # struct Registry<T>;
//! #
//! # impl<T> IntoIterator for Registry<T> {
//! #     type Item = T;
//! #     type IntoIter = Iter<T>;
//! #     fn into_iter(self) -> Self::IntoIter {
//! #         unimplemented!()
//! #     }
//! # }
//! #
//! # struct Iter<T>(T);
//! #
//! # impl<T> Iterator for Iter<T> {
//! #     type Item = T;
//! #     fn next(&mut self) -> Option<Self::Item> {
//! #         unimplemented!()
//! #     }
//! # }
//! #
//! # struct Flag;
//! #
//! # fn main() {
//! for flag in Registry::<Flag> {
//!     /* ... */
//! }
//! # }
//! ```

#![allow(
    clippy::doc_markdown,
    // https://github.com/rust-lang/rust-clippy/issues/8538
    clippy::iter_with_drain,
    clippy::needless_doctest_main,
    clippy::needless_pass_by_value,
    clippy::too_many_lines
)]

extern crate proc_macro;

mod derive;
mod parse;
mod variance;
mod visibility;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::parse::Nothing;
use syn::{parse_macro_input, Error, GenericParam, Token};

use crate::parse::UnitStruct;

/// Define your own PhantomData and similarly behaved unit types.
///
/// Please refer to the [crate level documentation](index.html) for explanation
/// and examples.
#[proc_macro_attribute]
pub fn phantom(args: TokenStream, input: TokenStream) -> TokenStream {
    parse_macro_input!(args as Nothing);
    let input = parse_macro_input!(input as UnitStruct);

    let ident = &input.ident;
    let call_site = Span::call_site();
    let void_namespace = Ident::new(&format!("__void_{}", ident), call_site);
    let value_namespace = Ident::new(&format!("__value_{}", ident), call_site);

    let vis = &input.vis;
    let vis_super = visibility::vis_super(vis);

    let (derives, attrs) = match derive::expand(&input.attrs, &input) {
        Ok(split) => split,
        Err(err) => return err.to_compile_error().into(),
    };

    let void = Ident::new(
        if ident == "Void" { "__Void" } else { "Void" },
        Span::call_site(),
    );

    let type_param = Ident::new(
        if ident == "TypeParam" {
            "__TypeParam"
        } else {
            "TypeParam"
        },
        Span::call_site(),
    );

    let mut generics = input.generics;
    let where_clause = generics.where_clause.take();
    let mut impl_generics = Vec::new();
    let mut ty_generics = Vec::new();
    let mut phantoms = Vec::new();
    for param in &mut generics.params {
        match param {
            GenericParam::Type(param) => {
                let ident = &param.ident;
                let elem = quote!(#ident);
                impl_generics.push(quote!(#ident: ?::core::marker::Sized));
                ty_generics.push(quote!(#ident));
                phantoms.push(variance::apply(param, elem, &type_param));
            }
            GenericParam::Lifetime(param) => {
                let lifetime = &param.lifetime;
                let elem = quote!(&#lifetime ());
                impl_generics.push(quote!(#lifetime));
                ty_generics.push(quote!(#lifetime));
                phantoms.push(variance::apply(param, elem, &type_param));
            }
            GenericParam::Const(param) => {
                let msg = "const generics are not supported";
                let err = Error::new_spanned(param, msg);
                return err.to_compile_error().into();
            }
        }
    }

    let impl_generics = &impl_generics;
    let ty_generics = &ty_generics;
    let enum_token = Token![enum](input.struct_token.span);
    let struct_token = input.struct_token;

    TokenStream::from(quote! {
        #[cfg(not(doc))]
        mod #void_namespace {
            enum #void {}
            impl ::core::marker::Copy for #void {}
            #[allow(clippy::expl_impl_clone_on_copy)]
            impl ::core::clone::Clone for #void {
                fn clone(&self) -> Self {
                    match *self {}
                }
            }

            #[repr(C, packed)]
            struct #type_param<T: ?::core::marker::Sized>([*const T; 0]);
            impl<T: ?::core::marker::Sized> ::core::marker::Copy for #type_param<T> {}
            #[allow(clippy::expl_impl_clone_on_copy)]
            impl<T: ?::core::marker::Sized> ::core::clone::Clone for #type_param<T> {
                fn clone(&self) -> Self {
                    *self
                }
            }
            unsafe impl<T: ?::core::marker::Sized + ::core::marker::Send> ::core::marker::Send for #type_param<T> {}
            unsafe impl<T: ?::core::marker::Sized + ::core::marker::Sync> ::core::marker::Sync for #type_param<T> {}

            #[allow(non_camel_case_types)]
            #vis_super struct #ident <#(#impl_generics),*> (
                #(
                    self::#type_param<#phantoms>,
                )*
                self::#void,
            );

            impl <#(#impl_generics),*> ::core::marker::Copy
            for #ident <#(#ty_generics),*> {}

            #[allow(clippy::expl_impl_clone_on_copy)]
            impl <#(#impl_generics),*> ::core::clone::Clone
            for #ident <#(#ty_generics),*> {
                fn clone(&self) -> Self {
                    *self
                }
            }
        }

        #[cfg(not(doc))]
        mod #value_namespace {
            #vis_super use super::#ident::#ident;
        }

        #[cfg(not(doc))]
        #(#attrs)*
        #vis #enum_token #ident #generics #where_clause {
            __Phantom(#void_namespace::#ident <#(#ty_generics),*>),
            #ident,
        }

        #[cfg(not(doc))]
        #[doc(hidden)]
        #vis use self::#value_namespace::*;

        #[cfg(doc)]
        #(#attrs)*
        #vis #struct_token #ident #generics #where_clause;

        #derives
    })
}
