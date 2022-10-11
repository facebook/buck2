use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{Attribute, LifetimeDef, TypeParam};

enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}

pub trait HasVarianceAttribute {
    fn attrs(&mut self) -> &mut Vec<Attribute>;
}

impl HasVarianceAttribute for TypeParam {
    fn attrs(&mut self) -> &mut Vec<Attribute> {
        &mut self.attrs
    }
}

impl HasVarianceAttribute for LifetimeDef {
    fn attrs(&mut self) -> &mut Vec<Attribute> {
        &mut self.attrs
    }
}

pub fn apply(
    param: &mut dyn HasVarianceAttribute,
    base: TokenStream,
    type_param: &Ident,
) -> TokenStream {
    let mut variance = Variance::Covariant;

    let attrs = param.attrs();
    *attrs = attrs
        .drain(..)
        .filter(|attr| {
            if attr.path.is_ident("contra") && attr.tokens.is_empty() {
                variance = Variance::Contravariant;
                false
            } else if attr.path.is_ident("invariant") && attr.tokens.is_empty() {
                variance = Variance::Invariant;
                false
            } else {
                true
            }
        })
        .collect();

    let phantom = quote!(self::#type_param<#base>);
    match variance {
        Variance::Covariant => base,
        Variance::Contravariant => quote!(fn(#phantom)),
        Variance::Invariant => quote!(fn(#phantom) -> #phantom),
    }
}
