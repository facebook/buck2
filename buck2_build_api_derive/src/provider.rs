/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use convert_case::{Case, Casing};
use quote::{format_ident, quote};

pub(crate) struct InternalProviderArgs {
    creator_func: syn::Ident,
}

impl syn::parse::Parse for InternalProviderArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let creator_func = syn::Ident::parse(input)?;
        Ok(InternalProviderArgs { creator_func })
    }
}

struct ProviderCodegen {
    input: syn::ItemStruct,
    args: InternalProviderArgs,
}

impl ProviderCodegen {
    fn name(&self) -> syn::Result<syn::Ident> {
        match self.input.ident.to_string().strip_suffix("Gen") {
            Some(v) => Ok(format_ident!("{}", v)),
            None => Err(syn::Error::new_spanned(
                &self.input.ident,
                "should end with Gen",
            )),
        }
    }

    fn name_str(&self) -> syn::Result<String> {
        Ok(self.name()?.to_string())
    }

    fn name_snake_str(&self) -> syn::Result<String> {
        Ok(self.name_str()?.to_case(Case::Snake))
    }

    fn frozen_name(&self) -> syn::Result<syn::Ident> {
        let name = self.name()?;
        Ok(format_ident!("Frozen{}", name))
    }

    fn callable_name(&self) -> syn::Result<syn::Ident> {
        let name = self.name()?;
        Ok(format_ident!("{}Callable", name))
    }

    fn register_func_name(&self) -> syn::Result<syn::Ident> {
        let name_snake_str = self.name_snake_str()?;
        Ok(format_ident!("register_{}", name_snake_str))
    }

    fn field_names(&self) -> syn::Result<Vec<&syn::Ident>> {
        match &self.input.fields {
            syn::Fields::Named(fields) => Ok(fields
                .named
                .iter()
                .map(|v| v.ident.as_ref().expect("no field name in named fields?"))
                .collect()),
            _ => Err(syn::Error::new_spanned(
                &self.input,
                "providers only support named fields",
            )),
        }
    }

    fn impl_display(&self) -> syn::Result<proc_macro2::TokenStream> {
        let gen_name = &self.input.ident;
        let name_str = self.name_str()?;
        let field_names = self.field_names()?;
        Ok(quote! {
            impl<V: std::fmt::Display> std::fmt::Display for #gen_name<V> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    starlark::values::display::display_keyed_container(
                        f,
                        &format!("{}(", #name_str),
                        ")",
                        "=",
                        [
                            #((stringify!(#field_names), &self.#field_names)),*
                        ].into_iter()
                    )
                }
            }
        })
    }

    fn impl_starlark_value(&self) -> syn::Result<proc_macro2::TokenStream> {
        let vis = &self.input.vis;
        let gen_name = &self.input.ident;
        let name = self.name()?;
        let name_str = self.name_str()?;
        let register_func_name = self.register_func_name()?;
        Ok(quote! {
            starlark::starlark_complex_value!(#vis #name);

            impl<'v, V: starlark::values::ValueLike<'v> + 'v> starlark::values::StarlarkValue<'v>
                for #gen_name<V>
            where
                Self: gazebo::any::ProvidesStaticType,
            {
                starlark::starlark_type!(#name_str);

                fn matches_type(&self, ty: &str) -> bool {
                    ty == #name_str || ty == "provider"
                }

                fn get_methods(&self) -> Option<&'static starlark::environment::Methods> {
                    static RES: starlark::environment::MethodsStatic =
                        starlark::environment::MethodsStatic::new();

                    RES.methods(|x| {
                        crate::interpreter::rule_defs::provider::provider_methods(x);
                        _register::#register_func_name(x);
                    })
                }

                // TODO(cjhopman): UserProvider implements more of the starlark functions. We should probably match them.
            }
        })
    }

    fn impl_serializable_value(&self) -> syn::Result<proc_macro2::TokenStream> {
        let gen_name = &self.input.ident;
        let field_names = self.field_names()?;
        let field_len = field_names.len();
        Ok(quote! {
            impl<'v, V: starlark::values::ValueLike<'v>> serde::Serialize
                for #gen_name<V>
            {
                fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error> where S : serde::Serializer {
                    use serde::ser::SerializeMap;

                    let mut s = s.serialize_map(Some(#field_len))?;
                    #(
                        s.serialize_entry(
                            stringify!(#field_names),
                            &self.#field_names
                        )?;
                    )*
                    s.end()
                }
            }
        })
    }

    fn from_providers(&self) -> syn::Result<proc_macro2::TokenStream> {
        let gen_name = &self.input.ident;
        let frozen_name = self.frozen_name()?;
        let callable_name = self.callable_name()?;
        Ok(quote! {
            impl<'v, V: starlark::values::ValueLike<'v>> #gen_name<V> {
                pub fn from_providers(
                    providers: &crate::interpreter::rule_defs::provider::FrozenProviderCollection,
                ) -> Option<starlark::values::FrozenRef<#frozen_name>> {
                    providers.get_provider(#callable_name::provider_id_t())
                }
            }
        })
    }

    fn impl_provider_like(&self) -> syn::Result<proc_macro2::TokenStream> {
        let gen_name = &self.input.ident;
        let field_names = self.field_names()?;
        let callable_name = self.callable_name()?;
        Ok(quote! {
            impl<'v, V: starlark::values::ValueLike<'v> + 'v> crate::interpreter::rule_defs::provider::ProviderLike<'v> for #gen_name<V>
            where
                Self: std::fmt::Debug,
            {
                fn id(&self) -> &std::sync::Arc<crate::interpreter::rule_defs::provider::ProviderId> {
                    #callable_name::provider_id()
                }

                fn get_field(&self, name: &str) -> Option<starlark::values::Value<'v>> {
                    match name {
                        #(stringify!(#field_names) => Some(self.#field_names.to_value()),)*
                        _ => None,
                    }
                }

                fn items(&self) -> Vec<(&str, starlark::values::Value<'v>)> {
                    vec![
                        #((stringify!(#field_names), self.#field_names.to_value())),*
                    ]
                }
            }
        })
    }

    fn callable_struct(&self) -> syn::Result<proc_macro2::TokenStream> {
        let vis = &self.input.vis;
        let callable_name = self.callable_name()?;
        Ok(quote! {
            #[derive(Debug, Clone, gazebo::dupe::Dupe, gazebo::any::ProvidesStaticType, starlark::values::NoSerialize)]
            #vis struct #callable_name {
                id: &'static std::sync::Arc<crate::interpreter::rule_defs::provider::ProviderId>,
            }
        })
    }

    fn callable_impl_starlark_value(&self) -> syn::Result<proc_macro2::TokenStream> {
        let name_str = self.name_str()?;
        let field_names = self.field_names()?;
        let create_func = &self.args.creator_func;
        let callable_name = self.callable_name()?;
        let callable_name_snake_str = callable_name.to_string().to_case(Case::Snake);

        Ok(quote! {
            starlark::starlark_simple_value!(#callable_name);

            impl<'v> starlark::values::StarlarkValue<'v> for #callable_name
            {
                starlark::starlark_type!(#callable_name_snake_str);

                fn get_methods(&self) -> Option<&'static starlark::environment::Methods> {
                    static RES: starlark::environment::MethodsStatic =
                        starlark::environment::MethodsStatic::new();
                    // TODO(nmj): This should use the docstring from the attribute, rather than
                    //            None
                    RES.methods(|x| x.set_attribute("type", #name_str, None))
                }

                fn invoke(
                    &self,
                    _me: starlark::values::Value<'v>,
                    args: &starlark::eval::Arguments<'v, '_>,
                    eval: &mut starlark::eval::Evaluator<'v, '_>,
                ) -> anyhow::Result<starlark::values::Value<'v>> {
                    static RES: starlark::environment::GlobalsStatic =
                        starlark::environment::GlobalsStatic::new();
                    starlark::values::ValueLike::invoke(
                        RES.function(#create_func), args, eval)
                }

                fn documentation(&self) -> Option<starlark::values::docs::DocItem> {
                    // TODO(nmj): Pull docstrings from attributes of the struct and
                    //            for the main struct
                    let field_names = [
                        #(stringify!(#field_names).to_owned()),*
                    ];
                    let field_docs = vec![None;field_names.len()];
                    use crate::interpreter::rule_defs::provider::ProviderCallableLike;
                    self.provider_callable_documentation(&None, &field_names, &field_docs)
                }
            }
        })
    }

    fn callable_impl_provider_callable_like(&self) -> syn::Result<proc_macro2::TokenStream> {
        let callable_name = self.callable_name()?;
        Ok(quote! {
            impl crate::interpreter::rule_defs::provider::ProviderCallableLike for #callable_name {
                fn id(&self) -> Option<&std::sync::Arc<crate::interpreter::rule_defs::provider::ProviderId>> {
                    Some(self.id)
                }
            }
        })
    }

    fn callable_impl(&self) -> syn::Result<proc_macro2::TokenStream> {
        let callable_name = self.callable_name()?;
        let vis = &self.input.vis;
        let name_str = self.name_str()?;
        let frozen_name = self.frozen_name()?;

        Ok(quote! {
            impl #callable_name {
                #vis fn provider_id()
                -> &'static std::sync::Arc<crate::interpreter::rule_defs::provider::ProviderId> {
                    Self::provider_id_t().id()
                }

                #vis fn provider_id_t() -> &'static std::sync::Arc<
                    crate::interpreter::rule_defs::provider::ProviderIdWithType<#frozen_name>,
                > {
                    static PROVIDER_ID_T: once_cell::sync::OnceCell<
                        std::sync::Arc<
                            crate::interpreter::rule_defs::provider::ProviderIdWithType<#frozen_name>,
                        >
                    > = once_cell::sync::OnceCell::new();
                    PROVIDER_ID_T.get_or_init(|| {
                        std::sync::Arc::new(
                            crate::interpreter::rule_defs::provider::ProviderIdWithType::new(
                                None,
                                #name_str.to_owned(),
                            ),
                        )
                    })
                }

                #vis fn new() -> Self {
                    Self {
                        id: Self::provider_id(),
                    }
                }
            }
        })
    }

    fn callable_impl_display(&self) -> syn::Result<proc_macro2::TokenStream> {
        let callable_name = self.callable_name()?;
        let name_str = self.name_str()?;
        Ok(quote! {
            impl std::fmt::Display for #callable_name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}()", #name_str)
                }
            }
        })
    }

    fn callable(&self) -> syn::Result<proc_macro2::TokenStream> {
        let gen = vec![
            self.callable_struct()?,
            self.callable_impl()?,
            self.callable_impl_display()?,
            self.callable_impl_starlark_value()?,
            self.callable_impl_provider_callable_like()?,
        ];

        Ok(quote! {
            #(#gen)*
        })
    }

    fn register(&self) -> syn::Result<proc_macro2::TokenStream> {
        let register_func_name = self.register_func_name()?;
        let field_names = self.field_names()?;
        let name = self.name()?;
        let name_str = self.name_str()?;
        let callable_name = self.callable_name()?;

        Ok(quote! {
            // workaround starlark requiring that GlobalsBuilder is unqualified
            mod _register {
                use super::*;
                use starlark::environment::MethodsBuilder;

                #[starlark_module]
                pub(crate) fn #register_func_name(builder: &mut MethodsBuilder) {
                    #(
                        #[starlark(attribute)]
                        fn #field_names<'v>(this: & #name) -> anyhow::Result<starlark::values::Value<'v>> {
                            Ok(this.#field_names)
                        }
                    )*
                }
            }

            fn register_provider(builder: &mut starlark::environment::GlobalsBuilder) {
                builder.set(#name_str, #callable_name::new());
            }
        })
    }

    fn inventory(&self) -> syn::Result<proc_macro2::TokenStream> {
        let callable_name = self.callable_name()?;
        let name = self.name()?;
        Ok(quote! {
            inventory::submit! {
                crate::interpreter::rule_defs::provider::ProviderRegistration {
                    as_provider_callable: |v| {
                        starlark::values::ValueLike::downcast_ref::<#callable_name>(v).map(
                            |o| o as &dyn crate::interpreter::rule_defs::provider::ProviderCallableLike)
                    },
                    as_provider: |v| {
                        <& #name as starlark::values::UnpackValue>::unpack_value(v).map(
                            |o| o as &dyn crate::interpreter::rule_defs::provider::ProviderLike)
                    },
                    register_globals: |globals| {
                        register_provider(globals)
                    }
                }
            }
        })
    }
}

pub(crate) fn define_provider(
    args: InternalProviderArgs,
    input: syn::ItemStruct,
) -> syn::Result<proc_macro::TokenStream> {
    let codegen = ProviderCodegen { input, args };

    if let Some(where_clause) = codegen.input.generics.where_clause {
        return Err(syn::Error::new_spanned(
            where_clause,
            "should have no where clauses",
        ));
    }
    if let Some(const_param) = codegen.input.generics.const_params().next() {
        return Err(syn::Error::new_spanned(
            const_param,
            "should have no const params",
        ));
    }
    if let Some(lifetime) = codegen.input.generics.lifetimes().next() {
        return Err(syn::Error::new_spanned(
            lifetime,
            "should have no lifetime params",
        ));
    }
    let mut type_params: Vec<_> = codegen.input.generics.type_params().collect();
    if type_params.len() != 1 {
        return Err(syn::Error::new_spanned(
            codegen.input,
            "should have exactly one type param",
        ));
    }
    let type_param = type_params.pop().unwrap();
    if let Some(bound) = type_param.bounds.iter().next() {
        return Err(syn::Error::new_spanned(
            bound,
            "type param should have no bounds",
        ));
    }

    // TODO(cjhopman): Verify `V` type param as expected
    // TODO(cjhopman): Verify all fields are type `V`

    let input = &codegen.input;

    let gen = vec![
        quote! { #input },
        codegen.impl_display()?,
        codegen.impl_starlark_value()?,
        codegen.impl_serializable_value()?,
        codegen.from_providers()?,
        codegen.impl_provider_like()?,
        codegen.callable()?,
        codegen.register()?,
        codegen.inventory()?,
    ];
    let gen = quote! { #(#gen)* };

    Ok(gen.into())
}
