/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use convert_case::Case;
use convert_case::Casing;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use syn::parse::ParseStream;
use syn::Attribute;
use syn::Fields;
use syn::LitStr;

const PROVIDER_IDENT: &str = "provider";

pub(crate) struct InternalProviderArgs {
    creator_func: syn::Ident,
}

impl syn::parse::Parse for InternalProviderArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let creator_func = syn::Ident::parse(input)?;
        Ok(InternalProviderArgs { creator_func })
    }
}

/// Documentation information for a single field.
struct FieldDoc {
    /// The name of the field
    name: syn::Ident,
    /// The docstring for the field, if present
    docstring: proc_macro2::TokenStream,
    /// The implementation to generate the field type documentation.
    field_type: proc_macro2::TokenStream,
}

struct ProviderCodegen {
    input: syn::ItemStruct,
    args: InternalProviderArgs,
    field_attr_providers: HashMap<syn::Ident, Attribute>,
}

impl ProviderCodegen {
    /// Create an instance from an input and args.
    ///
    /// This modifies the original input and removes any instances of `#[provider()]` macros
    /// on fields of the provided structs, and saves them into `field_attr_providers`.
    fn new(mut input: syn::ItemStruct, args: InternalProviderArgs) -> syn::Result<Self> {
        let mut provider_attrs = HashMap::new();
        if let Fields::Named(fields_named) = &mut input.fields {
            for field in fields_named.named.iter_mut() {
                let (attrs, mut provider_attr): (Vec<syn::Attribute>, Vec<syn::Attribute>) = field
                    .attrs
                    .clone()
                    .into_iter()
                    .partition(|a| !a.path.is_ident(PROVIDER_IDENT));
                field.attrs = attrs;
                if provider_attr.len() > 1 {
                    return Err(syn::Error::new_spanned(
                        field.to_token_stream(),
                        &format!("{} attribute can only be specified once", PROVIDER_IDENT),
                    ));
                } else if !provider_attr.is_empty() {
                    provider_attrs.insert(
                        field.ident.as_ref().unwrap().to_owned(),
                        provider_attr.remove(0),
                    );
                }
            }
        };
        Ok(Self {
            input,
            args,
            field_attr_providers: provider_attrs,
        })
    }

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

    /// Parse the "doc" attribute and return a tokenstream that is either None if "doc" is not
    /// present, or the result of DocString::parse_docstring if present.
    fn get_docstring_impl(&self, attrs: &Vec<syn::Attribute>) -> proc_macro2::TokenStream {
        let mut doc_lines = vec![];

        for attr in attrs {
            if attr.path.is_ident("doc") {
                if let Ok(syn::Meta::NameValue(syn::MetaNameValue {
                    lit: syn::Lit::Str(s),
                    ..
                })) = attr.parse_meta()
                {
                    doc_lines.push(s.value());
                }
            }
        }

        if doc_lines.is_empty() {
            quote! { None }
        } else {
            let docstring = Some(doc_lines.join("\n"));
            quote! {
                starlark::values::docs::DocString::from_docstring(
                    starlark::values::docs::DocStringKind::Rust,
                    #docstring,
                )
            }
        }
    }

    fn field_doc(&self, field: &syn::Field) -> syn::Result<FieldDoc> {
        syn::custom_keyword!(field_type);

        let name = field.ident.as_ref().unwrap().to_owned();

        let field_type = if let Some(attr) = self.field_attr_providers.get(&name) {
            attr.parse_args_with(
                |input: ParseStream| -> syn::Result<proc_macro2::TokenStream> {
                    if input.parse::<field_type>().is_ok() {
                        input.parse::<syn::Token![=]>()?;
                        let rust_type = input.parse::<LitStr>()?.value();
                        let rust_type: proc_macro2::TokenStream = rust_type.parse()?;
                        Ok(quote! {
                            Some(starlark::values::docs::Type {
                                raw_type: <#rust_type>::starlark_type_repr(),
                            })
                        })
                    } else {
                        Ok(quote! { None })
                    }
                },
            )?
        } else {
            quote! { None }
        };

        let docstring = self.get_docstring_impl(&field.attrs);

        Ok(FieldDoc {
            name,
            docstring,
            field_type,
        })
    }

    /// Grab the information for all fields on the struct, and create the
    /// documentation() function for StarlarkValue.
    fn documentation_function(&self) -> syn::Result<proc_macro2::TokenStream> {
        let provider_docstring = self.get_docstring_impl(&self.input.attrs);

        let field_docs = match &self.input.fields {
            syn::Fields::Named(fields) => Ok(fields
                .named
                .iter()
                .map(|f| self.field_doc(f))
                .collect::<syn::Result<Vec<_>>>()?),
            _ => Err(syn::Error::new_spanned(
                &self.input,
                "providers only support named fields",
            )),
        }?;

        let mut field_names = vec![];
        let mut field_docstrings = vec![];
        let mut field_types = vec![];

        for doc in field_docs {
            let name = doc.name;
            let name = quote! { stringify!(#name) };

            field_names.push(name);
            field_docstrings.push(doc.docstring);
            field_types.push(doc.field_type);
        }

        Ok(quote! {
            fn documentation(&self) -> Option<starlark::values::docs::DocItem> {
                let docstring = #provider_docstring;
                let field_names = [
                    #(#field_names.to_owned()),*
                ];
                let field_docs = [
                    #(#field_docstrings),*
                ];
                let field_types = [
                    #(#field_types),*
                ];
                use starlark::values::type_repr::StarlarkTypeRepr;
                use buck2_interpreter_for_build::provider::callable::ProviderCallableLike;
                self.provider_callable_documentation(&docstring, &field_names, &field_docs, &field_types)
            }
        })
    }

    fn impl_display(&self) -> syn::Result<proc_macro2::TokenStream> {
        let gen_name = &self.input.ident;
        let name_str = self.name_str()?;
        let field_names = self.field_names()?;
        Ok(quote! {
            impl<V: std::fmt::Display> std::fmt::Display for #gen_name<V> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    gazebo::display::display_keyed_container(
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

                fn get_methods() -> Option<&'static starlark::environment::Methods> {
                    static RES: starlark::environment::MethodsStatic =
                        starlark::environment::MethodsStatic::new();

                    RES.methods(|x| {
                        crate::interpreter::rule_defs::provider::provider_methods(x);
                        _register::#register_func_name(x);
                    })
                }

                fn provide(&'v self, demand: &mut starlark::values::Demand<'_, 'v>) {
                    demand.provide_value::<
                        &dyn crate::interpreter::rule_defs::provider::ProviderLike>(self);
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
                    providers: &crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection,
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
                fn id(&self) -> &std::sync::Arc<buck2_core::provider::id::ProviderId> {
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
            #[derive(Debug, Clone, gazebo::dupe::Dupe, gazebo::any::ProvidesStaticType, starlark::values::NoSerialize, allocative::Allocative)]
            #vis struct #callable_name {
                id: &'static std::sync::Arc<buck2_core::provider::id::ProviderId>,
            }
        })
    }

    fn callable_impl_starlark_value(&self) -> syn::Result<proc_macro2::TokenStream> {
        let name_str = self.name_str()?;
        let documentation_function = self.documentation_function()?;
        let create_func = &self.args.creator_func;
        let callable_name = self.callable_name()?;
        let callable_name_snake_str = callable_name.to_string().to_case(Case::Snake);

        Ok(quote! {
            starlark::starlark_simple_value!(#callable_name);

            impl<'v> starlark::values::StarlarkValue<'v> for #callable_name
            {
                starlark::starlark_type!(#callable_name_snake_str);

                fn get_methods() -> Option<&'static starlark::environment::Methods> {
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

                fn provide(&'v self, demand: &mut starlark::values::Demand<'_, 'v>) {
                    demand.provide_value::<
                        &dyn buck2_interpreter_for_build::provider::callable::ProviderCallableLike>(self);
                }

                #documentation_function
            }
        })
    }

    fn callable_impl_provider_callable_like(&self) -> syn::Result<proc_macro2::TokenStream> {
        let callable_name = self.callable_name()?;
        Ok(quote! {
            impl buck2_interpreter_for_build::provider::callable::ProviderCallableLike for #callable_name {
                fn id(&self) -> Option<&std::sync::Arc<buck2_core::provider::id::ProviderId>> {
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
                -> &'static std::sync::Arc<buck2_core::provider::id::ProviderId> {
                    Self::provider_id_t().id()
                }

                #vis fn provider_id_t() -> &'static std::sync::Arc<
                    buck2_core::provider::id::ProviderIdWithType<#frozen_name>,
                > {
                    static PROVIDER_ID_T: once_cell::sync::OnceCell<
                        std::sync::Arc<
                            buck2_core::provider::id::ProviderIdWithType<#frozen_name>,
                        >
                    > = once_cell::sync::OnceCell::new();
                    PROVIDER_ID_T.get_or_init(|| {
                        std::sync::Arc::new(
                            buck2_core::provider::id::ProviderIdWithType::new(
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

                #[starlark::starlark_module]
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
        Ok(quote! {
            inventory::submit! {
                crate::interpreter::rule_defs::provider::registration::ProviderRegistration {
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
    let codegen = ProviderCodegen::new(input, args)?;

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
