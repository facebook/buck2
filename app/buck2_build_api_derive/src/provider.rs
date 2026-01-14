/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use convert_case::Case;
use convert_case::Casing;
use gazebo::prelude::*;
use quote::ToTokens;
use quote::format_ident;
use quote::quote;
use syn::Fields;
use syn::TypeParamBound;

pub(crate) struct InternalProviderArgs {
    creator_func: syn::Ident,
    methods_func: Option<syn::Ident>,
}

impl syn::parse::Parse for InternalProviderArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let creator_func = syn::Ident::parse(input)?;

        // Parse optional methods parameter
        let methods_func = if input.peek(syn::Token![,]) {
            input.parse::<syn::Token![,]>()?;

            // Check if it's methods = ...
            if input.peek(syn::Ident) {
                let key = syn::Ident::parse(input)?;
                if key == "methods" {
                    input.parse::<syn::Token![=]>()?;
                    Some(syn::Ident::parse(input)?)
                } else {
                    return Err(syn::Error::new_spanned(key, "expected 'methods' parameter"));
                }
            } else {
                None
            }
        } else {
            None
        };

        Ok(InternalProviderArgs {
            creator_func,
            methods_func,
        })
    }
}

/// Provider field information.
/// This does not include the `id` field.
struct Field {
    /// The name of the field
    name: syn::Ident,
    /// The docstring for the field, if present
    docstring: syn::Expr,
    /// Field type as specified in the `#[provider(field_type = SomeType)]` attribute.
    field_type: syn::Type,
}

impl Field {
    /// Expression which produces `Ty` for the field.
    fn field_type_ty(&self) -> syn::Expr {
        let field_type = &self.field_type;
        syn::parse_quote_spanned! { self.name.span() =>
            <#field_type as starlark::values::type_repr::StarlarkTypeRepr>::starlark_type_repr()
        }
    }
}

struct ProviderCodegen {
    span: proc_macro2::Span,
    input: syn::ItemStruct,
    args: InternalProviderArgs,
}

impl ProviderCodegen {
    /// Create an instance from an input and args.
    ///
    /// This modifies the original input and removes any instances of `#[provider()]` macros
    /// on fields of the provided structs, and saves them into `field_attr_providers`.
    fn new(mut input: syn::ItemStruct, args: InternalProviderArgs) -> syn::Result<Self> {
        if let Fields::Named(fields_named) = &mut input.fields {
            for field in fields_named.named.iter_mut() {
                field.attrs = field.attrs.clone();
            }
        };
        Ok(Self {
            span: input.ident.span(),
            input,
            args,
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

    fn provider_methods_func_name(&self) -> syn::Result<syn::Ident> {
        let name_snake_str = self.name_snake_str()?;
        Ok(format_ident!("{}_methods", name_snake_str))
    }

    /// `id` field is object identity, which is ignored for equality or display purposes.
    fn is_id_field(&self, field: &syn::Field) -> bool {
        field.ident.as_ref().unwrap() == "id" && field.ty.to_token_stream().to_string() == "u64"
    }

    fn field_names(&self) -> syn::Result<Vec<syn::Ident>> {
        Ok(self.fields()?.into_map(|f| f.name))
    }

    /// Parse the "doc" attribute and return a tokenstream that is either None if "doc" is not
    /// present, or the result of DocString::parse_docstring if present.
    fn get_docstring_impl(&self, attrs: &Vec<syn::Attribute>) -> syn::Expr {
        let mut doc_lines = vec![];

        for attr in attrs {
            if attr.path().is_ident("doc") {
                if let syn::Meta::NameValue(syn::MetaNameValue {
                    value:
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(s),
                            ..
                        }),
                    ..
                }) = &attr.meta
                {
                    doc_lines.push(s.value());
                }
            }
        }

        if doc_lines.is_empty() {
            syn::parse_quote_spanned! { self.span=> None }
        } else {
            let docstring = Some(doc_lines.join("\n"));
            syn::parse_quote_spanned! { self.span=>
                starlark::docs::DocString::from_docstring(
                    starlark::docs::DocStringKind::Rust,
                    #docstring,
                )
            }
        }
    }

    fn field(&self, field: &syn::Field) -> syn::Result<Field> {
        if self.is_id_field(field) {
            return Err(syn::Error::new_spanned(
                field,
                "id field should not be documented",
            ));
        }

        let error = "Field type must be `ValueOfUncheckedGeneric<V, SomeType>`";

        let syn::Type::Path(ty) = &field.ty else {
            return Err(syn::Error::new_spanned(field, error));
        };
        let syn::TypePath {
            qself: None,
            path:
                syn::Path {
                    leading_colon: None,
                    segments,
                },
        } = ty
        else {
            return Err(syn::Error::new_spanned(field, error));
        };
        let [
            syn::PathSegment {
                ident,
                arguments: syn::PathArguments::AngleBracketed(args),
            },
        ] = Vec::from_iter(segments).as_slice()
        else {
            return Err(syn::Error::new_spanned(field, error));
        };
        if ident != "ValueOfUncheckedGeneric" {
            return Err(syn::Error::new_spanned(field, error));
        }
        let [
            syn::GenericArgument::Type(v),
            syn::GenericArgument::Type(field_type),
        ] = Vec::from_iter(&args.args).as_slice()
        else {
            return Err(syn::Error::new_spanned(field, error));
        };
        let expected_v: syn::Type = syn::parse_quote!(V);
        if v != &expected_v {
            return Err(syn::Error::new_spanned(field, error));
        }

        let name = field.ident.as_ref().unwrap().to_owned();

        let docstring = self.get_docstring_impl(&field.attrs);

        Ok(Field {
            name,
            docstring,
            field_type: field_type.clone(),
        })
    }

    fn fields(&self) -> syn::Result<Vec<Field>> {
        match &self.input.fields {
            syn::Fields::Named(fields) => Ok(fields
                .named
                .iter()
                .filter(|f| !self.is_id_field(f))
                .map(|f| self.field(f))
                .collect::<syn::Result<Vec<_>>>()?),
            _ => Err(syn::Error::new_spanned(
                &self.input,
                "providers only support named fields",
            )),
        }
    }

    /// Grab the information for all fields on the struct, and create the
    /// documentation() function for StarlarkValue.
    fn documentation_function(&self) -> syn::Result<syn::ImplItemFn> {
        let provider_docstring = self.get_docstring_impl(&self.input.attrs);
        let create_func = &self.args.creator_func;

        // Generate the documentation function based on whether custom methods are provided
        if let Some(ref custom_methods) = self.args.methods_func {
            // When using custom methods, we don't need the field arrays
            Ok(syn::parse_quote_spanned! {self.span=>
                fn documentation(&self) -> starlark::docs::DocItem {
                    let docstring = #provider_docstring;
                    buck2_build_api::interpreter::rule_defs::provider::doc::provider_callable_documentation(
                        Some(#create_func),
                        buck2_build_api::interpreter::rule_defs::provider::doc::ProviderMembersSource::FromMethods(#custom_methods),
                        BUILTIN_PROVIDER_TY.instance(),
                        &docstring,
                    )
                }
            })
        } else {
            // When not using custom methods, generate field arrays
            let field_docs = self.fields()?;

            let mut field_names = vec![];
            let mut field_docstrings = vec![];
            let mut field_types = vec![];

            for doc in &field_docs {
                let name = &doc.name;
                let name = quote! { stringify!(#name) };

                field_names.push(name);
                field_docstrings.push(&doc.docstring);
                field_types.push(doc.field_type_ty());
            }

            Ok(syn::parse_quote_spanned! {self.span=>
                fn documentation(&self) -> starlark::docs::DocItem {
                    let docstring = #provider_docstring;
                    let field_names: [&str; _] = [
                        #(#field_names),*
                    ];
                    let field_docs: [std::option::Option<starlark::docs::DocString>; _] = [
                        #(#field_docstrings),*
                    ];
                    let field_types: [starlark::typing::Ty; _] = [
                        #(#field_types),*
                    ];
                    buck2_build_api::interpreter::rule_defs::provider::doc::provider_callable_documentation(
                        Some(#create_func),
                        buck2_build_api::interpreter::rule_defs::provider::doc::ProviderMembersSource::FromFields {
                            fields: &field_names,
                            field_docs: &field_docs,
                            field_types: &field_types,
                        },
                        BUILTIN_PROVIDER_TY.instance(),
                        &docstring,
                    )
                }
            })
        }
    }

    fn builtin_provider_ty(&self) -> syn::Result<syn::Item> {
        let gen_name = &self.input.ident;
        let callable_name = self.callable_name()?;
        Ok(syn::parse_quote_spanned! { self.span =>
            static BUILTIN_PROVIDER_TY: buck2_build_api::interpreter::rule_defs::provider::builtin::ty::BuiltinProviderTy<
                    #gen_name<starlark::values::Value>,
                    #callable_name,
            > =
                buck2_build_api::interpreter::rule_defs::provider::builtin::ty::BuiltinProviderTy::new();
        })
    }

    fn typechecker_ty_function(&self) -> syn::Result<syn::Item> {
        let creator_func = &self.args.creator_func;
        Ok(syn::parse_quote_spanned! {
            self.span=>
            fn typechecker_ty(&self) -> Option<starlark::typing::Ty> {
                Some(BUILTIN_PROVIDER_TY.callable(#creator_func))
            }
        })
    }

    fn impl_display(&self) -> syn::Result<syn::Item> {
        let gen_name = &self.input.ident;
        let name_str = self.name_str()?;
        let field_names = self.field_names()?;
        Ok(syn::parse_quote_spanned! { self.span=>
            impl<V: starlark::values::ValueLifetimeless> std::fmt::Display for #gen_name<V> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    buck2_build_api::__derive_refs::display_container::fmt_keyed_container(
                        f,
                        &format!("{}(", #name_str),
                        ")",
                        "=",
                        [
                            #((stringify!(#field_names), &self.#field_names.get())),*
                        ]
                    )
                }
            }
        })
    }

    fn impl_starlark_value(&self) -> syn::Result<Vec<syn::Item>> {
        let vis = &self.input.vis;
        let gen_name = &self.input.ident;
        let name = self.name()?;
        let name_str = self.name_str()?;
        // Use custom methods function if provided, otherwise use auto-generated one
        let provider_methods_func_name = if let Some(ref custom) = self.args.methods_func {
            custom.clone()
        } else {
            self.provider_methods_func_name()?
        };
        let field_names = self.field_names()?;
        Ok(vec![
            syn::parse_quote_spanned! { self.span=>
                starlark::starlark_complex_value!(#vis #name);
            },
            syn::parse_quote_spanned! { self.span=>
                #[starlark::values::starlark_value(type = #name_str)]
                impl<'v, V: starlark::values::ValueLike<'v>> starlark::values::StarlarkValue<'v>
                    for #gen_name<V>
                where
                    Self: starlark::any::ProvidesStaticType<'v>,
                {
                    fn get_methods() -> Option<&'static starlark::environment::Methods> {
                        static RES: starlark::environment::MethodsStatic =
                            starlark::environment::MethodsStatic::new();

                        RES.methods(|x| {
                            #provider_methods_func_name(x);
                        })
                    }

                    fn provide(&'v self, demand: &mut starlark::values::Demand<'_, 'v>) {
                        demand.provide_value::<
                            &dyn buck2_build_api::interpreter::rule_defs::provider::ProviderLike>(self);
                    }

                    fn equals(&self, other: starlark::values::Value<'v>) -> starlark::Result<bool> {
                        let this: &#name = starlark::coerce::coerce(self);
                        let other: &#name = match #name::from_value(other) {
                            Some(other) => other,
                            None => return Ok(false),
                        };

                        #(
                            if !this.#field_names.to_value().get().equals(other.#field_names.to_value().get())? {
                                return Ok(false);
                            }
                        )*
                        Ok(true)
                    }

                    fn get_type_starlark_repr() -> starlark::typing::Ty {
                        BUILTIN_PROVIDER_TY.instance()
                    }

                    // TODO(cjhopman): UserProvider implements more of the starlark functions. We should probably match them.
                }
            },
        ])
    }

    fn impl_serializable_value(&self) -> syn::Result<syn::Item> {
        let gen_name = &self.input.ident;
        let field_names = self.field_names()?;
        let field_len = field_names.len();
        Ok(syn::parse_quote_spanned! { self.span=>
            impl<'v, V: starlark::values::ValueLike<'v>> buck2_build_api::__derive_refs::serde::Serialize
                for #gen_name<V>
            {
                fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error> where S : buck2_build_api::__derive_refs::serde::Serializer {
                    use buck2_build_api::__derive_refs::serde::ser::SerializeMap;

                    let mut s = s.serialize_map(Some(#field_len))?;
                    #(
                        s.serialize_entry(
                            stringify!(#field_names),
                            &self.#field_names.get().to_value()
                        )?;
                    )*
                    s.end()
                }
            }
        })
    }

    fn impl_provider_like(&self) -> syn::Result<syn::Item> {
        let gen_name = &self.input.ident;
        let field_names = self.field_names()?;
        let callable_name = self.callable_name()?;
        Ok(syn::parse_quote_spanned! { self.span=>
            impl<'v, V: starlark::values::ValueLike<'v>> buck2_build_api::interpreter::rule_defs::provider::ProviderLike<'v> for #gen_name<V>
            where
                Self: std::fmt::Debug,
            {
                fn id(&self) -> &std::sync::Arc<buck2_core::provider::id::ProviderId> {
                    #callable_name::provider_id()
                }

                fn items(&self) -> Vec<(&str, starlark::values::Value<'v>)> {
                    vec![
                        #((stringify!(#field_names), self.#field_names.get().to_value())),*
                    ]
                }
            }
        })
    }

    fn impl_frozen_builtin_provider(&self) -> syn::Result<syn::Item> {
        let frozen_name = self.frozen_name()?;
        let callable_name = self.callable_name()?;
        Ok(syn::parse_quote_spanned! { self.span=>
            impl buck2_build_api::interpreter::rule_defs::provider::FrozenBuiltinProviderLike for #frozen_name {
                fn builtin_provider_id() -> &'static std::sync::Arc<buck2_core::provider::id::ProviderId> {
                    #callable_name::provider_id()
                }
            }
        })
    }

    fn callable_struct(&self) -> syn::Result<syn::Item> {
        let vis = &self.input.vis;
        let callable_name = self.callable_name()?;
        Ok(syn::parse_quote_spanned! { self.span=>
            #[derive(Debug, Clone, dupe::Dupe, starlark::any::ProvidesStaticType, starlark::values::NoSerialize, allocative::Allocative)]
            #vis struct #callable_name {
                id: &'static std::sync::Arc<buck2_core::provider::id::ProviderId>,
            }
        })
    }

    fn callable_impl_starlark_value(&self) -> syn::Result<Vec<syn::Item>> {
        let callable_name = self.callable_name()?;
        let documentation_function = self.documentation_function()?;
        let typechecker_ty_function = self.typechecker_ty_function()?;
        let create_func = &self.args.creator_func;
        let callable_name_snake_str = callable_name.to_string().to_case(Case::Snake);

        Ok(vec![
            syn::parse_quote_spanned! {self.span=>
                starlark::starlark_simple_value!(#callable_name);
            },
            syn::parse_quote_spanned! {self.span=>
                #[starlark::values::starlark_value(type = #callable_name_snake_str)]
                impl<'v> starlark::values::StarlarkValue<'v> for #callable_name
                {
                    fn invoke(
                        &self,
                        _me: starlark::values::Value<'v>,
                        args: &starlark::eval::Arguments<'v, '_>,
                        eval: &mut starlark::eval::Evaluator<'v, '_, '_>,
                    ) -> starlark::Result<starlark::values::Value<'v>> {
                        static RES: starlark::environment::GlobalsStatic =
                            starlark::environment::GlobalsStatic::new();
                        starlark::values::ValueLike::invoke(
                            RES.function(#create_func), args, eval)
                    }

                    fn provide(&'v self, demand: &mut starlark::values::Demand<'_, 'v>) {
                        demand.provide_value::<
                            &dyn buck2_interpreter::types::provider::callable::ProviderCallableLike>(self);
                    }

                    fn eval_type(&self) -> Option<starlark::typing::Ty> {
                        Some(BUILTIN_PROVIDER_TY.instance())
                    }

                    #documentation_function
                    #typechecker_ty_function
                }
            },
        ])
    }

    fn callable_impl_provider_callable_like(&self) -> syn::Result<syn::Item> {
        let callable_name = self.callable_name()?;
        Ok(syn::parse_quote_spanned! { self.span=>
            impl buck2_interpreter::types::provider::callable::ProviderCallableLike for #callable_name {
                fn id(&self) -> buck2_error::Result<&std::sync::Arc<buck2_core::provider::id::ProviderId>> {
                    Ok(self.id)
                }
            }
        })
    }

    fn callable_impl(&self) -> syn::Result<syn::Item> {
        let callable_name = self.callable_name()?;
        let vis = &self.input.vis;
        let name_str = self.name_str()?;
        let frozen_name = self.frozen_name()?;

        Ok(syn::parse_quote_spanned! { self.span=>
            impl #callable_name {
                #vis fn provider_id()
                -> &'static std::sync::Arc<buck2_core::provider::id::ProviderId> {
                    Self::provider_id_t().id()
                }

                #vis fn provider_id_t() -> &'static std::sync::Arc<
                    buck2_core::provider::id::ProviderIdWithType<#frozen_name>,
                > {
                    static PROVIDER_ID_T: std::sync::OnceLock<
                        std::sync::Arc<
                            buck2_core::provider::id::ProviderIdWithType<#frozen_name>,
                        >
                    > = std::sync::OnceLock::new();
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

    fn callable_impl_display(&self) -> syn::Result<syn::Item> {
        let callable_name = self.callable_name()?;
        let name_str = self.name_str()?;
        Ok(syn::parse_quote_spanned! { self.span=>
            impl std::fmt::Display for #callable_name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", #name_str)
                }
            }
        })
    }

    fn callable(&self) -> syn::Result<Vec<syn::Item>> {
        Ok([
            vec![self.callable_struct()?],
            vec![self.callable_impl()?],
            vec![self.callable_impl_display()?],
            self.callable_impl_starlark_value()?,
            vec![self.callable_impl_provider_callable_like()?],
        ]
        .into_iter()
        .flatten()
        .collect())
    }

    fn register(&self) -> syn::Result<Vec<syn::Item>> {
        let name = self.name()?;
        let name_str = self.name_str()?;
        let callable_name = self.callable_name()?;

        let mut items = vec![];

        // Only generate default provider_methods if no custom methods_func was provided
        if self.args.methods_func.is_none() {
            let provider_methods_func_name = self.provider_methods_func_name()?;
            let (field_names, field_types): (Vec<_>, Vec<_>) = self
                .fields()?
                .into_iter()
                .map(|f| (f.name, f.field_type))
                .unzip();

            items.push(syn::parse_quote_spanned! { self.span=>
                #[starlark::starlark_module]
                fn #provider_methods_func_name(builder: &mut starlark::environment::MethodsBuilder) {
                    #(
                        #[starlark(attribute)]
                        fn #field_names<'v>(this: &#name<'v>)
                                -> starlark::Result<starlark::values::ValueOfUnchecked<'v, #field_types>>
                        {
                            Ok(this.#field_names.to_value())
                        }
                    )*
                }
            });
        }

        items.push(syn::parse_quote_spanned! { self.span=>
            fn register_provider(builder: &mut starlark::environment::GlobalsBuilder) {
                builder.set(#name_str, #callable_name::new());
            }
        });

        Ok(items)
    }

    fn inventory(&self) -> syn::Result<syn::Item> {
        Ok(syn::parse_quote_spanned! { self.span=>
            buck2_build_api::__derive_refs::inventory::submit! {
                buck2_build_api::interpreter::rule_defs::provider::registration::ProviderRegistration {
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

    let type_bound_error = "type param should be V: ValueLifetimeless";
    let type_param = type_params.pop().unwrap();
    let Some(bound) = type_param.bounds.iter().into_singleton() else {
        return Err(syn::Error::new_spanned(type_param, type_bound_error));
    };
    match bound {
        TypeParamBound::Trait(b) => {
            if b.to_token_stream().to_string() != "ValueLifetimeless" {
                return Err(syn::Error::new_spanned(b, type_bound_error));
            }
        }
        _ => {
            return Err(syn::Error::new_spanned(bound, type_bound_error));
        }
    }

    let input = &codegen.input;
    let input: syn::Item = syn::parse_quote_spanned! { codegen.span=>
        #input
    };
    let generated: Vec<syn::Item> = [
        vec![codegen.builtin_provider_ty()?],
        vec![input],
        vec![codegen.impl_display()?],
        codegen.impl_starlark_value()?,
        vec![codegen.impl_serializable_value()?],
        vec![codegen.impl_provider_like()?],
        vec![codegen.impl_frozen_builtin_provider()?],
        codegen.callable()?,
        codegen.register()?,
        vec![codegen.inventory()?],
    ]
    .into_iter()
    .flatten()
    .collect();

    let generated = quote! {
        #(#generated)*
    };

    Ok(generated.into())
}
