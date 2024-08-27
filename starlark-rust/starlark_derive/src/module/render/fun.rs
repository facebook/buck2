/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::Attribute;
use syn::Expr;
use syn::ExprLit;
use syn::Lit;

use crate::module::render::render_starlark_return_type;
use crate::module::render::render_starlark_type;
use crate::module::typ::SpecialParam;
use crate::module::typ::StarArg;
use crate::module::typ::StarArgPassStyle;
use crate::module::typ::StarArgSource;
use crate::module::typ::StarFun;
use crate::module::typ::StarFunSource;
use crate::module::util::ident_string;

impl StarFun {
    fn ty_custom_expr(&self) -> syn::Expr {
        match &self.starlark_ty_custom_function {
            Some(x) => syn::parse_quote! {
                std::option::Option::Some(starlark::typing::Ty::custom_function(#x))
            },
            None => {
                syn::parse_quote! {
                    std::option::Option::None
                }
            }
        }
    }

    fn special_builtin_function_expr(&self) -> syn::Expr {
        match &self.special_builtin_function {
            Some(x) => syn::parse_quote! {
                std::option::Option::Some(#x)
            },
            None => {
                syn::parse_quote! {
                    std::option::Option::None
                }
            }
        }
    }

    fn as_type_expr(&self) -> syn::Expr {
        match &self.as_type {
            Some(x) => syn::parse_quote! {
                    std::option::Option::Some(
                        <#x as starlark::values::StarlarkValue>::get_type_starlark_repr(),
                    )
            },
            None => syn::parse_quote! {
                std::option::Option::None
            },
        }
    }

    /// Evaluator function parameter and call argument.
    fn eval_param_arg(
        &self,
    ) -> (
        Option<TokenStream>,
        Option<TokenStream>,
        Option<TokenStream>,
    ) {
        if let Some(SpecialParam { ident, ty }) = &self.eval {
            (
                Some(quote! {
                    #ident: #ty,
                }),
                Some(quote! {
                    #ty,
                }),
                Some(quote! {
                    eval,
                }),
            )
        } else {
            (None, None, None)
        }
    }

    /// Heap function parameter and call argument.
    fn heap_param_arg(
        &self,
    ) -> (
        Option<TokenStream>,
        Option<TokenStream>,
        Option<TokenStream>,
    ) {
        if let Some(SpecialParam { ident, ty }) = &self.heap {
            (
                Some(quote! {
                    #ident: #ty,
                }),
                Some(quote! {
                    #ty,
                }),
                Some(quote! {
                    eval.heap(),
                }),
            )
        } else {
            (None, None, None)
        }
    }

    /// `this` param if needed and call argument.
    fn this_param_arg(
        &self,
    ) -> (
        Option<TokenStream>,
        Option<TokenStream>,
        Option<TokenStream>,
    ) {
        if self.is_method() {
            (
                Some(quote! { __this: starlark::values::Value<'v>, }),
                Some(quote! { starlark::values::Value<'v>, }),
                Some(quote! { __this, }),
            )
        } else {
            (None, None, None)
        }
    }

    /// Non-special params.
    fn binding_params_arg(&self) -> (Vec<syn::FnArg>, Vec<syn::Type>, TokenStream, Vec<syn::Expr>) {
        let Bindings { prepare, bindings } = render_binding(self);
        let binding_params: Vec<_> = bindings.iter().map(|b| b.render_param()).collect();
        let binding_param_types: Vec<_> = bindings.iter().map(|b| b.render_param_type()).collect();
        let binding_args: Vec<_> = bindings.iter().map(|b| b.render_arg()).collect();
        (binding_params, binding_param_types, prepare, binding_args)
    }

    fn trait_name(&self) -> syn::Path {
        if self.is_method() {
            syn::parse_quote! { starlark::values::function::NativeMeth }
        } else {
            syn::parse_quote! { starlark::values::function::NativeFunc }
        }
    }

    fn name_str(&self) -> String {
        ident_string(&self.name)
    }

    pub(crate) fn struct_name(&self) -> Ident {
        format_ident!("Impl_{}", self.name_str())
    }

    /// Fields and field initializers for the struct implementing the trait.
    fn struct_fields(&self) -> syn::Result<(TokenStream, TokenStream)> {
        let signature = if let StarFunSource::Signature { .. } = self.source {
            Some(render_signature(self, Purpose::Parsing)?)
        } else {
            None
        };
        if let Some(signature) = signature {
            Ok((
                quote! {
                    signature: starlark::eval::ParametersSpec<starlark::values::FrozenValue>,
                },
                quote! {
                    signature: #signature,
                },
            ))
        } else {
            Ok((TokenStream::new(), TokenStream::new()))
        }
    }

    /// Globals builder call to register the function.
    fn builder_set(
        &self,
        documentation_var: &Ident,
        struct_fields_init: TokenStream,
    ) -> syn::Result<syn::Stmt> {
        let name_str = self.name_str();
        let speculative_exec_safe = self.speculative_exec_safe;
        let struct_name = self.struct_name();
        let special_builtin_function = self.special_builtin_function_expr();

        if self.is_method() {
            if self.as_type.is_some() {
                return Err(syn::Error::new(
                    self.span(),
                    "methods cannot have an `as_type` attribute",
                ));
            }
            if self.starlark_ty_custom_function.is_some() {
                return Err(syn::Error::new(
                    self.span(),
                    "methods cannot have a `ty_custom_function` attribute",
                ));
            }
            Ok(syn::parse_quote! {
                #[allow(clippy::redundant_closure)]
                globals_builder.set_method(
                    #name_str,
                    #speculative_exec_safe,
                    #documentation_var,
                    #struct_name {
                        #struct_fields_init
                    },
                );
            })
        } else {
            let as_type = self.as_type_expr();
            let ty_custom = self.ty_custom_expr();
            Ok(syn::parse_quote! {
                #[allow(clippy::redundant_closure)]
                globals_builder.set_function(
                    #name_str,
                    #speculative_exec_safe,
                    #documentation_var,
                    #as_type,
                    #ty_custom,
                    #special_builtin_function,
                    #struct_name {
                        #struct_fields_init
                    },
                );
            })
        }
    }
}

pub(crate) fn render_fun(x: StarFun) -> syn::Result<syn::Stmt> {
    let (documentation_var, documentation) = render_documentation(&x)?;

    let (this_param, this_param_type, this_arg) = x.this_param_arg();
    let (eval_param, eval_param_type, eval_arg) = x.eval_param_arg();
    let (heap_param, heap_param_type, heap_arg) = x.heap_param_arg();
    let (binding_params, binding_param_types, prepare, binding_args) = x.binding_params_arg();

    let trait_name = x.trait_name();
    let (struct_fields, struct_fields_init) = x.struct_fields()?;

    let struct_name = x.struct_name();

    let builder_set = x.builder_set(&documentation_var, struct_fields_init)?;

    let StarFun {
        attrs,
        return_type,
        body,
        ..
    } = x;

    Ok(syn::parse_quote! {
        {
            struct #struct_name {
                #struct_fields
            }

            impl #struct_name {
                // TODO(nga): copy lifetime parameter from declaration,
                //   so the warning would be precise.
                #[allow(clippy::extra_unused_lifetimes)]
                #( #attrs )*
                fn invoke_impl<'v>(
                    #this_param
                    #( #binding_params, )*
                    #eval_param
                    #heap_param
                ) -> #return_type {
                    #body
                }

                // When function signature declares return type as `anyhow::Result<impl AllocValue>`,
                // we cannot call `T::starlark_type_repr` to render documentation, because there's no T.
                // Future Rust will provide syntax `type ReturnType = impl AllocValue`:
                // https://github.com/rust-lang/rfcs/pull/2515
                // Until then we use this hack as a workaround.
                #[allow(dead_code)] // Function is not used when return type is specified explicitly.
                fn return_type_starlark_type_repr() -> starlark::typing::Ty {
                    fn get_impl<'v, T: starlark::values::AllocValue<'v>, E>(
                        _f: fn(
                            #this_param_type
                            #( #binding_param_types, )*
                            #eval_param_type
                            #heap_param_type
                        ) -> std::result::Result<T, E>,
                    ) -> starlark::typing::Ty {
                        <T as starlark::values::type_repr::StarlarkTypeRepr>::starlark_type_repr()
                    }
                    get_impl(Self::invoke_impl)
                }
            }

            impl #trait_name for #struct_name {
                #[allow(non_snake_case)] // Starlark doesn't have this convention
                fn invoke<'v>(
                    &self,
                    eval: &mut starlark::eval::Evaluator<'v, '_, '_>,
                    #this_param
                    parameters: &starlark::eval::Arguments<'v, '_>,
                ) -> starlark::Result<starlark::values::Value<'v>> {
                    #prepare
                    match Self::invoke_impl(#this_arg #( #binding_args, )* #eval_arg #heap_arg) {
                        Ok(v) => Ok(eval.heap().alloc(v)),
                        // The `.into()` is an `anyhow -> anyhow` conversion if the return type is `anyhow`
                        #[allow(clippy::useless_conversion)]
                        Err(e) => Err(e.into()),
                    }
                }
            }

            #documentation
            #builder_set
        }
    })
}

struct Bindings {
    prepare: TokenStream,
    bindings: Vec<BindingArg>,
}

// Given __args and __signature (if render_signature was Some)
// create bindings for all the arguments
fn render_binding(x: &StarFun) -> Bindings {
    match x.source {
        StarFunSource::Arguments => {
            let StarArg {
                attrs, name, ty, ..
            } = &x.args[0];
            Bindings {
                prepare: TokenStream::new(),
                bindings: vec![BindingArg {
                    name: name.to_owned(),
                    ty: ty.to_owned(),
                    attrs: attrs.clone(),
                    mutability: None,
                    expr: syn::parse_quote! { parameters },
                }],
            }
        }
        StarFunSource::ThisArguments => {
            let StarArg {
                attrs, name, ty, ..
            } = &x.args[1];
            let this = render_binding_arg(&x.args[0]);
            Bindings {
                prepare: TokenStream::new(),
                bindings: vec![
                    this,
                    BindingArg {
                        name: name.to_owned(),
                        ty: ty.to_owned(),
                        attrs: attrs.clone(),
                        mutability: None,
                        expr: syn::parse_quote! { parameters },
                    },
                ],
            }
        }
        StarFunSource::Signature { count } => {
            let bind_args: Vec<BindingArg> = x.args.iter().map(render_binding_arg).collect();
            Bindings {
                prepare: quote! {
                    let __args: [_; #count] = self.signature.collect_into(parameters, eval.heap())?;
                },
                bindings: bind_args,
            }
        }
        StarFunSource::Positional { required, optional } => {
            let bind_args = x.args.iter().map(render_binding_arg).collect();
            if optional == 0 {
                Bindings {
                    prepare: quote! {
                        parameters.no_named_args()?;
                        let __required: [_; #required] = parameters.positional(eval.heap())?;
                    },
                    bindings: bind_args,
                }
            } else {
                Bindings {
                    prepare: quote! {
                        parameters.no_named_args()?;
                        let (__required, __optional): ([_; #required], [_; #optional]) = parameters.optional(eval.heap())?;
                    },
                    bindings: bind_args,
                }
            }
        }
    }
}

struct BindingArg {
    expr: syn::Expr,

    attrs: Vec<Attribute>,
    mutability: Option<syn::Token![mut]>,
    name: Ident,
    ty: syn::Type,
}

impl BindingArg {
    fn render_param_type(&self) -> syn::Type {
        let BindingArg { ty, .. } = self;
        ty.clone()
    }

    fn render_param(&self) -> syn::FnArg {
        let mutability = &self.mutability;
        let name = &self.name;
        let ty = self.render_param_type();
        let attrs = &self.attrs;
        syn::parse_quote! {
            #( #attrs )*
            #mutability #name: #ty
        }
    }

    fn render_arg(&self) -> syn::Expr {
        self.expr.clone()
    }
}

// Create a binding for an argument given. If it requires an index, take from the index
fn render_binding_arg(arg: &StarArg) -> BindingArg {
    let name = &arg.name;
    let name_str = ident_string(name);

    let source = match arg.source {
        StarArgSource::This => quote! { __this },
        StarArgSource::Argument(i) => quote! { __args[#i].get() },
        StarArgSource::Required(i) => quote! {  Some(__required[#i])},
        StarArgSource::Optional(i) => quote! { __optional[#i] },
        ref s => unreachable!("unknown source: {:?}", s),
    };

    // Rust doesn't have powerful enough nested if yet
    let next = if arg.pass_style == StarArgPassStyle::This {
        syn::parse_quote! { starlark::eval::Arguments::check_this(#source)? }
    } else if arg.is_option() {
        assert!(
            arg.default.is_none(),
            "Can't have Option argument with a default, for `{}`",
            name_str
        );
        syn::parse_quote! { starlark::eval::Arguments::check_optional(#name_str, #source)? }
    } else if !arg.is_value() && arg.default.is_some() {
        let default = arg
            .default
            .as_ref()
            .unwrap_or_else(|| unreachable!("Checked on the line above"));
        syn::parse_quote! {
            {
                // Combo
                #[allow(clippy::manual_unwrap_or)]
                #[allow(clippy::unnecessary_lazy_evaluations)]
                #[allow(clippy::redundant_closure)]
                let x = starlark::eval::Arguments::check_optional(#name_str, #source)?.unwrap_or_else(|| #default);
                x
            }
        }
    } else {
        syn::parse_quote! { starlark::eval::Arguments::check_required(#name_str, #source)? }
    };

    BindingArg {
        expr: next,
        attrs: arg.attrs.clone(),
        mutability: arg.mutable,
        name: arg.name.to_owned(),
        ty: arg.ty.clone(),
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Purpose {
    Documentation,
    Parsing,
}

// Given the arguments, create a variable `signature` with a `ParametersSpec` object.
// Or return None if you don't need a signature
fn render_signature(x: &StarFun, purpose: Purpose) -> syn::Result<TokenStream> {
    let name_str = ident_string(&x.name);
    let signature_var = format_ident!("__signature");
    let sig_args = render_signature_args(&x.args, &signature_var, purpose)?;
    Ok(quote! {
        {
            #[allow(unused_mut)]
            let mut #signature_var = starlark::eval::ParametersSpec::new(#name_str.to_owned());
            #sig_args
            #signature_var.finish()
        }
    })
}

fn render_documentation(x: &StarFun) -> syn::Result<(Ident, TokenStream)> {
    // A signature is not needed to invoke positional-only functions, but we still want
    // information like names, order, type, etc to be available to call '.documentation()' on.
    let name_str = ident_string(&x.name);
    let need_render_signature = match &x.source {
        StarFunSource::Signature { .. } | StarFunSource::Positional { .. } => true,
        StarFunSource::Arguments | StarFunSource::ThisArguments => false,
    };
    let documentation_signature = if need_render_signature {
        render_signature(x, Purpose::Documentation)?
    } else {
        // An Arguments can take anything, so give the most generic documentation signature
        quote! {
            {
                let mut __signature = starlark::eval::ParametersSpec::<starlark::values::FrozenValue>::new(#name_str.to_owned());
                __signature.args();
                __signature.kwargs();
                __signature.finish()
            }
        }
    };

    let docs = match x.docstring.as_ref() {
        Some(d) => quote!(Some(#d)),
        None => quote!(None),
    };
    let parameter_types: Vec<syn::Expr> = x
        .args
        .iter()
        .flat_map(|arg| {
            match arg.pass_style {
                StarArgPassStyle::This => {
                    // "this" gets ignored when creating the signature, so make sure the indexes match up.
                    vec![]
                }
                StarArgPassStyle::Args => {
                    let typ_str = render_starlark_type(&arg.ty);
                    vec![syn::parse_quote! {
                    starlark::typing::macro_support::unpack_args_item_ty(#typ_str) }]
                }
                StarArgPassStyle::Kwargs => {
                    let typ_str = render_starlark_type(&arg.ty);
                    vec![syn::parse_quote! {
                    starlark::typing::macro_support::unpack_kwargs_value_ty(#typ_str) }]
                }
                StarArgPassStyle::PosOnly
                | StarArgPassStyle::PosOrNamed
                | StarArgPassStyle::NamedOnly => {
                    let typ_str = render_starlark_type(arg.without_option());
                    vec![syn::parse_quote! { #typ_str }]
                }
                StarArgPassStyle::Arguments => {
                    // `*args` and `**kwargs`.
                    vec![
                        syn::parse_quote! { starlark::typing::Ty::any() },
                        syn::parse_quote! { starlark::typing::Ty::any() },
                    ]
                }
            }
        })
        .collect();

    let return_type_str = render_starlark_return_type(x);
    let var_name = format_ident!("__documentation");
    let as_type = x.as_type_expr();
    let documentation = quote!(
        let #var_name = {
            let parameter_types = std::vec![#(#parameter_types),*];
            starlark::values::function::NativeCallableRawDocs {
                rust_docstring: #docs,
                signature: #documentation_signature,
                parameter_types,
                return_type: #return_type_str,
                as_type: #as_type,
            }
        };
    );
    Ok((var_name, documentation))
}

fn render_signature_args(
    args: &[StarArg],
    signature_var: &Ident,
    purpose: Purpose,
) -> syn::Result<TokenStream> {
    #[derive(PartialEq, Eq, PartialOrd, Ord)]
    enum CurrentParamStyle {
        PosOnly,
        PosOrNamed,
        NamedOnly,
        NoMore,
    }

    let mut sig_args = TokenStream::new();
    let mut last_param_style = CurrentParamStyle::PosOnly;
    for arg in args {
        match arg.pass_style {
            StarArgPassStyle::This => {}
            StarArgPassStyle::Args => {
                if last_param_style >= CurrentParamStyle::NamedOnly {
                    return Err(syn::Error::new(
                        arg.span,
                        "`args` cannot follow named-only parameters",
                    ));
                }
                last_param_style = CurrentParamStyle::NamedOnly;
            }
            StarArgPassStyle::Kwargs => {
                if last_param_style == CurrentParamStyle::NoMore {
                    return Err(syn::Error::new(
                        arg.span,
                        "Cannot have more than one `kwargs` parameter",
                    ));
                }
                last_param_style = CurrentParamStyle::NoMore;
            }
            StarArgPassStyle::PosOnly => {
                if last_param_style > CurrentParamStyle::PosOnly {
                    return Err(syn::Error::new(
                        arg.span,
                        "Positional-only parameter after non-positional-only",
                    ));
                }
                last_param_style = CurrentParamStyle::PosOnly;
            }
            StarArgPassStyle::PosOrNamed => {
                if last_param_style == CurrentParamStyle::PosOnly {
                    sig_args.extend(quote! {
                        #signature_var.no_more_positional_only_args();
                    });
                }
                last_param_style = CurrentParamStyle::PosOrNamed;
            }
            StarArgPassStyle::NamedOnly => {
                if last_param_style < CurrentParamStyle::NamedOnly {
                    sig_args.extend(quote! {
                        #signature_var.no_more_positional_args();
                    });
                }
                last_param_style = CurrentParamStyle::NamedOnly;
            }
            StarArgPassStyle::Arguments => {
                return Err(syn::Error::new(
                    arg.span,
                    "unreachable: signature is not meant to be created for `&Arguments`",
                ));
            }
        }
        sig_args.extend(render_signature_arg(arg, signature_var, purpose)?);
    }
    Ok(sig_args)
}

// Generate a statement that modifies signature to add a new argument in.
fn render_signature_arg(
    arg: &StarArg,
    signature_var: &Ident,
    purpose: Purpose,
) -> syn::Result<TokenStream> {
    let name_str = ident_string(&arg.name);

    if arg.pass_style == StarArgPassStyle::Args {
        assert!(arg.default.is_none(), "Can't have *args with a default");
        Ok(quote! { #signature_var.args();})
    } else if arg.pass_style == StarArgPassStyle::Kwargs {
        assert!(arg.default.is_none(), "Can't have **kwargs with a default");
        Ok(quote! { #signature_var.kwargs();})
    } else if arg.pass_style == StarArgPassStyle::This {
        Ok(TokenStream::new())
    } else if arg.is_option() {
        Ok(quote! { #signature_var.optional(#name_str);})
    } else if let Some(default) = &arg.default {
        // For things that are type Value, we put them on the frozen heap.
        // For things that aren't type value, use optional and then next_opt/unwrap
        // to avoid the to/from value conversion.
        if arg.is_value() {
            Ok(quote! {
                #signature_var.defaulted(#name_str, globals_builder.alloc(#default));
            })
        } else if purpose == Purpose::Documentation
            && render_default_as_frozen_value(default).is_some()
        {
            // We want the repr of the default arugment to show up, so pass it along
            let frozen = render_default_as_frozen_value(default).unwrap();
            Ok(quote! {
                #signature_var.defaulted(#name_str, #frozen);
            })
        } else {
            Ok(quote! {
                #signature_var.optional(#name_str);
            })
        }
    } else {
        Ok(quote! {
            #signature_var.required(#name_str);
        })
    }
}

/// We have an argument that the user wants to use as a default.
/// That _might_ have a valid `FrozenValue` representation, if so, it would be great to use for documentation.
/// Try and synthesise it if we can.
fn render_default_as_frozen_value(default: &Expr) -> Option<TokenStream> {
    let x = quote!(#default).to_string();
    if let Ok(x) = x.trim_end_matches("i32").parse::<i32>() {
        Some(quote! { globals_builder.alloc(#x) })
    } else if let Ok(x) = x.parse::<bool>() {
        Some(quote! { starlark::values::FrozenValue::new_bool(#x) })
    } else if x == "NoneOr :: None" {
        Some(quote! { starlark::values::FrozenValue::new_none() })
    } else if matches!(
        default,
        Expr::Lit(ExprLit {
            lit: Lit::Str(_),
            ..
        })
    ) {
        // Make sure we don't splice in `x` again, or we double quote the string
        Some(quote! { globals_builder.alloc(#default) })
    } else if x == "UnpackListOrTuple :: default()" || x == "UnpackList :: default()" {
        Some(quote! { globals_builder.alloc(starlark::values::list::AllocList::EMPTY) })
    } else if x == "SmallMap :: new()" {
        Some(quote! { globals_builder.alloc(starlark::values::dict::AllocDict::EMPTY) })
    } else {
        None
    }
}
