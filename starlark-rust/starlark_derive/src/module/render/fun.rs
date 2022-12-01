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

use gazebo::prelude::*;
use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote_spanned;
use syn::Attribute;
use syn::Type;

use crate::module::render::render_starlark_type;
use crate::module::typ::SpecialParam;
use crate::module::typ::StarArg;
use crate::module::typ::StarArgPassStyle;
use crate::module::typ::StarArgSource;
use crate::module::typ::StarFun;
use crate::module::typ::StarFunSource;
use crate::module::util::ident_string;
use crate::module::util::mut_token;

impl StarFun {
    fn type_expr(&self) -> TokenStream {
        match &self.type_attribute {
            Some(x) => quote_spanned! {
                self.span()=>
                std::option::Option::Some(starlark::const_frozen_string!(#x).to_frozen_value())
            },
            None => {
                quote_spanned! {
                    self.span()=>
                    std::option::Option::None
                }
            }
        }
    }
}

pub(crate) fn render_fun(x: StarFun) -> syn::Result<TokenStream> {
    let span = x.span();

    let name_str = ident_string(&x.name);
    let signature = render_signature(&x)?;
    let documentation = render_documentation(&x)?;
    let binding = render_binding(&x);
    let is_method = x.is_method();

    let typ = x.type_expr();

    let StarFun {
        attrs,
        heap,
        eval,
        return_type,
        speculative_exec_safe,
        body,
        ..
    } = x;

    let struct_name = format_ident!("Impl_{}", name_str);
    let trait_name = if is_method {
        quote_spanned! {span=> starlark::values::function::NativeMeth }
    } else {
        quote_spanned! {span=> starlark::values::function::NativeFunc }
    };

    let (struct_fields, struct_fields_init) = if let Some(signature) = signature {
        (
            quote_spanned! { span=>
                signature: starlark::eval::ParametersSpec<starlark::values::FrozenValue>,
            },
            quote_spanned! { span=>
                signature: #signature,
            },
        )
    } else {
        (quote_spanned! { span=> }, quote_spanned! { span=> })
    };

    let (this_param, this_arg, builder_set) = if is_method {
        (
            quote_spanned! {span=> __this: starlark::values::Value<'v>, },
            quote_spanned! {span=> __this, },
            quote_spanned! {span=>
                #[allow(clippy::redundant_closure)]
                globals_builder.set_method(
                    #name_str,
                    #speculative_exec_safe,
                    __documentation_renderer,
                    #typ,
                    #struct_name {
                        #struct_fields_init
                    },
                );
            },
        )
    } else {
        (
            quote_spanned! {span=> },
            quote_spanned! {span=> },
            quote_spanned! {span=>
                #[allow(clippy::redundant_closure)]
                globals_builder.set_function(
                    #name_str,
                    #speculative_exec_safe,
                    __documentation_renderer,
                    #typ,
                    #struct_name {
                        #struct_fields_init
                    },
                );
            },
        )
    };

    let (eval_param, eval_arg) = if let Some(SpecialParam { ident, ty }) = eval {
        (
            Some(quote_spanned! {span=>
                #ident: #ty,
            }),
            Some(quote_spanned! {span=>
                eval,
            }),
        )
    } else {
        (None, None)
    };
    let (heap_param, heap_arg) = if let Some(SpecialParam { ident, ty }) = heap {
        (
            Some(quote_spanned! {span=>
                #ident: #ty,
            }),
            Some(quote_spanned! {span=>
                eval.heap(),
            }),
        )
    } else {
        (None, None)
    };

    let Bindings { prepare, bindings } = binding;
    let binding_params: Vec<_> = bindings.map(|b| b.render_param());
    let binding_args: Vec<_> = bindings.map(|b| b.render_arg());

    Ok(quote_spanned! {
        span=>
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
        }

        impl #trait_name for #struct_name {
            #[allow(non_snake_case)] // Starlark doesn't have this convention
            fn invoke<'v>(
                &self,
                eval: &mut starlark::eval::Evaluator<'v, '_>,
                #this_param
                parameters: &starlark::eval::Arguments<'v, '_>,
            ) -> anyhow::Result<starlark::values::Value<'v>> {
                #prepare
                match Self::invoke_impl(#this_arg #( #binding_args, )* #eval_arg #heap_arg) {
                    Ok(v) => Ok(eval.heap().alloc(v)),
                    Err(e) => Err(e),
                }
            }
        }

        {
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
    let span = x.args_span();
    match x.source {
        StarFunSource::Parameters => {
            let StarArg {
                span,
                attrs,
                name,
                ty,
                ..
            } = &x.args[0];
            let span = *span;
            Bindings {
                prepare: quote_spanned! { span=> },
                bindings: vec![BindingArg {
                    name: name.to_owned(),
                    ty: ty.to_owned(),
                    attrs: attrs.clone(),
                    mutability: quote_spanned! { span=> },
                    expr: quote_spanned! { span=> parameters },
                }],
            }
        }
        StarFunSource::ThisParameters => {
            let StarArg {
                span,
                attrs,
                name,
                ty,
                ..
            } = &x.args[1];
            let span = *span;
            let this = render_binding_arg(&x.args[0]);
            Bindings {
                prepare: quote_spanned! { span=> },
                bindings: vec![
                    this,
                    BindingArg {
                        name: name.to_owned(),
                        ty: ty.to_owned(),
                        attrs: attrs.clone(),
                        mutability: quote_spanned! { span=> },
                        expr: quote_spanned! { span=> parameters },
                    },
                ],
            }
        }
        StarFunSource::Argument(arg_count) => {
            let bind_args: Vec<BindingArg> = x.args.map(render_binding_arg);
            Bindings {
                prepare: quote_spanned! { span=>
                    let __args: [_; #arg_count] = self.signature.collect_into(parameters, eval.heap())?;
                },
                bindings: bind_args,
            }
        }
        StarFunSource::Positional(required, optional) => {
            let bind_args = x.args.map(render_binding_arg);
            if optional == 0 {
                Bindings {
                    prepare: quote_spanned! {
                        span=>
                        parameters.no_named_args()?;
                        let __required: [_; #required] = parameters.positional(eval.heap())?;
                    },
                    bindings: bind_args,
                }
            } else {
                Bindings {
                    prepare: quote_spanned! {
                        span=>
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
    expr: TokenStream,

    attrs: Vec<Attribute>,
    mutability: TokenStream,
    name: Ident,
    ty: Type,
}

impl BindingArg {
    fn render_param(&self) -> TokenStream {
        let mutability = &self.mutability;
        let name = &self.name;
        let ty = &self.ty;
        let attrs = &self.attrs;
        quote_spanned! {
            self.name.span()=>
            #( #attrs )*
            #mutability #name: #ty
        }
    }

    fn render_arg(&self) -> TokenStream {
        self.expr.clone()
    }
}

// Create a binding for an argument given. If it requires an index, take from the index
fn render_binding_arg(arg: &StarArg) -> BindingArg {
    let span = arg.span;
    let name = &arg.name;
    let name_str = ident_string(name);

    let source = match arg.source {
        StarArgSource::This => quote_spanned! {span=> __this},
        StarArgSource::Argument(i) => quote_spanned! {span=> __args[#i].get()},
        StarArgSource::Required(i) => quote_spanned! {span=> Some(__required[#i])},
        StarArgSource::Optional(i) => quote_spanned! {span=> __optional[#i]},
        ref s => unreachable!("unknown source: {:?}", s),
    };

    // Rust doesn't have powerful enough nested if yet
    let next = if arg.pass_style == StarArgPassStyle::This {
        quote_spanned! { span=> starlark::eval::Arguments::check_this(#source)? }
    } else if arg.is_option() {
        assert!(
            arg.default.is_none(),
            "Can't have Option argument with a default, for `{}`",
            name_str
        );
        quote_spanned! { span=> starlark::eval::Arguments::check_optional(#name_str, #source)? }
    } else if !arg.is_value() && arg.default.is_some() {
        let default = arg
            .default
            .as_ref()
            .unwrap_or_else(|| unreachable!("Checked on the line above"));
        quote_spanned! { span=>
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
        quote_spanned! { span=> starlark::eval::Arguments::check_required(#name_str, #source)? }
    };

    let mutability = mut_token(arg.mutable);

    BindingArg {
        expr: next,
        attrs: arg.attrs.clone(),
        mutability,
        name: arg.name.to_owned(),
        ty: arg.ty.clone(),
    }
}

// Given the arguments, create a variable `signature` with a `ParametersSpec` object.
// Or return None if you don't need a signature
fn render_signature(x: &StarFun) -> syn::Result<Option<TokenStream>> {
    let span = x.args_span();
    if let StarFunSource::Argument(args_count) = x.source {
        let name_str = ident_string(&x.name);
        let sig_args = render_signature_args(&x.args)?;
        Ok(Some(quote_spanned! {
            span=> {
                #[allow(unused_mut)]
                let mut __signature = starlark::eval::ParametersSpec::with_capacity(#name_str.to_owned(), #args_count);
                #sig_args
                __signature.finish()
            }
        }))
    } else {
        Ok(None)
    }
}

fn render_documentation(x: &StarFun) -> syn::Result<TokenStream> {
    let span = x.args_span();

    // A signature is not needed to invoke positional-only functions, but we still want
    // information like names, order, type, etc to be available to call '.documentation()' on.
    let args_count = match &x.source {
        StarFunSource::Argument(args_count) => Some(*args_count),
        StarFunSource::Positional(required, optional) => Some(required + optional),
        StarFunSource::Parameters | StarFunSource::ThisParameters => None,
    };
    let name_str = ident_string(&x.name);
    let documentation_signature = match args_count {
        Some(args_count) => {
            let sig_args = render_signature_args(&x.args)?;
            quote_spanned! {
                span=> {
                #[allow(unused_mut)]
                let mut __signature = starlark::eval::ParametersSpec::with_capacity(#name_str.to_owned(), #args_count);
                #sig_args
                __signature.finish()
                }
            }
        }
        None => {
            quote_spanned!(span=> starlark::eval::ParametersSpec::<starlark::values::FrozenValue>::new(#name_str.to_owned()).finish())
        }
    };

    let docs = match x.docstring.as_ref() {
        Some(d) => quote_spanned!(span=> Some(#d)),
        None => quote_spanned!(span=> None),
    };
    let return_type_arg = &x.return_type_arg;
    let parameter_types: Vec<_> = x
        .args
        .iter()
        .filter(|a| a.pass_style != StarArgPassStyle::This) // "this" gets ignored when creating the signature, so make sure the indexes match up.
        .enumerate()
        .map(|(i, arg)| {
            let typ = &arg.ty;
            let typ_str = render_starlark_type(span, typ);
            quote_spanned!(span=> (#i, starlark::values::docs::Type { raw_type: #typ_str }) )
        })
        .collect();

    let return_type_str = render_starlark_type(span, return_type_arg);
    Ok(quote_spanned!(span=>
        let __documentation_renderer = {
            let signature = #documentation_signature;
            let parameter_types = std::collections::HashMap::from([#(#parameter_types),*]);
            let return_type = Some(
                starlark::values::docs::Type {
                    raw_type: #return_type_str
                }
            );
            starlark::values::function::NativeCallableRawDocs {
                rust_docstring: #docs,
                signature,
                parameter_types,
                return_type,
            }
        };
    ))
}

fn render_signature_args(args: &[StarArg]) -> syn::Result<TokenStream> {
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
                    sig_args.extend(quote_spanned! { arg.span=>
                        __signature.no_more_positional_only_args();
                    });
                }
                last_param_style = CurrentParamStyle::PosOrNamed;
            }
            StarArgPassStyle::NamedOnly => {
                if last_param_style < CurrentParamStyle::NamedOnly {
                    sig_args.extend(quote_spanned! { arg.span=>
                        __signature.no_more_positional_args();
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
        sig_args.extend(render_signature_arg(arg)?);
    }
    Ok(sig_args)
}

// Generate a statement that modifies signature to add a new argument in.
fn render_signature_arg(arg: &StarArg) -> syn::Result<TokenStream> {
    let span = arg.span;

    let name_str = ident_string(&arg.name);

    if arg.pass_style == StarArgPassStyle::Args {
        assert!(arg.default.is_none(), "Can't have *args with a default");
        Ok(quote_spanned! { span=> __signature.args();})
    } else if arg.pass_style == StarArgPassStyle::Kwargs {
        assert!(arg.default.is_none(), "Can't have **kwargs with a default");
        Ok(quote_spanned! { span=> __signature.kwargs();})
    } else if arg.pass_style == StarArgPassStyle::This {
        Ok(quote_spanned! { span=> })
    } else if arg.is_option() {
        Ok(quote_spanned! { span=> __signature.optional(#name_str);})
    } else if let Some(default) = &arg.default {
        // For things that are type Value, we put them on the frozen heap.
        // For things that aren't type value, use optional and then next_opt/unwrap
        // to avoid the to/from value conversion.
        if arg.is_value() {
            Ok(
                quote_spanned! { span=> __signature.defaulted(#name_str, globals_builder.alloc(#default));},
            )
        } else {
            Ok(quote_spanned! { span=> __signature.optional(#name_str);})
        }
    } else {
        Ok(quote_spanned! { span=> __signature.required(#name_str);})
    }
}
