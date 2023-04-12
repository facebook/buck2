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
use quote::quote_spanned;
use syn::spanned::Spanned;
use syn::Attribute;
use syn::Type;

use crate::module::render::render_starlark_return_type;
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
                Some(quote_spanned! {self.span()=>
                    #ident: #ty,
                }),
                Some(quote_spanned! {self.span()=>
                    #ty,
                }),
                Some(quote_spanned! {self.span()=>
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
                Some(quote_spanned! {self.span()=>
                    #ident: #ty,
                }),
                Some(quote_spanned! {self.span()=>
                    #ty,
                }),
                Some(quote_spanned! {self.span()=>
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
                Some(quote_spanned! {self.span()=> __this: starlark::values::Value<'v>, }),
                Some(quote_spanned! {self.span()=> starlark::values::Value<'v>, }),
                Some(quote_spanned! {self.span()=> __this, }),
            )
        } else {
            (None, None, None)
        }
    }

    /// Non-special params.
    fn binding_params_arg(
        &self,
    ) -> (
        Vec<TokenStream>,
        Vec<TokenStream>,
        TokenStream,
        Vec<TokenStream>,
    ) {
        let Bindings { prepare, bindings } = render_binding(self);
        let binding_params: Vec<_> = bindings.iter().map(|b| b.render_param()).collect();
        let binding_param_types: Vec<_> = bindings.iter().map(|b| b.render_param_type()).collect();
        let binding_args: Vec<_> = bindings.iter().map(|b| b.render_arg()).collect();
        (binding_params, binding_param_types, prepare, binding_args)
    }

    fn trait_name(&self) -> TokenStream {
        if self.is_method() {
            quote_spanned! {self.span()=> starlark::values::function::NativeMeth }
        } else {
            quote_spanned! {self.span()=> starlark::values::function::NativeFunc }
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
            Some(render_signature(self)?)
        } else {
            None
        };
        if let Some(signature) = signature {
            Ok((
                quote_spanned! { self.span()=>
                    signature: starlark::eval::ParametersSpec<starlark::values::FrozenValue>,
                },
                quote_spanned! { self.span()=>
                    signature: #signature,
                },
            ))
        } else {
            Ok((
                quote_spanned! { self.span()=> },
                quote_spanned! { self.span()=> },
            ))
        }
    }

    /// Globals builder call to register the function.
    fn builder_set(
        &self,
        documentation_var: &Ident,
        struct_fields_init: TokenStream,
    ) -> TokenStream {
        let name_str = self.name_str();
        let speculative_exec_safe = self.speculative_exec_safe;
        let typ = self.type_expr();
        let struct_name = self.struct_name();

        if self.is_method() {
            quote_spanned! {self.span()=>
                #[allow(clippy::redundant_closure)]
                globals_builder.set_method(
                    #name_str,
                    #speculative_exec_safe,
                    #documentation_var,
                    #typ,
                    #struct_name {
                        #struct_fields_init
                    },
                );
            }
        } else {
            quote_spanned! {self.span()=>
                #[allow(clippy::redundant_closure)]
                globals_builder.set_function(
                    #name_str,
                    #speculative_exec_safe,
                    #documentation_var,
                    #typ,
                    #struct_name {
                        #struct_fields_init
                    },
                );
            }
        }
    }
}

pub(crate) fn render_fun(x: StarFun) -> syn::Result<TokenStream> {
    let span = x.span();

    let (documentation_var, documentation) = render_documentation(&x)?;

    let (this_param, this_param_type, this_arg) = x.this_param_arg();
    let (eval_param, eval_param_type, eval_arg) = x.eval_param_arg();
    let (heap_param, heap_param_type, heap_arg) = x.heap_param_arg();
    let (binding_params, binding_param_types, prepare, binding_args) = x.binding_params_arg();

    let trait_name = x.trait_name();
    let (struct_fields, struct_fields_init) = x.struct_fields()?;

    let struct_name = x.struct_name();

    let builder_set = x.builder_set(&documentation_var, struct_fields_init);

    let StarFun {
        attrs,
        return_type,
        body,
        ..
    } = x;

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

            // When function signature declares return type as `anyhow::Result<impl AllocValue>`,
            // we cannot call `T::starlark_type_repr` to render documentation, because there's no T.
            // Future Rust will provide syntax `type ReturnType = impl AllocValue`:
            // https://github.com/rust-lang/rfcs/pull/2515
            // Until then we use this hack as a workaround.
            #[allow(dead_code)] // Function is not used when return type is specified explicitly.
            fn return_type_starlark_type_repr() -> std::string::String {
                fn get_impl<'v, T: starlark::values::AllocValue<'v>>(
                    _f: fn(
                        #this_param_type
                        #( #binding_param_types, )*
                        #eval_param_type
                        #heap_param_type
                    ) -> anyhow::Result<T>,
                ) -> std::string::String {
                    <T as starlark::values::type_repr::StarlarkTypeRepr>::starlark_type_repr()
                }
                get_impl(Self::invoke_impl)
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
        StarFunSource::Arguments => {
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
        StarFunSource::ThisArguments => {
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
        StarFunSource::Signature { count } => {
            let bind_args: Vec<BindingArg> = x.args.iter().map(render_binding_arg).collect();
            Bindings {
                prepare: quote_spanned! { span=>
                    let __args: [_; #count] = self.signature.collect_into(parameters, eval.heap())?;
                },
                bindings: bind_args,
            }
        }
        StarFunSource::Positional { required, optional } => {
            let bind_args = x.args.iter().map(render_binding_arg).collect();
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
    fn render_param_type(&self) -> TokenStream {
        let BindingArg { ty, .. } = self;
        quote_spanned! { ty.span()=>
            #ty
        }
    }

    fn render_param(&self) -> TokenStream {
        let mutability = &self.mutability;
        let name = &self.name;
        let ty = self.render_param_type();
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
fn render_signature(x: &StarFun) -> syn::Result<TokenStream> {
    let span = x.args_span();
    let name_str = ident_string(&x.name);
    let signature_var = format_ident!("__signature");
    let sig_args = render_signature_args(&x.args, &signature_var)?;
    Ok(quote_spanned! {
        span=> {
            #[allow(unused_mut)]
            let mut #signature_var = starlark::eval::ParametersSpec::new(#name_str.to_owned());
            #sig_args
            #signature_var.finish()
        }
    })
}

fn render_documentation(x: &StarFun) -> syn::Result<(Ident, TokenStream)> {
    let span = x.args_span();

    // A signature is not needed to invoke positional-only functions, but we still want
    // information like names, order, type, etc to be available to call '.documentation()' on.
    let name_str = ident_string(&x.name);
    let need_render_signature = match &x.source {
        StarFunSource::Signature { .. } | StarFunSource::Positional { .. } => true,
        StarFunSource::Arguments | StarFunSource::ThisArguments => false,
    };
    let documentation_signature = if need_render_signature {
        render_signature(x)?
    } else {
        // An Arguments can take anything, so give the most generic documentation signature
        quote_spanned! {
            span=> {
                let mut __signature = starlark::eval::ParametersSpec::<starlark::values::FrozenValue>::new(#name_str.to_owned());
                __signature.args();
                __signature.kwargs();
                __signature.finish()
            }
        }
    };

    let docs = match x.docstring.as_ref() {
        Some(d) => quote_spanned!(span=> Some(#d)),
        None => quote_spanned!(span=> None),
    };
    let parameter_types: Vec<_> = x
        .args
        .iter()
        .filter(|a| {
            // "this" gets ignored when creating the signature, so make sure the indexes match up.
            // Arguments doesn't correspond to a parameter type, since it is many parameters
            a.pass_style != StarArgPassStyle::This && a.pass_style != StarArgPassStyle::Arguments
        })
        .enumerate()
        .filter(|(_, a)| a.pass_style != StarArgPassStyle::Args) // these aren't coerced according to their type (Vec vs tuple)
        .map(|(i, arg)| {
            let typ_str = render_starlark_type(span, &arg.ty, &arg.starlark_type);
            quote_spanned!(span=> (#i, starlark::docs::DocType { raw_type: #typ_str }) )
        })
        .collect();

    let return_type_str = render_starlark_return_type(x, &x.starlark_return_type);
    let var_name = format_ident!("__documentation");
    let documentation = quote_spanned!(span=>
        let #var_name = {
            let signature = #documentation_signature;
            let parameter_types = std::collections::HashMap::from([#(#parameter_types),*]);
            let return_type = Some(
                starlark::docs::DocType {
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
    );
    Ok((var_name, documentation))
}

fn render_signature_args(args: &[StarArg], signature_var: &Ident) -> syn::Result<TokenStream> {
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
                        #signature_var.no_more_positional_only_args();
                    });
                }
                last_param_style = CurrentParamStyle::PosOrNamed;
            }
            StarArgPassStyle::NamedOnly => {
                if last_param_style < CurrentParamStyle::NamedOnly {
                    sig_args.extend(quote_spanned! { arg.span=>
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
        sig_args.extend(render_signature_arg(arg, signature_var)?);
    }
    Ok(sig_args)
}

// Generate a statement that modifies signature to add a new argument in.
fn render_signature_arg(arg: &StarArg, signature_var: &Ident) -> syn::Result<TokenStream> {
    let span = arg.span;

    let name_str = ident_string(&arg.name);

    if arg.pass_style == StarArgPassStyle::Args {
        assert!(arg.default.is_none(), "Can't have *args with a default");
        Ok(quote_spanned! { span=> #signature_var.args();})
    } else if arg.pass_style == StarArgPassStyle::Kwargs {
        assert!(arg.default.is_none(), "Can't have **kwargs with a default");
        Ok(quote_spanned! { span=> #signature_var.kwargs();})
    } else if arg.pass_style == StarArgPassStyle::This {
        Ok(quote_spanned! { span=> })
    } else if arg.is_option() {
        Ok(quote_spanned! { span=> #signature_var.optional(#name_str);})
    } else if let Some(default) = &arg.default {
        // For things that are type Value, we put them on the frozen heap.
        // For things that aren't type value, use optional and then next_opt/unwrap
        // to avoid the to/from value conversion.
        if arg.is_value() {
            Ok(quote_spanned! { span=>
                #signature_var.defaulted(#name_str, globals_builder.alloc(#default));
            })
        } else {
            Ok(quote_spanned! { span=>
                #signature_var.optional(#name_str);
            })
        }
    } else {
        Ok(quote_spanned! { span=>
            #signature_var.required(#name_str);
        })
    }
}
