/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use buck2_core::cells::cell_path::CellPath;
use buck2_core::provider::id::ProviderId;
use buck2_interpreter_for_build::provider::callable::ProviderCallableLike;
use gazebo::any::ProvidesStaticType;
use gazebo::dupe::Dupe;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Arguments;
use starlark::eval::Evaluator;
use starlark::eval::ParametersSpec;
use starlark::values::docs::DocItem;
use starlark::values::docs::DocString;
use starlark::values::AllocValue;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenRef;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::provider::registration::ProviderRegistration;
use crate::interpreter::rule_defs::provider::user::user_provider_creator;

#[derive(Debug, thiserror::Error)]
enum ProviderCallableError {
    #[error(
        "The result of `provider()` must be assigned to a top-level variable before it can be called"
    )]
    NotBound,
    #[error(
        "Provider type must be assigned to a variable, e.g. `ProviderInfo = provider(fields = {0:?})`"
    )]
    ProviderNotAssigned(Vec<String>),
}

fn create_callable_function_signature(
    function_name: &str,
    fields: &[String],
) -> ParametersSpec<FrozenValue> {
    let mut signature = ParametersSpec::with_capacity(function_name.to_owned(), fields.len());
    // TODO(nmj): Should double check we don't actually need positional args in-repo
    signature.no_more_positional_args();
    for field in fields {
        signature.defaulted(field, FrozenValue::new_none());
    }

    signature.finish()
}

#[derive(Debug)]
struct UserProviderCallableData {
    provider_id: Arc<ProviderId>,
    fields: Vec<String>,
}

#[derive(Debug, Trace)]
enum UserProviderCallableImpl {
    Unbound,
    Bound(
        ParametersSpec<FrozenValue>,
        FrozenRef<'static, UserProviderCallableData>,
    ),
}

impl UserProviderCallableImpl {
    fn invoke<'v>(
        &self,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        match self {
            UserProviderCallableImpl::Unbound => Err(ProviderCallableError::NotBound.into()),
            UserProviderCallableImpl::Bound(signature, data) => {
                signature.parser(args, eval, |parser, eval| {
                    user_provider_creator(data.provider_id.dupe(), &data.fields, eval, parser)
                })
            }
        }
    }
}

/// The result of calling `provider()`. This is a callable that accepts the fields
/// provided in the `provider()` call, and generates a Starlark `UserProvider` object.
///
/// This object must be assigned to a variable at the top level of the module before it may be invoked
///
/// Field values default to `None`
#[derive(Debug, ProvidesStaticType, Trace, NoSerialize)]
pub struct UsedProviderCallable {
    /// The name of this provider, filled in by `export_as()`. This must be set before this
    /// object can be called and Providers created.
    #[trace(unsafe_ignore)]
    id: RefCell<Option<Arc<ProviderId>>>,
    /// The path where this `ProviderCallable` is created and assigned
    #[trace(unsafe_ignore)]
    path: CellPath,
    /// The docstring for this provider
    docs: Option<DocString>,
    /// The docstrings for each field. The length of must be identical to `fields`
    field_docs: Vec<Option<DocString>>,
    /// The names of the fields used in `callable`
    fields: Vec<String>,
    /// The actual callable that creates instances of `UserProvider`
    callable: RefCell<UserProviderCallableImpl>,
}

impl Display for UsedProviderCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.id() {
            None => write!(f, "unnamed provider"),
            Some(id) => {
                write!(f, "{}(", id.name)?;
                for (i, x) in self.fields.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", x)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl UsedProviderCallable {
    pub fn new(
        path: CellPath,
        docs: Option<DocString>,
        field_docs: Vec<Option<DocString>>,
        fields: Vec<String>,
    ) -> Self {
        assert_eq!(
            field_docs.len(),
            fields.len(),
            "Expected {} fields, but got docs for {} fields",
            fields.len(),
            field_docs.len()
        );
        Self {
            id: RefCell::new(None),
            path,
            docs,
            field_docs,
            fields,
            callable: RefCell::new(UserProviderCallableImpl::Unbound),
        }
    }

    /// Get the documentation for all builtin providers that have been registered with `inventory`
    pub fn builtin_provider_documentation() -> HashMap<String, Option<DocItem>> {
        let mut provider_globals_builder = GlobalsBuilder::new();
        for registration in inventory::iter::<ProviderRegistration> {
            (registration.register_globals)(&mut provider_globals_builder);
        }
        let provider_globals = provider_globals_builder.build();
        provider_globals.member_documentation()
    }
}

impl ProviderCallableLike for UsedProviderCallable {
    fn id(&self) -> Option<&Arc<ProviderId>> {
        // Safe because once we set id, we never change it
        unsafe { self.id.try_borrow_unguarded().unwrap().as_ref() }
    }
}

impl<'v> AllocValue<'v> for UsedProviderCallable {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl Freeze for UsedProviderCallable {
    type Frozen = FrozenUserProviderCallable;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let callable = self.callable.into_inner();
        let id = match self.id.into_inner() {
            Some(x) => x,
            None => {
                // Unfortunately we have no name or location for the provider at this point,
                // so reproduce the fields so that the provider can be identified.
                return Err(ProviderCallableError::ProviderNotAssigned(self.fields).into());
            }
        };

        Ok(FrozenUserProviderCallable::new(
            id,
            self.docs,
            self.field_docs,
            self.fields,
            callable,
        ))
    }
}

impl<'v> StarlarkValue<'v> for UsedProviderCallable {
    starlark_type!("provider_callable");

    fn export_as(&self, variable_name: &str, eval: &mut Evaluator<'v, '_>) {
        // First export wins
        let mut id = self.id.borrow_mut();
        if id.is_none() {
            let new_id = Arc::new(ProviderId {
                path: Some(self.path.clone()),
                name: variable_name.to_owned(),
            });
            *id = Some(new_id.dupe());
            *self.callable.borrow_mut() = UserProviderCallableImpl::Bound(
                create_callable_function_signature(&new_id.name, &self.fields),
                eval.frozen_heap()
                    .alloc_any_display_from_debug(UserProviderCallableData {
                        provider_id: new_id,
                        fields: self.fields.clone(),
                    }),
            );
        }
    }

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(provider_callable_methods)
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.callable.borrow().invoke(args, eval)
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn ProviderCallableLike>(self);
    }

    fn documentation(&self) -> Option<DocItem> {
        let return_types = vec![None; self.fields.len()];
        self.provider_callable_documentation(
            &self.docs,
            &self.fields,
            &self.field_docs,
            &return_types,
        )
    }
}

#[derive(Debug, ProvidesStaticType, NoSerialize)]
pub struct FrozenUserProviderCallable {
    /// The name of this provider, filled in by `export_as()`. This must be set before this
    /// object can be called and Providers created.
    id: Arc<ProviderId>,
    /// The docstring for this provider
    docs: Option<DocString>,
    /// The docstrings for each field. The length of must be identical to `fields`
    field_docs: Vec<Option<DocString>>,
    /// The names of the fields used in `callable`
    fields: Vec<String>,
    /// The actual callable that creates instances of `UserProvider`
    callable: UserProviderCallableImpl,
}
starlark_simple_value!(FrozenUserProviderCallable);

impl Display for FrozenUserProviderCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.id.name)?;
        for (i, x) in self.fields.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", x)?;
        }
        write!(f, ")")
    }
}

impl FrozenUserProviderCallable {
    fn new(
        id: Arc<ProviderId>,
        docs: Option<DocString>,
        field_docs: Vec<Option<DocString>>,
        fields: Vec<String>,
        callable: UserProviderCallableImpl,
    ) -> Self {
        assert_eq!(
            field_docs.len(),
            fields.len(),
            "Expected {} fields, but got docs for {} fields",
            fields.len(),
            field_docs.len()
        );
        Self {
            id,
            docs,
            field_docs,
            fields,
            callable,
        }
    }
}

impl ProviderCallableLike for FrozenUserProviderCallable {
    fn id(&self) -> Option<&Arc<ProviderId>> {
        Some(&self.id)
    }
}

impl<'v> StarlarkValue<'v> for FrozenUserProviderCallable {
    starlark_type!("provider_callable");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(provider_callable_methods)
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.callable.invoke(args, eval)
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn ProviderCallableLike>(self);
    }

    fn documentation(&self) -> Option<DocItem> {
        let return_types = vec![None; self.fields.len()];
        self.provider_callable_documentation(
            &self.docs,
            &self.fields,
            &self.field_docs,
            &return_types,
        )
    }
}

#[starlark_module]
fn provider_callable_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn r#type<'v>(this: Value<'v>, heap: &Heap) -> anyhow::Result<Value<'v>> {
        if let Some(x) = this.downcast_ref::<UsedProviderCallable>() {
            match &*x.id.borrow() {
                None => Err(ProviderCallableError::ProviderNotAssigned(x.fields.clone()).into()),
                Some(id) => Ok(heap.alloc(id.name.as_str())),
            }
        } else if let Some(x) = this.downcast_ref::<FrozenUserProviderCallable>() {
            Ok(heap.alloc(x.id.name.as_str()))
        } else {
            unreachable!(
                "This parameter must be one of the types, but got `{}`",
                this.get_type()
            )
        }
    }
}
