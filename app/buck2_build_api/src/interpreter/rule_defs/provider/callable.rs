/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::provider::id::ProviderId;
use buck2_interpreter::build_context::starlark_path_from_build_context;
use buck2_interpreter::types::provider::callable::ProviderCallableLike;
use dupe::Dupe;
use either::Either;
use itertools::Itertools;
use once_cell::unsync;
use starlark::any::ProvidesStaticType;
use starlark::docs::DocItem;
use starlark::docs::DocString;
use starlark::docs::DocStringKind;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Arguments;
use starlark::eval::Evaluator;
use starlark::eval::ParametersSpec;
use starlark::typing::Param;
use starlark::typing::Ty;
use starlark::typing::TyFunction;
use starlark::typing::TyStarlarkValue;
use starlark::values::dict::AllocDict;
use starlark::values::dict::DictRef;
use starlark::values::list::AllocList;
use starlark::values::list::ListRef;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::typing::TypeCompiled;
use starlark::values::typing::TypeInstanceId;
use starlark::values::typing::TypeMatcher;
use starlark::values::typing::TypeMatcherFactory;
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
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::interpreter::rule_defs::provider::doc::provider_callable_documentation;
use crate::interpreter::rule_defs::provider::ty::abstract_provider::AbstractProvider;
use crate::interpreter::rule_defs::provider::ty::provider::ty_provider;
use crate::interpreter::rule_defs::provider::ty::provider_callable::ty_provider_callable;
use crate::interpreter::rule_defs::provider::user::user_provider_creator;
use crate::interpreter::rule_defs::provider::user::UserProvider;

#[derive(Debug, thiserror::Error)]
enum ProviderCallableError {
    #[error(
        "The result of `provider()` must be assigned to a top-level variable before it can be called"
    )]
    NotBound,
    #[error(
        "Provider type must be assigned to a variable, e.g. `ProviderInfo = provider(fields = {0:?})`"
    )]
    ProviderNotAssigned(SmallSet<String>),
    #[error("non-unique field names: [{}]", .0.iter().map(|s| format!("`{}`", s)).join(", "))]
    NonUniqueFields(Vec<String>),
    #[error("Field default value can be either frozen value or an empty list or dict")]
    InvalidDefaultValue,
    #[error("Default value `{0}` (type `{1}`) does not match field type `{2}`")]
    InvalidDefaultValueType(String, &'static str, Ty),
}

fn create_callable_function_signature(
    function_name: &str,
    fields: &SmallMap<String, UserProviderField>,
    ret_ty: Ty,
) -> (ParametersSpec<FrozenValue>, TyFunction) {
    let mut signature = ParametersSpec::with_capacity(function_name.to_owned(), fields.len());
    let mut ty_params = Vec::with_capacity(fields.len());
    // TODO(nmj): Should double check we don't actually need positional args in-repo
    signature.no_more_positional_args();
    for (name, field) in fields {
        if field.default.is_some() {
            signature.optional(name);
            ty_params.push(Param::name_only(name, field.ty.as_ty().dupe()).optional());
        } else {
            signature.required(name);
            ty_params.push(Param::name_only(name, field.ty.as_ty().dupe()));
        }
    }

    (signature.finish(), TyFunction::new(ty_params, ret_ty))
}

#[derive(Debug, Allocative)]
pub(crate) struct UserProviderCallableData {
    pub(crate) provider_id: Arc<ProviderId>,
    /// Type id of provider callable instance.
    pub(crate) ty_provider_type_instance_id: TypeInstanceId,
    pub(crate) fields: SmallMap<String, UserProviderField>,
}

/// Initialized after the name is assigned to the provider.
#[derive(Debug, Trace, Allocative)]
struct UserProviderCallableNamed {
    /// The name of this provider, filled in by `export_as()`. This must be set before this
    /// object can be called and Providers created.
    id: Arc<ProviderId>,
    signature: ParametersSpec<FrozenValue>,
    /// This field is shared with provider instances.
    data: FrozenRef<'static, UserProviderCallableData>,
    /// Type of provider instance.
    ty_provider: Ty,
    /// Type of provider callable.
    ty_callable: Ty,
}

impl UserProviderCallableNamed {
    fn invoke<'v>(
        &self,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.signature.parser(args, eval, |parser, eval| {
            user_provider_creator(self.data, eval, parser)
        })
    }
}

#[derive(Debug, Trace, Allocative, ProvidesStaticType, NoSerialize, Clone, Dupe)]
pub(crate) struct UserProviderField {
    /// Field type.
    pub(crate) ty: TypeCompiled<FrozenValue>,
    /// Default value. If `None`, the field is required.
    pub(crate) default: Option<FrozenValue>,
}

impl<'v> AllocValue<'v> for UserProviderField {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

impl Display for UserProviderField {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ProviderField({}, ", self.ty)?;
        if let Some(default) = &self.default {
            write!(f, "default = {}", default)?;
        } else {
            write!(f, "required")?;
        }
        write!(f, ")")
    }
}

impl UserProviderField {
    pub(crate) fn default() -> UserProviderField {
        UserProviderField {
            ty: TypeCompiled::any(),
            default: Some(FrozenValue::new_none()),
        }
    }
}

#[starlark_value(type = "ProviderField")]
impl<'v> StarlarkValue<'v> for UserProviderField {}

/// The result of calling `provider()`. This is a callable that accepts the fields
/// provided in the `provider()` call, and generates a Starlark `UserProvider` object.
///
/// This object must be assigned to a variable at the top level of the module before it may be invoked
///
/// Field values default to `None`
#[derive(Debug, ProvidesStaticType, Trace, NoSerialize, Allocative)]
pub struct UserProviderCallable {
    /// The path where this `ProviderCallable` is created and assigned
    path: CellPath,
    /// The docstring for this provider
    docs: Option<DocString>,
    /// The names of the fields used in `callable`
    fields: SmallMap<String, UserProviderField>,
    /// Field is initialized after the provider is assigned to a variable.
    callable: unsync::OnceCell<UserProviderCallableNamed>,
}

fn user_provider_callable_display(
    id: Option<&Arc<ProviderId>>,
    fields: &SmallMap<String, UserProviderField>,
    f: &mut Formatter,
) -> fmt::Result {
    write!(f, "provider")?;
    if let Some(id) = id {
        write!(f, "[{}]", id.name)?;
    }
    write!(f, "(fields={{")?;
    for (i, (name, ty)) in fields.iter().enumerate() {
        if i != 0 {
            write!(f, ", ")?;
        }
        write!(f, "\"{}\": provider_field({}", name, ty.ty)?;
        if let Some(default) = ty.default {
            write!(f, ", default={}", default)?;
        }
        write!(f, ")")?;
    }
    write!(f, "}})")?;
    Ok(())
}

impl Display for UserProviderCallable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        user_provider_callable_display(self.id(), &self.fields, f)
    }
}

impl UserProviderCallable {
    fn new(
        path: CellPath,
        docs: Option<DocString>,
        fields: SmallMap<String, UserProviderField>,
    ) -> Self {
        Self {
            callable: unsync::OnceCell::new(),
            path,
            docs,
            fields,
        }
    }
}

impl ProviderCallableLike for UserProviderCallable {
    fn id(&self) -> Option<&Arc<ProviderId>> {
        self.callable.get().map(|x| &x.id)
    }
}

impl<'v> AllocValue<'v> for UserProviderCallable {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl Freeze for UserProviderCallable {
    type Frozen = FrozenUserProviderCallable;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let callable = self.callable.into_inner();
        let callable = match callable {
            Some(x) => x,
            None => {
                // Unfortunately we have no name or location for the provider at this point,
                // so reproduce the fields so that the provider can be identified.
                return Err(ProviderCallableError::ProviderNotAssigned(
                    self.fields.into_iter().map(|(name, _)| name).collect(),
                )
                .into());
            }
        };

        Ok(FrozenUserProviderCallable::new(
            self.docs,
            self.fields,
            callable,
        ))
    }
}

#[derive(Debug, Clone, Allocative)]
struct UserProviderMatcher {
    type_instance_id: TypeInstanceId,
}

impl TypeMatcher for UserProviderMatcher {
    fn matches(&self, value: Value) -> bool {
        match UserProvider::from_value(value) {
            Some(x) => {
                // TODO(nga): this is a bit suboptimal:
                //   instead we could compare just a pointer to the callable.
                x.callable.ty_provider_type_instance_id == self.type_instance_id
            }
            None => false,
        }
    }
}

#[starlark_value(type = "provider_callable")]
impl<'v> StarlarkValue<'v> for UserProviderCallable {
    type Canonical = FrozenUserProviderCallable;

    fn export_as(&self, variable_name: &str, eval: &mut Evaluator<'v, '_>) -> anyhow::Result<()> {
        // First export wins
        self.callable.get_or_try_init(|| {
            let provider_id = Arc::new(ProviderId {
                path: Some(self.path.clone()),
                name: variable_name.to_owned(),
            });
            let ty_provider_type_instance_id = TypeInstanceId::gen();
            let ty_provider = ty_provider(
                &provider_id.name,
                ty_provider_type_instance_id,
                TyStarlarkValue::new::<UserProvider>(),
                Some(TypeMatcherFactory::new(UserProviderMatcher {
                    type_instance_id: ty_provider_type_instance_id,
                })),
                self.fields
                    .iter()
                    .map(|(name, field)| (name.to_owned(), field.ty.as_ty().dupe()))
                    .collect(),
            )?;
            let (signature, creator_func) = create_callable_function_signature(
                &provider_id.name,
                &self.fields,
                ty_provider.clone(),
            );
            let ty_callable = ty_provider_callable::<UserProviderCallable>(creator_func)?;
            anyhow::Ok(UserProviderCallableNamed {
                id: provider_id.dupe(),
                signature,
                data: eval
                    .frozen_heap()
                    .alloc_any_display_from_debug(UserProviderCallableData {
                        provider_id,
                        fields: self.fields.clone(),
                        ty_provider_type_instance_id,
                    }),
                ty_provider,
                ty_callable,
            })
        })?;
        Ok(())
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
        match self.callable.get() {
            Some(callable) => callable.invoke(args, eval),
            None => Err(ProviderCallableError::NotBound.into()),
        }
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn ProviderCallableLike>(self);
    }

    fn eval_type(&self) -> Option<Ty> {
        self.callable.get().map(|named| named.ty_provider.dupe())
    }

    fn documentation(&self) -> Option<DocItem> {
        let return_types = vec![Ty::any(); self.fields.len()];
        Some(provider_callable_documentation(
            None,
            &self.docs,
            &self.fields.keys().map(|x| x.as_str()).collect::<Vec<_>>(),
            // TODO(nga): types.
            &vec![None; self.fields.len()],
            &return_types,
        ))
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        self.callable.get().map(|named| named.ty_callable.dupe())
    }
}

#[derive(Debug, ProvidesStaticType, NoSerialize, Allocative)]
pub struct FrozenUserProviderCallable {
    /// The docstring for this provider
    docs: Option<DocString>,
    /// The names of the fields used in `callable`
    fields: SmallMap<String, UserProviderField>,
    /// The actual callable that creates instances of `UserProvider`
    callable: UserProviderCallableNamed,
}
starlark_simple_value!(FrozenUserProviderCallable);

impl Display for FrozenUserProviderCallable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        user_provider_callable_display(Some(&self.callable.id), &self.fields, f)
    }
}

impl FrozenUserProviderCallable {
    fn new(
        docs: Option<DocString>,
        fields: SmallMap<String, UserProviderField>,
        callable: UserProviderCallableNamed,
    ) -> Self {
        Self {
            docs,
            fields,
            callable,
        }
    }
}

impl ProviderCallableLike for FrozenUserProviderCallable {
    fn id(&self) -> Option<&Arc<ProviderId>> {
        Some(&self.callable.id)
    }
}

#[starlark_value(type = "provider_callable")]
impl<'v> StarlarkValue<'v> for FrozenUserProviderCallable {
    type Canonical = Self;

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
        let return_types = vec![Ty::any(); self.fields.len()];
        Some(provider_callable_documentation(
            None,
            &self.docs,
            &self.fields.keys().map(|x| x.as_str()).collect::<Vec<_>>(),
            &vec![None; self.fields.len()],
            &return_types,
        ))
    }

    fn typechecker_ty(&self) -> Option<Ty> {
        Some(self.callable.ty_callable.dupe())
    }

    fn eval_type(&self) -> Option<Ty> {
        Some(self.callable.ty_provider.dupe())
    }
}

#[starlark_module]
fn provider_callable_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn r#type<'v>(this: Value<'v>, heap: &Heap) -> anyhow::Result<Value<'v>> {
        if let Some(x) = this.downcast_ref::<UserProviderCallable>() {
            match x.callable.get() {
                None => Err(ProviderCallableError::ProviderNotAssigned(
                    x.fields.keys().cloned().collect(),
                )
                .into()),
                Some(named) => Ok(heap.alloc(named.id.name.as_str())),
            }
        } else if let Some(x) = this.downcast_ref::<FrozenUserProviderCallable>() {
            Ok(heap.alloc(x.callable.id.name.as_str()))
        } else {
            unreachable!(
                "This parameter must be one of the types, but got `{}`",
                this.get_type()
            )
        }
    }
}

#[starlark_module]
pub fn register_provider(builder: &mut GlobalsBuilder) {
    /// Create a field definition object which can be passed to `provider` type constructor.
    fn provider_field<'v>(
        #[starlark(require=pos)] ty: Value<'v>,
        #[starlark(require=named)] default: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<UserProviderField> {
        let ty = TypeCompiled::new(ty, eval.heap())?.to_frozen(eval.frozen_heap());
        let default = match default {
            None => None,
            Some(x) => {
                if let Some(x) = x.unpack_frozen() {
                    Some(x)
                } else if ListRef::from_value(x).map_or(false, |x| x.is_empty()) {
                    Some(eval.frozen_heap().alloc(AllocList::EMPTY))
                } else if DictRef::from_value(x).map_or(false, |x| x.is_empty()) {
                    Some(eval.frozen_heap().alloc(AllocDict::EMPTY))
                } else {
                    // Dealing only with frozen values is much easier.
                    return Err(ProviderCallableError::InvalidDefaultValue.into());
                }
            }
        };
        if let Some(default) = default {
            if !ty.matches(default.to_value()) {
                return Err(ProviderCallableError::InvalidDefaultValueType(
                    default.to_string(),
                    default.to_value().get_type(),
                    ty.as_ty().dupe(),
                )
                .into());
            }
        }
        Ok(UserProviderField { ty, default })
    }

    /// Create a `"provider"` type that can be returned from `rule` implementations.
    /// Used to pass information from a rule to the things that depend on it.
    /// Typically named with an `Info` suffix.
    ///
    /// ```python
    /// GroovyLibraryInfo(fields = [
    ///     "objects",  # a list of artifacts
    ///     "options",  # a string containing compiler options
    /// ])
    /// ```
    ///
    /// Given a dependency you can obtain the provider with `my_dep[GroovyLibraryInfo]`
    /// which returns either `None` or a value of type `GroovyLibraryInfo`.
    ///
    /// For providers that accumulate upwards a transitive set is often a good choice.
    fn provider<'v>(
        #[starlark(require=named, default = "")] doc: &str,
        #[starlark(require=named)] fields: Either<Vec<String>, SmallMap<String, Value<'v>>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<UserProviderCallable> {
        let docstring = DocString::from_docstring(DocStringKind::Starlark, doc);
        let path = starlark_path_from_build_context(eval)?.path();

        let fields = match fields {
            Either::Left(fields) => {
                let new_fields: SmallMap<String, UserProviderField> = fields
                    .iter()
                    .map(|name| (name.clone(), UserProviderField::default()))
                    .collect();
                if new_fields.len() != fields.len() {
                    return Err(ProviderCallableError::NonUniqueFields(fields).into());
                }
                new_fields
            }
            Either::Right(fields) => {
                let mut new_fields = SmallMap::with_capacity(fields.len());
                for (name, field) in fields {
                    if let Some(field) = field.downcast_ref::<UserProviderField>() {
                        new_fields.insert(name, field.dupe());
                    } else {
                        let ty = TypeCompiled::new(field, eval.heap())
                            .with_context(|| format!("Field `{name}` type `{field}` is not created with `provider_field`, and cannot be evaluated as a type"))?
                            .to_frozen(eval.frozen_heap());
                        new_fields.insert(name, UserProviderField { ty, default: None });
                    }
                }
                new_fields
            }
        };
        Ok(UserProviderCallable::new(
            path.into_owned(),
            docstring,
            fields,
        ))
    }

    /// Provider type, can be used in type expressions.
    ///
    /// # Examples
    ///
    /// ```python
    /// def foo() -> list[Provider]:
    ///     return [DefaultInfo()]
    /// ```
    const Provider: StarlarkValueAsType<AbstractProvider> = StarlarkValueAsType::new();
}
