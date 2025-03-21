/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::bzl::ImportPath;
use buck2_error::BuckErrorContext;
use buck2_interpreter::build_context::starlark_path_from_build_context;
use buck2_interpreter::paths::path::StarlarkPath;
use derive_more::Display;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::coerce::coerce;
use starlark::coerce::Coerce;
use starlark::collections::SmallMap;
use starlark::collections::StarlarkHasher;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::typing::Ty;
use starlark::typing::TyStarlarkValue;
use starlark::typing::TyUser;
use starlark::typing::TyUserParams;
use starlark::values::list::ListType;
use starlark::values::starlark_value;
use starlark::values::typing::FrozenStarlarkCallable;
use starlark::values::typing::StarlarkCallableChecked;
use starlark::values::typing::TypeInstanceId;
use starlark::values::typing::TypeMatcherFactory;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueOfUncheckedGeneric;

use crate::interpreter::rule_defs::transitive_set::transitive_set::TransitiveSetMatcher;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetError;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum TransitiveSetDefinitionError {
    #[error("`transitive_set()` can only be used in `bzl` files")]
    TransitiveSetOnlyInBzl,
}

#[derive(Debug, Clone, Dupe, Copy, Trace, Freeze, PartialEq, Allocative)]
pub enum TransitiveSetProjectionKind {
    Args,
    Json,
}

impl TransitiveSetProjectionKind {
    pub fn short_name(&self) -> &'static str {
        match self {
            TransitiveSetProjectionKind::Args => "args",
            TransitiveSetProjectionKind::Json => "json",
        }
    }

    pub fn function_name(&self) -> &'static str {
        match self {
            TransitiveSetProjectionKind::Args => "project_as_args",
            TransitiveSetProjectionKind::Json => "project_as_json",
        }
    }
}

// The Coerce derivation doesn't work if this is just a tuple in the SmallMap value.
#[derive(Debug, Clone, Trace, Coerce, Freeze, Allocative)]
#[repr(C)]
pub struct TransitiveSetProjectionSpec<V: ValueLifetimeless> {
    pub kind: TransitiveSetProjectionKind,
    pub projection: ValueOfUncheckedGeneric<V, FrozenStarlarkCallable<(FrozenValue,), FrozenValue>>,
}

/// A unique identity for a given [`TransitiveSetDefinition`].
#[derive(Debug, Clone, Display, Allocative, Hash)]
#[display("{}", name)]
struct TransitiveSetId {
    module_id: ImportPath,
    name: String,
}

#[derive(Debug, Allocative)]
pub(crate) struct TransitiveSetDefinitionExported {
    /// The name of this transitive set. This is filed in by `export_as` when it's assigned to a
    /// top-level variable. This must be set before this is used.
    id: Arc<TransitiveSetId>,
    /// Type of transitive set type.
    set_ty: Ty,
    /// Type id of transitive set type.
    pub(crate) set_type_instance_id: TypeInstanceId,
}

#[derive(Debug, ProvidesStaticType, Allocative, Trace)]
pub struct TransitiveSetDefinition<'v> {
    pub(crate) exported: std::cell::OnceCell<TransitiveSetDefinitionExported>,

    /// The module id where this `TransitiveSetDefinition` is created and assigned
    module_id: ImportPath,

    operations: TransitiveSetOperationsGen<Value<'v>>,
}

#[derive(Debug, Clone, Trace, Coerce, Freeze, Allocative)]
#[repr(C)]
pub struct TransitiveSetOperationsGen<V: ValueLifetimeless> {
    /// Callables that will project the values contained in transitive sets of this type to
    /// cmd_args or json. This can be used to include a transitive set into a command or json file.
    pub(crate) projections: SmallMap<String, TransitiveSetProjectionSpec<V>>,

    /// Callables that will reduce the values contained in transitive sets to a single value per
    /// node. This can be used to e.g. aggregate flags throughout a transitive set;
    pub(crate) reductions: SmallMap<
        String,
        ValueOfUncheckedGeneric<
            V,
            FrozenStarlarkCallable<(ListType<FrozenValue>, FrozenValue), FrozenValue>,
        >,
    >,
}

pub type TransitiveSetOperations<'v> = TransitiveSetOperationsGen<Value<'v>>;

impl<V: ValueLifetimeless> TransitiveSetOperationsGen<V> {
    pub fn valid_projections(&self, kind: TransitiveSetProjectionKind) -> Vec<String> {
        self.projections
            .iter()
            .filter_map(|(k, spec)| {
                if kind == spec.kind {
                    Some(k.to_string())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    pub fn get_index_of_projection(
        &self,
        kind: TransitiveSetProjectionKind,
        proj: &str,
    ) -> buck2_error::Result<usize> {
        let index = match self.projections.get_index_of(proj) {
            Some(index) => index,
            None => {
                return Err(TransitiveSetError::ProjectionDoesNotExist {
                    projection: proj.to_owned(),
                    valid_projections: self.valid_projections(TransitiveSetProjectionKind::Args),
                }
                .into());
            }
        };

        let (_, spec) = self.projections.get_index(index).unwrap();
        if spec.kind != kind {
            return Err(TransitiveSetError::ProjectionKindMismatch {
                projection: proj.to_owned(),
                expected_kind: kind,
                actual_kind: spec.kind,
            }
            .into());
        }

        Ok(index)
    }
}

impl<'v> Display for TransitiveSetDefinition<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.exported.get() {
            Some(exported) => {
                write!(f, "{}", exported.id)
            }
            None => write!(f, "unnamed transitive set"),
        }
    }
}

impl<'v> Serialize for TransitiveSetDefinition<'v> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.serialize_str(&format!("{}", self))
    }
}

impl<'v> TransitiveSetDefinition<'v> {
    fn new(module_id: ImportPath, operations: TransitiveSetOperations<'v>) -> Self {
        Self {
            exported: std::cell::OnceCell::new(),
            module_id,
            operations,
        }
    }

    pub fn has_id(&self) -> bool {
        self.exported.get().is_some()
    }
}

impl<'v> AllocValue<'v> for TransitiveSetDefinition<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

#[starlark_value(type = "transitive_set_definition")]
impl<'v> StarlarkValue<'v> for TransitiveSetDefinition<'v> {
    type Canonical = FrozenTransitiveSetDefinition;

    fn export_as(
        &self,
        variable_name: &str,
        _: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<()> {
        // First export wins
        self.exported.get_or_try_init(|| {
            let id = Arc::new(TransitiveSetId {
                module_id: self.module_id.clone(),
                name: variable_name.to_owned(),
            });
            let set_type_instance_id = TypeInstanceId::gen();
            let set_ty = Ty::custom(TyUser::new(
                variable_name.to_owned(),
                TyStarlarkValue::new::<TransitiveSet>(),
                set_type_instance_id,
                TyUserParams {
                    matcher: Some(TypeMatcherFactory::new(TransitiveSetMatcher {
                        type_instance_id: set_type_instance_id,
                    })),

                    ..TyUserParams::default()
                },
            )?);
            buck2_error::Ok(TransitiveSetDefinitionExported {
                id,
                set_ty,
                set_type_instance_id,
            })
        })?;
        Ok(())
    }

    fn dir_attr(&self) -> Vec<String> {
        vec!["type".to_owned()]
    }

    fn has_attr(&self, attribute: &str, _heap: &'v Heap) -> bool {
        attribute == "type"
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        if attribute == "type" {
            let typ = self
                .exported
                .get()
                .map_or("transitive_set_definition", |exported| {
                    exported.id.name.as_str()
                });
            Some(heap.alloc(typ))
        } else {
            None
        }
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        let exported = self
            .exported
            .get()
            .buck_error_context("cannot hash a transitive_set_definition without id")?;
        exported.id.hash(hasher);
        Ok(())
    }

    fn eval_type(&self) -> Option<Ty> {
        self.exported.get().map(|exported| exported.set_ty.dupe())
    }
}

#[derive(Display, ProvidesStaticType, Allocative)]
#[display("{}", exported.id)]
pub struct FrozenTransitiveSetDefinition {
    pub(crate) exported: TransitiveSetDefinitionExported,

    operations: TransitiveSetOperationsGen<FrozenValue>,
}

impl fmt::Debug for FrozenTransitiveSetDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "TransitiveSetDefinition({} declared in {})",
            self.exported.id.name, self.exported.id.module_id
        )
    }
}

impl Serialize for FrozenTransitiveSetDefinition {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.serialize_str(&format!("{}", self))
    }
}

#[starlark_value(type = "transitive_set_definition")]
impl<'v> StarlarkValue<'v> for FrozenTransitiveSetDefinition {
    type Canonical = Self;

    fn dir_attr(&self) -> Vec<String> {
        vec!["type".to_owned()]
    }

    fn has_attr(&self, attribute: &str, _heap: &'v Heap) -> bool {
        attribute == "type"
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        if attribute == "type" {
            let typ = self.exported.id.name.as_str();
            Some(heap.alloc(typ))
        } else {
            None
        }
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        self.exported.id.hash(hasher);
        Ok(())
    }

    fn eval_type(&self) -> Option<Ty> {
        Some(self.exported.set_ty.dupe())
    }
}

starlark_simple_value!(FrozenTransitiveSetDefinition);

impl<'v> Freeze for TransitiveSetDefinition<'v> {
    type Frozen = FrozenTransitiveSetDefinition;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let Self {
            exported,
            module_id: _,
            operations,
        } = self;

        let exported = match exported.into_inner() {
            Some(x) => x,
            None => {
                // Unfortunately we have no name or location for the definition at this point.
                return Err(FreezeError::new(
                    TransitiveSetError::TransitiveSetNotAssigned.to_string(),
                ));
            }
        };

        let operations = operations.freeze(freezer)?;

        Ok(FrozenTransitiveSetDefinition {
            exported,
            operations,
        })
    }
}

pub trait TransitiveSetDefinitionLike<'v> {
    fn has_id(&self) -> bool;

    fn as_debug(&self) -> &dyn fmt::Debug;

    fn operations(&self) -> &TransitiveSetOperations<'v>;
}

impl<'v> TransitiveSetDefinitionLike<'v> for TransitiveSetDefinition<'v> {
    fn has_id(&self) -> bool {
        Self::has_id(self)
    }

    fn as_debug(&self) -> &dyn fmt::Debug {
        self
    }

    fn operations(&self) -> &TransitiveSetOperations<'v> {
        &self.operations
    }
}

impl<'v> TransitiveSetDefinitionLike<'v> for FrozenTransitiveSetDefinition {
    fn has_id(&self) -> bool {
        true
    }

    fn as_debug(&self) -> &dyn fmt::Debug {
        self
    }

    fn operations(&self) -> &TransitiveSetOperations<'v> {
        coerce(&self.operations)
    }
}

#[starlark_module]
pub fn register_transitive_set(builder: &mut GlobalsBuilder) {
    fn transitive_set<'v>(
        #[starlark(require = named)] args_projections: Option<
            SmallMap<String, StarlarkCallableChecked<'v, (Value<'v>,), Value<'v>>>,
        >,
        #[starlark(require = named)] json_projections: Option<
            SmallMap<String, StarlarkCallableChecked<'v, (Value<'v>,), Value<'v>>>,
        >,
        #[starlark(require = named)] reductions: Option<
            SmallMap<
                String,
                StarlarkCallableChecked<'v, (ListType<Value<'v>>, Value<'v>), Value<'v>>,
            >,
        >,
        eval: &mut Evaluator,
    ) -> starlark::Result<TransitiveSetDefinition<'v>> {
        let projections: SmallMap<_, _> = args_projections
            .into_iter()
            .flat_map(|v| v.into_iter())
            .map(|(k, v)| {
                (
                    k,
                    TransitiveSetProjectionSpec {
                        kind: TransitiveSetProjectionKind::Args,
                        projection: ValueOfUncheckedGeneric::new(v.0),
                    },
                )
            })
            .chain(
                json_projections
                    .into_iter()
                    .flat_map(|v| v.into_iter())
                    .map(|(k, v)| {
                        (
                            k,
                            TransitiveSetProjectionSpec {
                                kind: TransitiveSetProjectionKind::Json,
                                projection: ValueOfUncheckedGeneric::new(v.0),
                            },
                        )
                    }),
            )
            .collect();

        let reductions = reductions
            .unwrap_or_default()
            .into_iter()
            .map(|(k, v)| (k, ValueOfUncheckedGeneric::new(v.0)))
            .collect();

        let starlark_path: StarlarkPath = starlark_path_from_build_context(eval)?;
        Ok(TransitiveSetDefinition::new(
            match starlark_path {
                StarlarkPath::LoadFile(import_path) => import_path.clone(),
                _ => {
                    return Err(buck2_error::Error::from(
                        TransitiveSetDefinitionError::TransitiveSetOnlyInBzl,
                    )
                    .into());
                }
            },
            TransitiveSetOperations {
                projections,
                reductions,
            },
        ))
    }
}
