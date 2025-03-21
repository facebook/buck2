/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The context containing the available buck commands and query operations for `bxl` functions.

use std::cell::RefCell;
use std::io::Write;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use allocative::Allocative;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_common::target_aliases::HasTargetAliasResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::pattern::query_file_literal::parse_query_file_literal;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::console_message;
use buck2_execute::digest_config::DigestConfig;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use indexmap::IndexSet;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsStatic;
use starlark::values::starlark_value;
use starlark::values::structs::StructRef;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;

use crate::bxl::key::BxlKey;
use crate::bxl::starlark_defs::context::actions::BxlExecutionResolution;
use crate::bxl::starlark_defs::context::output::OutputStream;
use crate::bxl::starlark_defs::context::starlark_async::BxlDiceComputations;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::value_as_starlark_target_label::ValueAsStarlarkTargetLabel;

pub(crate) mod actions;
pub(crate) mod analysis;
pub(crate) mod anon_target;
pub(crate) mod build;
pub(crate) mod dynamic;
pub(crate) mod fs;
pub(crate) mod methods;
pub(crate) mod output;
pub(crate) mod starlark_async;

/// Errors that can occur when accessing some field of `BxlContext` for dynamic action or anon target.
#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum BxlContextError {
    #[error("`{0}()` is unsupported")]
    Unsupported(String),
    #[error("Execution platform is inherited from the root BXL")]
    RequireSameExecutionPlatformAsRoot,
}

#[derive(buck2_error::Error, Debug)]
#[error("Expected a single target as a string literal, not a target pattern")]
#[buck2(tag = Input)]
struct NotATargetLabelString;

#[derive(buck2_error::Error, Debug)]
#[error(
    "Unconfigured target label(s)/node(s) was passed into analysis. Targets passed into analysis should be configured."
)]
#[buck2(tag = Input)]
struct UnconfiguredTargetInAnalysis;

/// Data object for `BxlContextType::Root`.
#[derive(ProvidesStaticType, Trace, NoSerialize, Allocative, Debug, Derivative)]
pub(crate) struct RootBxlContextData<'v> {
    output_stream: ValueTyped<'v, OutputStream>,
    cli_args: ValueOfUnchecked<'v, StructRef<'v>>,
}

/// Data object for `BxlContextType::Dynamic`.
#[derive(ProvidesStaticType, Trace, Allocative, Debug, Derivative)]
pub(crate) struct DynamicBxlContextData {
    exec_deps: Vec<ConfiguredProvidersLabel>,
    toolchains: Vec<ConfiguredProvidersLabel>,
}

/// Environment-specific fields of `BxlContext`.
#[derive(ProvidesStaticType, Trace, NoSerialize, Allocative, Debug, Derivative)]
pub(crate) enum BxlContextType<'v> {
    /// Context passed to `ctx` parameter to BXL entry function
    Root(RootBxlContextData<'v>),
    /// Context passed to `ctx` parameter to the dynamic lambda entry function
    Dynamic(DynamicBxlContextData),
    AnonTarget,
}

impl<'v> BxlContextType<'v> {
    fn unpack_root(&self) -> buck2_error::Result<&'v RootBxlContextData> {
        match &self {
            BxlContextType::Root(root) => Ok(root),
            BxlContextType::Dynamic(_) => Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Expected root BXL context type"
            )),
            BxlContextType::AnonTarget => Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Expected root BXL context type"
            )),
        }
    }
}

impl<'v> Display for BxlContextType<'v> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            BxlContextType::Root { .. } => {
                write!(f, "root")
            }
            BxlContextType::Dynamic(_) => {
                write!(f, "dynamic")
            }
            BxlContextType::AnonTarget => {
                write!(f, "anon_target")
            }
        }
    }
}

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative
)]
#[derivative(Debug)]
#[display("{:?}", self)]
pub(crate) struct BxlContext<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    pub(crate) async_ctx: Rc<RefCell<dyn BxlDiceComputations + 'v>>,
    pub(crate) data: BxlContextNoDice<'v>,
}

impl<'v> Deref for BxlContext<'v> {
    type Target = BxlContextNoDice<'v>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

#[derive(Derivative, Display, Trace, NoSerialize, Allocative)]
#[derivative(Debug)]
#[display("{:?}", self)]
pub(crate) struct BxlContextNoDice<'v> {
    state: ValueTyped<'v, AnalysisActions<'v>>,
    context_type: BxlContextType<'v>,
    core: Rc<BxlContextCoreData>,
}

impl Deref for BxlContextNoDice<'_> {
    type Target = BxlContextCoreData;

    fn deref(&self) -> &Self::Target {
        &self.core
    }
}

#[derive(Derivative, Display, Trace, Allocative)]
#[derivative(Debug)]
#[display("{:?}", self)]
pub(crate) struct BxlContextCoreData {
    current_bxl: BxlKey,
    #[derivative(Debug = "ignore")]
    target_alias_resolver: BuckConfigTargetAliasResolver,
    cell_name: CellName,
    cell_root_abs: AbsNormPathBuf,
    #[derivative(Debug = "ignore")]
    cell_resolver: CellResolver,
    #[derivative(Debug = "ignore")]
    cell_alias_resolver: CellAliasResolver,
    project_fs: ProjectRoot,
    #[derivative(Debug = "ignore")]
    artifact_fs: ArtifactFs,
}

impl BxlContextCoreData {
    pub(crate) async fn new(
        key: BxlKey,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Self> {
        let label = key.label();
        let cell_resolver = dice.get_cell_resolver().await?;
        let cell = label.bxl_path.cell();
        let bxl_cell = cell_resolver
            .get(cell)
            .with_buck_error_context(|| format!("Cell does not exist: `{}`", cell))?
            .dupe();
        let cell_name = bxl_cell.name();
        let target_alias_resolver = dice.target_alias_resolver().await?;
        let cell_alias_resolver = dice.get_cell_alias_resolver(cell).await?;
        let artifact_fs = dice.get_artifact_fs().await?;
        let project_fs = dice.global_data().get_io_provider().project_root().dupe();

        let cell_root_abs = project_fs.root().join(
            cell_resolver
                .get(cell_name)?
                .path()
                .as_project_relative_path(),
        );

        Ok(Self {
            current_bxl: key,
            target_alias_resolver,
            cell_name,
            cell_root_abs,
            cell_resolver,
            cell_alias_resolver,
            project_fs,
            artifact_fs,
        })
    }

    pub(crate) fn key(&self) -> &BxlKey {
        &self.current_bxl
    }

    pub(crate) fn project_root(&self) -> &ProjectRoot {
        &self.project_fs
    }

    pub(crate) fn global_cfg_options(&self) -> &GlobalCfgOptions {
        self.current_bxl.global_cfg_options()
    }

    pub(crate) fn target_alias_resolver(&self) -> &BuckConfigTargetAliasResolver {
        &self.target_alias_resolver
    }

    pub(crate) fn cell_resolver(&self) -> &CellResolver {
        &self.cell_resolver
    }

    pub(crate) fn cell_name(&self) -> CellName {
        self.cell_name
    }

    pub(crate) fn cell_root_abs(&self) -> &AbsNormPathBuf {
        &self.cell_root_abs
    }

    pub(crate) fn cell_alias_resolver(&self) -> &CellAliasResolver {
        &self.cell_alias_resolver
    }

    pub(crate) fn current_bxl(&self) -> &BxlKey {
        &self.current_bxl
    }

    pub(crate) fn project_fs(&self) -> &ProjectRoot {
        &self.project_fs
    }

    pub(crate) fn artifact_fs(&self) -> &ArtifactFs {
        &self.artifact_fs
    }

    /// Working dir for resolving literals.
    /// Note, unlike buck2 command line UI, we resolve targets and literals
    /// against the cell root instead of user working dir.
    pub(crate) fn working_dir(&self) -> buck2_error::Result<ProjectRelativePathBuf> {
        let cell = self.cell_resolver().get(self.cell_name())?;
        Ok(cell.path().as_project_relative_path().to_owned())
    }

    pub(crate) fn parse_query_file_literal(&self, literal: &str) -> buck2_error::Result<CellPath> {
        parse_query_file_literal(
            literal,
            self.cell_alias_resolver(),
            self.cell_resolver(),
            // NOTE(nga): we pass cell root as working directory here,
            //   which is inconsistent with the rest of buck2:
            //   The same query `owner(foo.h)` is resolved using
            //   current directory in `buck2 query`, but relative to cell root in BXL.
            self.cell_root_abs(),
            self.project_root(),
        )
    }

    pub(crate) fn resolve_target_platfrom(
        &self,
        target_platform: ValueAsStarlarkTargetLabel<'_>,
    ) -> buck2_error::Result<Option<TargetLabel>> {
        target_platform.parse_target_platforms(
            self.target_alias_resolver(),
            self.cell_resolver(),
            self.cell_alias_resolver(),
            self.cell_name(),
            &self.global_cfg_options().target_platform,
        )
    }

    pub(crate) fn resolve_global_cfg_options(
        &self,
        target_platform: ValueAsStarlarkTargetLabel<'_>,
        modifiers: Vec<String>,
    ) -> buck2_error::Result<GlobalCfgOptions> {
        let target_platform = self.resolve_target_platfrom(target_platform);
        let global_cfg_options = target_platform.map(|target_platform| GlobalCfgOptions {
            target_platform,
            cli_modifiers: Arc::new(modifiers),
        })?;
        Ok(global_cfg_options)
    }
}

impl<'v> BxlContext<'v> {
    pub(crate) fn new(
        heap: &'v Heap,
        core: Rc<BxlContextCoreData>,
        cli_args: ValueOfUnchecked<'v, StructRef<'v>>,
        async_ctx: Rc<RefCell<BxlSafeDiceComputations<'v, '_>>>,
        output_sink: Rc<RefCell<dyn Write>>,
        error_sink: Rc<RefCell<dyn Write>>,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<Self> {
        let root_data = RootBxlContextData {
            cli_args,
            output_stream: heap.alloc_typed(OutputStream::new(
                core.project_fs.clone(),
                core.artifact_fs.clone(),
                output_sink,
                error_sink,
            )),
        };
        let context_type = BxlContextType::Root(root_data);

        Ok(Self {
            async_ctx: async_ctx.clone(),
            data: BxlContextNoDice {
                state: heap.alloc_typed(AnalysisActions {
                    state: RefCell::new(None),
                    attributes: None,
                    plugins: None,
                    digest_config,
                }),
                context_type,
                core,
            },
        })
    }

    pub(crate) fn new_dynamic(
        heap: &'v Heap,
        core: Rc<BxlContextCoreData>,
        async_ctx: Rc<RefCell<BxlSafeDiceComputations<'v, '_>>>,
        digest_config: DigestConfig,
        analysis_registry: AnalysisRegistry<'v>,
        dynamic_data: DynamicBxlContextData,
    ) -> buck2_error::Result<Self> {
        Ok(Self {
            async_ctx,
            data: BxlContextNoDice {
                state: heap.alloc_typed(AnalysisActions {
                    state: RefCell::new(Some(analysis_registry)),
                    attributes: None,
                    plugins: None,
                    digest_config,
                }),
                context_type: BxlContextType::Dynamic(dynamic_data),
                core,
            },
        })
    }

    pub(crate) fn new_anon(
        heap: &'v Heap,
        core: Rc<BxlContextCoreData>,
        async_ctx: Rc<RefCell<BxlSafeDiceComputations<'v, '_>>>,
        digest_config: DigestConfig,
        analysis_registry: AnalysisRegistry<'v>,
        attributes: ValueOfUnchecked<'v, StructRef<'static>>,
    ) -> buck2_error::Result<Self> {
        Ok(Self {
            async_ctx,
            data: BxlContextNoDice {
                state: heap.alloc_typed(AnalysisActions {
                    state: RefCell::new(Some(analysis_registry)),
                    attributes: Some(attributes),
                    plugins: None,
                    digest_config,
                }),
                context_type: BxlContextType::AnonTarget,
                core,
            },
        })
    }

    /// runs the async computation over dice as sync,
    /// This should generally only be called at the top level functions in bxl.
    /// Within the lambdas, use the existing reference to Dice provided instead of calling nested
    /// via_dice, as that breaks borrow invariants of the dice computations.
    pub(crate) fn via_dice<'a, 's, T>(
        &'a self,
        f: impl for<'x> FnOnce(
            &'x mut dyn BxlDiceComputations,
            &'a BxlContextNoDice<'v>,
        ) -> buck2_error::Result<T>,
    ) -> buck2_error::Result<T>
    where
        'v: 'a,
    {
        let data = &self.data;
        f(&mut *self.async_ctx.borrow_mut(), data)
    }

    /// Must take an `AnalysisContext` and `OutputStream` which has never had `take_state` called on it before.
    pub(crate) fn take_state(
        value: ValueTyped<'v, BxlContext<'v>>,
    ) -> buck2_error::Result<(AnalysisRegistry<'v>, IndexSet<ArtifactGroup>)> {
        let this = value.as_ref();
        let root_data = this.data.context_type.unpack_root()?;
        let output_stream = &root_data.output_stream;

        let analysis_registry = this
            .data
            .state
            .as_ref()
            .state
            .borrow_mut()
            .take()
            .map(Ok)
            .unwrap_or_else(|| {
                // BXL did not request actions, so we don't know execution platform.
                // It doesn't matter what owner/platform we put here because
                // the registry is empty, nothing will be fetched from it.
                AnalysisRegistry::new_from_owner(
                    this.core
                        .current_bxl
                        .dupe()
                        .into_base_deferred_key(BxlExecutionResolution::unspecified()),
                    ExecutionPlatformResolution::unspecified(),
                )
            })?;

        // artifacts should be bound by now as the bxl has finished running
        let artifacts = output_stream
            .as_ref()
            .take_artifacts()
            .into_iter()
            .map(|ensured_artifact| ensured_artifact.into_artifact_groups())
            .flatten_ok()
            .collect::<buck2_error::Result<IndexSet<ArtifactGroup>>>()?;

        Ok((analysis_registry, artifacts))
    }

    /// Take the state for dynamic action or anon target
    pub(crate) fn take_state_dynamic_or_anon_impl(
        &self,
    ) -> buck2_error::Result<AnalysisRegistry<'v>> {
        let state = self.data.state.as_ref();
        state.state()?.assert_no_promises()?;

        Ok(state
            .state
            .borrow_mut()
            .take()
            .expect("nothing to have stolen state yet"))
    }

    /// Must take an `AnalysisContext` which has never had `take_state` called on it before.
    pub(crate) fn take_state_dynamic(&self) -> buck2_error::Result<AnalysisRegistry<'v>> {
        self.take_state_dynamic_or_anon_impl()
    }

    /// Must take an `AnalysisContext` which has never had `take_state` called on it before.
    pub(crate) fn take_state_anon(&self) -> buck2_error::Result<AnalysisRegistry<'v>> {
        self.take_state_dynamic_or_anon_impl()
    }
}

pub(crate) trait ErrorPrinter {
    fn print_to_error_stream(&self, msg: String) -> buck2_error::Result<()>;
}

impl<'v> ErrorPrinter for BxlContextNoDice<'v> {
    // Used for caching error logs emitted from within the BXL core.
    fn print_to_error_stream(&self, msg: String) -> buck2_error::Result<()> {
        match &self.context_type {
            BxlContextType::Root(root) => {
                writeln!(root.output_stream.error_sink.borrow_mut(), "{}", msg)?
            }
            BxlContextType::Dynamic(_) => console_message(msg),
            BxlContextType::AnonTarget => console_message(msg),
        }
        Ok(())
    }
}

#[starlark_value(type = "bxl.Context", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for BxlContext<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(methods::bxl_context_methods)
    }
}

impl<'v> AllocValue<'v> for BxlContext<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}
