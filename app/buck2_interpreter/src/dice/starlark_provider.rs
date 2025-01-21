/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::DerefMut;
use std::sync::Arc;

use allocative::Allocative;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::package::PackageLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::internal_error;
use buck2_util::arc_str::ThinArcStr;
use dice::DiceComputations;
use dupe::Dupe;
use starlark::environment::Module;
use starlark::eval::Evaluator;

use crate::dice::starlark_debug::HasStarlarkDebugger;
use crate::factory::StarlarkEvaluatorProvider;
use crate::paths::module::OwnedStarlarkModulePath;
use crate::starlark_debug::StarlarkDebugController;
use crate::starlark_profiler::data::ProfileTarget;
use crate::starlark_profiler::profiler::StarlarkProfilerOpt;

pub trait DynEvalKindKey: Display + Send + Sync + Debug + Allocative + 'static {
    fn hash(&self, state: &mut dyn Hasher);
    fn eq(&self, other: &dyn DynEvalKindKey) -> bool;
    fn as_any(&self) -> &dyn Any;
}

impl<T: Display + Send + Sync + Debug + Allocative + Hash + Eq + PartialEq + Any + 'static>
    DynEvalKindKey for T
{
    fn hash(&self, mut state: &mut dyn Hasher) {
        Hash::hash(self, &mut state)
    }

    fn eq(&self, other: &dyn DynEvalKindKey) -> bool {
        match other.as_any().downcast_ref::<Self>() {
            None => false,
            Some(v) => v == self,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Hash for dyn DynEvalKindKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        DynEvalKindKey::hash(self, state)
    }
}

impl PartialEq for dyn DynEvalKindKey {
    fn eq(&self, other: &Self) -> bool {
        DynEvalKindKey::eq(self, other)
    }
}

impl Eq for dyn DynEvalKindKey {}

/// StarlarkEvalKind is used as an identifier for a particular starlark evaluation.
///
/// It's used to selectively enable profiling and provides an identifier for the debugger.
#[derive(Debug, Clone, Dupe, Hash, Eq, PartialEq, Allocative)]
pub enum StarlarkEvalKind {
    Analysis(ConfiguredTargetLabel),
    Load(Arc<OwnedStarlarkModulePath>),
    LoadPackageFile(PackageLabel),
    LoadBuildFile(PackageLabel),
    Transition(Arc<TransitionId>),
    // These types are defined in higher crates, so we just accept dyn DynEvalKindKey here.
    AnonTarget(Arc<dyn DynEvalKindKey>),
    Bxl(Arc<dyn DynEvalKindKey>),
    BxlDynamic(Arc<dyn DynEvalKindKey>),
    Unknown(ThinArcStr),
}

impl StarlarkEvalKind {
    pub fn to_profile_target(&self) -> buck2_error::Result<ProfileTarget> {
        match self {
            StarlarkEvalKind::Analysis(label) => Ok(ProfileTarget::Analysis(label.dupe())),
            StarlarkEvalKind::LoadBuildFile(package) => Ok(ProfileTarget::Loading(package.dupe())),
            StarlarkEvalKind::Bxl(_) => Ok(ProfileTarget::Bxl),
            StarlarkEvalKind::BxlDynamic(_) => Ok(ProfileTarget::Bxl),
            v => Err(internal_error!("no profiletarget type for {}", v)),
        }
    }
}

impl std::fmt::Display for StarlarkEvalKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StarlarkEvalKind::Analysis(target) => write!(f, "analysis:{}", target),
            StarlarkEvalKind::Load(module) => write!(f, "load:{}", module),
            StarlarkEvalKind::LoadPackageFile(package) => write!(f, "load_packagefile:{}", package),
            StarlarkEvalKind::LoadBuildFile(package) => write!(f, "load_buildfile:{}", package),
            StarlarkEvalKind::AnonTarget(key) => write!(f, "anon_analysis:{}", key),
            StarlarkEvalKind::Transition(id) => write!(f, "transition:{}", id),
            StarlarkEvalKind::Bxl(key) => write!(f, "bxl:{}", key),
            StarlarkEvalKind::BxlDynamic(key) => write!(f, "bxl_dynamic:{}", key),
            StarlarkEvalKind::Unknown(key) => write!(f, "generic_starlark:{}", key),
        }
    }
}

/// This constructs an appropriate StarlarkEvaluatorProvider to set up
/// profiling/instrumentation/debugging in a starlark Evaluator for buck.
///
/// Taking this via a closure ensures that the Evaluator isn't used in an
/// async context and allows us to do things like the block_in_place required
/// when debugging.
///
/// The kind is used for the thread name when debugging.
///
/// The provided closure will be invoked and passed an appropriate
/// StarlarkEvaluatorProvider.
pub async fn with_starlark_eval_provider<'a, D: DerefMut<Target = DiceComputations<'a>>, R>(
    mut ctx: D,
    profiler_instrumentation: &mut StarlarkProfilerOpt<'_>,
    kind: &StarlarkEvalKind,
    closure: impl FnOnce(&mut dyn StarlarkEvaluatorProvider, D) -> buck2_error::Result<R>,
) -> buck2_error::Result<R> {
    let root_buckconfig = ctx.get_legacy_root_config_on_dice().await?;

    let starlark_max_callstack_size =
        root_buckconfig
            .view(&mut ctx)
            .parse::<usize>(BuckconfigKeyRef {
                section: "buck2",
                property: "starlark_max_callstack_size",
            })?;

    let debugger_handle = ctx.get_starlark_debugger_handle();
    let debugger = match debugger_handle {
        Some(v) => Some(v.start_eval(&kind.to_string()).await?),
        None => None,
    };

    struct EvalProvider<'a, 'b> {
        profiler: &'a mut StarlarkProfilerOpt<'b>,
        debugger: Option<Box<dyn StarlarkDebugController>>,
        starlark_max_callstack_size: Option<usize>,
    }

    impl StarlarkEvaluatorProvider for EvalProvider<'_, '_> {
        fn make<'v, 'a, 'e>(
            &mut self,
            module: &'v Module,
        ) -> buck2_error::Result<(Evaluator<'v, 'a, 'e>, bool)> {
            let mut eval = Evaluator::new(module);
            if let Some(stack_size) = self.starlark_max_callstack_size {
                eval.set_max_callstack_size(stack_size)
                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
            }

            let is_profiling_enabled = self.profiler.initialize(&mut eval)?;
            if let Some(v) = &mut self.debugger {
                v.initialize(&mut eval)?;
            }
            Ok((eval, is_profiling_enabled))
        }

        fn evaluation_complete(&mut self, eval: &mut Evaluator) -> buck2_error::Result<()> {
            self.profiler.evaluation_complete(eval)
        }
    }

    {
        let mut provider = EvalProvider {
            profiler: profiler_instrumentation,
            debugger,
            starlark_max_callstack_size,
        };

        // If we're debugging, we need to move this to a tokio blocking task.
        //
        // This is required because the debugger itself is running on the
        // tokio worker tasks, and if we have a starlark breakpoint in common
        // code we could get a lot of evaluators all blocked waiting on the debugger
        // and those could block all the tokio worker tasks and the debugger wouldn't
        // even get a chance to resume them.
        //
        // It's the debuggers responsibility to ensure that we don't run too many
        // evaluations concurrently (in the non-debugger case they are limited by the
        // tokio worker tasks, but once in a blocking task that limit is greatly
        // increased).

        // TODO(cjhopman): It would be nicer if we could have this functionality be
        // provided by the debugger handle, but I couldn't figure out a nice clean
        // way to do that. Potentially the thing would be to invert the dependencies
        // so we could operate against a concrete type rather than injecting a trait
        // implementation.
        if debugger_handle.is_some() {
            tokio::task::block_in_place(move || closure(&mut provider, ctx))
        } else {
            closure(&mut provider, ctx)
        }
    }
}
