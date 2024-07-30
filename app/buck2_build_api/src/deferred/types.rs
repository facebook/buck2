/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::type_name;
use std::any::Any;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::future::Future;
use std::marker::PhantomData;
use std::slice;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::deferred::data::DeferredData;
use buck2_artifact::deferred::id::DeferredId;
use buck2_artifact::deferred::key::DeferredHolderKey;
use buck2_artifact::deferred::key::DeferredKey;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_execute::digest_config::DigestConfig;
use buck2_futures::cancellable_future::CancellationObserver;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dice::DiceComputations;
use dupe::Clone_;
use dupe::Dupe;
use dupe::Dupe_;
use dupe::OptionDupedExt;
use either::Either;
use gazebo::variants::VariantName;
use indexmap::IndexSet;

use crate::analysis::registry::RecordedAnalysisValues;
use crate::deferred::arc_borrow::ArcBorrow;

/// An asynchronous chunk of work that will be executed when requested.
/// The 'Deferred' can have "inputs" which are values that will be guaranteed to be ready to use
/// before the 'Deferred' is actually executed. These can be 'Artifact's, which means that those
/// 'Artifact's will be materialized and its corresponding 'Action's executed, or other
/// 'DeferredData', which means those 'Deferred' will be computed first.
///
/// `any::Provider` can be used to obtain data for introspection.
pub trait Deferred: Debug + Allocative + provider::Provider {
    type Output: DeferredOutput;

    /// the set of 'Deferred's that should be computed first before executing this 'Deferred'
    fn inputs(&self) -> DeferredInputsRef<'_>;

    /// executes this 'Deferred', assuming all inputs and input artifacts are already computed
    fn execute(
        &self,
        ctx: &mut dyn DeferredCtx,
        dice: &mut DiceComputations,
    ) -> impl Future<Output = anyhow::Result<DeferredValue<Self::Output>>> + Send;

    /// An optional stage to wrap execution in.
    fn span(&self) -> Option<buck2_data::span_start_event::Data> {
        None
    }
}

/// The context for executing a 'Deferred'.
pub trait DeferredCtx: Send {
    fn get_configured_target(&self, label: &ConfiguredTargetLabel)
    -> Option<&ConfiguredTargetNode>;

    fn get_action_key(&self) -> String;

    fn get_deferred_data(&self, key: &DeferredKey) -> anyhow::Result<DeferredValueAnyReady>;

    fn get_materialized_artifact(&self, artifact: &Artifact) -> Option<&ProjectRelativePath>;

    fn registry(&mut self) -> &mut DeferredRegistry;

    fn project_filesystem(&self) -> &ProjectRoot;

    fn digest_config(&self) -> DigestConfig;

    fn liveness(&self) -> CancellationObserver;
}

/// DeferredCtx with already resolved values
pub struct ResolveDeferredCtx<'a> {
    action_key: String,
    configured_targets: HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
    deferreds: HashMap<DeferredKey, DeferredValueAnyReady>,
    materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
    registry: &'a mut DeferredRegistry,
    project_filesystem: ProjectRoot,
    digest_config: DigestConfig,
    liveness: CancellationObserver,
}

impl<'a> ResolveDeferredCtx<'a> {
    pub fn new(
        action_key: String,
        configured_targets: HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
        deferreds: HashMap<DeferredKey, DeferredValueAnyReady>,
        materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
        registry: &'a mut DeferredRegistry,
        project_filesystem: ProjectRoot,
        digest_config: DigestConfig,
        liveness: CancellationObserver,
    ) -> Self {
        Self {
            action_key,
            configured_targets,
            deferreds,
            materialized_artifacts,
            registry,
            project_filesystem,
            digest_config,
            liveness,
        }
    }
}

impl<'a> DeferredCtx for ResolveDeferredCtx<'a> {
    fn get_configured_target(
        &self,
        label: &ConfiguredTargetLabel,
    ) -> Option<&ConfiguredTargetNode> {
        self.configured_targets.get(label)
    }

    fn get_action_key(&self) -> String {
        self.action_key.clone()
    }

    fn get_deferred_data(&self, key: &DeferredKey) -> anyhow::Result<DeferredValueAnyReady> {
        self.deferreds
            .get(key)
            .duped()
            .with_internal_error(|| format!("deferred data not found by key: {}", key))
    }

    fn get_materialized_artifact(&self, artifact: &Artifact) -> Option<&ProjectRelativePath> {
        self.materialized_artifacts
            .get(artifact)
            .map(|x| x.as_ref())
    }

    fn registry(&mut self) -> &mut DeferredRegistry {
        self.registry
    }

    fn project_filesystem(&self) -> &ProjectRoot {
        &self.project_filesystem
    }

    fn digest_config(&self) -> DigestConfig {
        self.digest_config
    }

    fn liveness(&self) -> CancellationObserver {
        self.liveness.dupe()
    }
}

/// input to a deferred that needs to be computed first before executing
#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative)]
pub enum DeferredInput {
    ConfiguredTarget(ConfiguredTargetLabel),
    Deferred(DeferredKey),
    /// Materialized artifact is an input of `dynamic_output`.
    /// Regular actions like `run` do not have `MaterializedArtifact` inputs.
    MaterializedArtifact(Artifact),
}

#[derive(Copy, Clone, Dupe)]
pub enum DeferredInputsRef<'a> {
    IndexSet(&'a IndexSet<DeferredInput>),
    Slice(&'a [DeferredInput]),
}

impl<'a> IntoIterator for DeferredInputsRef<'a> {
    type Item = &'a DeferredInput;
    type IntoIter = Either<indexmap::set::Iter<'a, DeferredInput>, slice::Iter<'a, DeferredInput>>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            DeferredInputsRef::IndexSet(set) => Either::Left(set.iter()),
            DeferredInputsRef::Slice(slice) => Either::Right(slice.iter()),
        }
    }
}

/// Implemented by all trivial deferreds.
pub trait TrivialDeferred: Allocative + AnyValue + Debug + Send + Sync {
    /// Convert the object to an AnyValue object
    fn as_any_value(&self) -> &dyn AnyValue;

    /// Obtain deferred-specific debug data.
    ///
    /// This function is copied from `any::Provider` trait, which cannot be implemented
    /// for `Arc<RegisteredAction>`.
    fn provide<'a>(&'a self, demand: &mut provider::Demand<'a>);
}

#[derive(Allocative, Debug)]
#[repr(transparent)]
pub struct TrivialDeferredValue<T: TrivialDeferred>(pub T);

impl<T: TrivialDeferred> provider::Provider for TrivialDeferredValue<T> {
    fn provide<'a>(&'a self, demand: &mut provider::Demand<'a>) {
        self.0.provide(demand)
    }
}

#[async_trait]
impl<T: TrivialDeferred> DeferredAny for TrivialDeferredValue<T> {
    fn inputs(&self) -> DeferredInputsRef<'_> {
        DeferredInputsRef::Slice(&[])
    }

    async fn execute(
        &self,
        _ctx: &mut dyn DeferredCtx,
        _dice: &mut DiceComputations,
    ) -> anyhow::Result<Result<DeferredValueAny, IsTriviallyDeferred>> {
        Ok(Err(IsTriviallyDeferred))
    }

    fn as_any(&self) -> &dyn Any {
        self.0.into_any()
    }

    fn type_name(&self) -> &str {
        self.0.type_name()
    }

    fn span(&self) -> Option<buck2_data::span_start_event::Data> {
        // No evaluation Span if you never evaluate.
        None
    }
}

#[derive(Allocative)]
pub struct DeferredTableEntry {
    any: Arc<dyn DeferredAny>,
}

impl provider::Provider for DeferredTableEntry {
    fn provide<'a>(&'a self, demand: &mut provider::Demand<'a>) {
        self.any.provide(demand)
    }
}

#[derive(Allocative)]
enum DeferredRegistryEntry {
    Set(DeferredTableEntry),
    Pending,
}

/// The registry for creating 'DeferredData's and registering 'Deferred's
#[derive(Allocative)]
pub struct DeferredRegistry {
    base_key: DeferredHolderKey,
    registry: Vec<DeferredRegistryEntry>,
    recorded_values: Option<RecordedAnalysisValues>,
}

#[derive(Allocative)]
#[allocative(bound = "")]
pub struct ReservedDeferredData<T>(DeferredData<T>);

impl<'a, T> ReservedDeferredData<T>
where
    T: Clone + Send + Sync + 'static,
{
    fn new(id: DeferredData<T>) -> Self {
        Self(id)
    }

    pub fn data(&'a self) -> &'a DeferredData<T> {
        &self.0
    }
}

#[derive(Allocative)]
#[allocative(bound = "")]
pub struct ReservedTrivialDeferredData<T>(DeferredData<T>);

impl<'a, T> ReservedTrivialDeferredData<T>
where
    T: Send + Sync + 'static,
{
    fn new(id: DeferredData<T>) -> Self {
        Self(id)
    }

    pub fn data(&'a self) -> &'a DeferredData<T> {
        &self.0
    }
}

impl DeferredRegistry {
    pub fn new(base_key: DeferredHolderKey) -> Self {
        Self {
            base_key,
            registry: Vec::new(),
            recorded_values: None,
        }
    }

    pub fn key(&self) -> &DeferredHolderKey {
        &self.base_key
    }

    /// Reserves a 'DeferredData', with it's underlying key, on the promise that it should be bound
    /// to a 'Deferred' before 'take_result' is called.
    pub fn reserve<D>(&mut self) -> ReservedDeferredData<D>
    where
        D: Clone + Send + Sync + 'static,
    {
        let id = DeferredId {
            id: self.registry.len().try_into().unwrap(),
            trivial: false,
        };
        self.registry.push(DeferredRegistryEntry::Pending);
        ReservedDeferredData::new(DeferredData::unchecked_new(self.base_key.make_key(id)))
    }

    /// Reserves a 'DeferredData', with it's underlying key, on the promise that it should be bound
    /// to a 'Deferred' before 'take_result' is called.
    pub fn reserve_trivial<D>(&mut self) -> ReservedTrivialDeferredData<D>
    where
        D: Send + Sync + 'static,
    {
        let id = DeferredId {
            id: self.registry.len().try_into().unwrap(),
            trivial: true,
        };
        self.registry.push(DeferredRegistryEntry::Pending);
        ReservedTrivialDeferredData::new(DeferredData::unchecked_new(self.base_key.make_key(id)))
    }

    /// binds a reserved 'ReservedDeferredData' to a 'Deferred'
    pub fn bind<D, T>(&mut self, reserved: ReservedDeferredData<T>, d: D) -> DeferredData<T>
    where
        D: Deferred<Output = T> + Send + Sync + 'static,
        T: DeferredOutput,
    {
        let id = reserved.0.deferred_key().id().as_usize();

        match self.registry.get_mut(id) {
            Some(entry @ DeferredRegistryEntry::Pending) => {
                *entry = DeferredRegistryEntry::Set(DeferredTableEntry { any: Arc::new(d) });
            }
            _ => {
                panic!("the reserved should always be in pending");
            }
        }

        reserved.0
    }

    /// binds a reserved 'ReservedDeferredData' to a 'Deferred'
    pub fn bind_trivial<D>(
        &mut self,
        reserved: ReservedTrivialDeferredData<D>,
        d: D,
    ) -> DeferredData<D>
    where
        D: TrivialDeferred + 'static,
    {
        let id = reserved.0.deferred_key().id().as_usize();

        match self.registry.get_mut(id) {
            Some(entry @ DeferredRegistryEntry::Pending) => {
                *entry = DeferredRegistryEntry::Set(DeferredTableEntry {
                    any: Arc::new(TrivialDeferredValue(d)),
                });
            }
            _ => {
                panic!("the reserved should always be in pending");
            }
        }

        reserved.0
    }

    /// creates a new 'DeferredData'
    pub fn defer<D: Deferred<Output = T> + Send + Sync + 'static, T: DeferredOutput>(
        &mut self,
        d: D,
    ) -> DeferredData<T> {
        let id = DeferredId {
            id: self.registry.len().try_into().unwrap(),
            trivial: false,
        };
        self.registry
            .push(DeferredRegistryEntry::Set(DeferredTableEntry {
                any: Arc::new(d),
            }));
        DeferredData::unchecked_new(self.base_key.make_key(id))
    }

    /// creates a new 'DeferredData'
    pub fn defer_trivial<D>(&mut self, d: D) -> DeferredData<D>
    where
        D: TrivialDeferred + Clone + 'static,
    {
        let id = DeferredId {
            id: self.registry.len().try_into().unwrap(),
            trivial: true,
        };
        self.registry
            .push(DeferredRegistryEntry::Set(DeferredTableEntry {
                any: Arc::new(TrivialDeferredValue(d)),
            }));
        DeferredData::unchecked_new(self.base_key.make_key(id))
    }

    pub fn take_result(self) -> anyhow::Result<(DeferredTable, RecordedAnalysisValues)> {
        let table = DeferredTable::new(
            self.registry
                .into_iter()
                .enumerate()
                .map(|(i, e)| match e {
                    DeferredRegistryEntry::Set(e) => anyhow::Ok(e),
                    DeferredRegistryEntry::Pending => {
                        Err(DeferredErrors::UnboundReservedDeferred(i).into())
                    }
                })
                .collect::<anyhow::Result<_>>()?,
        );

        let values = self
            .recorded_values
            // TODO(cjhopman): We should require this to be set, but a bunch of non-analysis things still use deferreds.
            .unwrap_or_else(RecordedAnalysisValues::new_empty);

        Ok((table, values))
    }

    pub(crate) fn register_values(&mut self, values: RecordedAnalysisValues) -> anyhow::Result<()> {
        match self.recorded_values.replace(values) {
            // TODO(cjhopman): delete this error in this stack
            Some(_) => Err(internal_error!("recorded analysis values already set")),
            None => Ok(()),
        }
    }
}

#[derive(Debug, buck2_error::Error)]
pub enum DeferredErrors {
    #[error("no deferred found for deferred id `{0}`")]
    DeferredNotFound(u32),
    #[error("reserved deferred id of `{0:?}` was never bound")]
    UnboundReservedDeferred(usize),
}

pub struct DeferredLookup<'a> {
    pub(crate) any: ArcBorrow<'a, dyn DeferredAny>,
}

impl<'a> DeferredLookup<'a> {
    pub fn as_complex(&self) -> &'a dyn DeferredAny {
        ArcBorrow::get(self.any)
    }
}

/// Contains all the deferreds generated by analyzing a particular rule implementation
#[derive(Clone, Debug, Dupe, Allocative)]
pub struct DeferredResult(Arc<DeferredResultData>);

#[derive(Allocative)]
struct DeferredResultData {
    deferreds: DeferredTable,
    analysis_values: RecordedAnalysisValues,
    value: DeferredValueAny,
}

/// All direct deferreds for some `DeferredHolderKey`.
#[derive(Allocative)]
pub struct DeferredTable(
    /// Indexed by `DeferredId`.
    Box<[DeferredTableEntry]>,
);

impl Debug for DeferredResultData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "DeferredResult(value=`{:?}`, {} deferreds)",
            self.value,
            self.deferreds.len(),
        )
    }
}

impl Debug for DeferredTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "DeferredTable({} deferreds)", self.0.len())
    }
}

impl DeferredTable {
    pub fn new(deferreds: Vec<DeferredTableEntry>) -> Self {
        DeferredTable(deferreds.into_boxed_slice())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// looks up an 'Deferred' given the id
    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<DeferredLookup<'_>> {
        match self.0.get(id.as_usize()) {
            Some(value) => Ok(DeferredLookup {
                any: ArcBorrow::borrow(&value.any),
            }),
            None => Err(anyhow::anyhow!(DeferredErrors::DeferredNotFound(id.id))),
        }
    }

    /// Iterator on the DeferredTable which converts a `DeferredTableEntry` to a `DeferredLookup`
    pub fn iter(&self) -> impl Iterator<Item = DeferredLookup<'_>> {
        self.0.iter().map(|deferred| DeferredLookup {
            any: ArcBorrow::borrow(&deferred.any),
        })
    }
}

impl DeferredResult {
    pub fn new(
        value: DeferredValueAny,
        deferreds: DeferredTable,
        analysis_values: RecordedAnalysisValues,
    ) -> Self {
        Self(Arc::new(DeferredResultData {
            deferreds,
            value,
            analysis_values,
        }))
    }

    /// looks up an 'Deferred' given the id
    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<DeferredLookup<'_>> {
        self.0.deferreds.lookup_deferred(id)
    }

    pub fn value(&self) -> &DeferredValueAny {
        &self.0.value
    }

    pub(crate) fn analysis_values(&self) -> &crate::analysis::registry::RecordedAnalysisValues {
        &self.0.analysis_values
    }

    pub fn try_into_parts(self) -> Result<(DeferredTable, RecordedAnalysisValues), Self> {
        match Arc::try_unwrap(self.0) {
            Ok(data) => Ok((data.deferreds, data.analysis_values)),
            Err(e) => Err(Self(e)),
        }
    }
}

/// typed value computed by a deferred. This can either be a completed calculation, in which case
/// a value is returned, or defer to another 'Deferred' computation.
#[derive(Allocative)]
pub enum DeferredValue<T> {
    Ready(T),
    Deferred(DeferredData<T>),
}

/// Enum of AnyValue or TrivialDeferreds.
#[derive(Allocative, Debug, Dupe, Clone)]
pub enum DeferredValueAnyReady {
    AnyValue(Arc<dyn DeferredOutput>),
    TrivialDeferred(
        // This must be `TriviallyDeferred`.
        Arc<dyn DeferredAny>,
    ),
}

impl DeferredValueAnyReady {
    pub fn downcast<T: Send + 'static>(&self) -> anyhow::Result<&T> {
        match self {
            Self::AnyValue(v) => v.downcast(),
            Self::TrivialDeferred(v) => v.downcast(),
        }
    }

    pub fn downcast_into_arc<T: Send + 'static>(self) -> anyhow::Result<Arc<T>> {
        match self {
            DeferredValueAnyReady::AnyValue(any) => any.downcast_arc(),
            DeferredValueAnyReady::TrivialDeferred(any) => any.downcast_arc(),
        }
    }

    pub fn downcast_into<T: Send + 'static>(self) -> anyhow::Result<DeferredValueReady<T>> {
        // Check if it can downcast to T
        self.downcast::<T>()?;

        Ok(DeferredValueReady {
            inner: self,
            _type: PhantomData,
        })
    }

    pub fn resolve<T: Send + 'static>(
        self,
        _data: &DeferredData<T>,
    ) -> anyhow::Result<DeferredValueReady<T>> {
        self.downcast_into()
    }
}

/// This is a `Any` that has been checked to contain a T and can therefore provide &T infallibly
#[derive(Allocative, Debug, Dupe_, Clone_)]
pub struct DeferredValueReady<T> {
    inner: DeferredValueAnyReady,
    _type: PhantomData<T>,
}

impl<T> std::ops::Deref for DeferredValueReady<T>
where
    T: Send + 'static,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // This was checked earlier
        self.inner.downcast::<T>().unwrap()
    }
}

/// untyped value computed by the deferred. This is same as 'DeferredValue', but with 'T' as an
/// 'ValueAny'
#[derive(Debug, VariantName, Clone, Dupe, Allocative)]
pub enum DeferredValueAny {
    Ready(DeferredValueAnyReady),
    Deferred(DeferredKey),
}

impl DeferredValueAny {
    fn ready<T: DeferredOutput>(t: T) -> Self {
        Self::Ready(DeferredValueAnyReady::AnyValue(Arc::new(t)))
    }

    fn defer<T>(k: DeferredData<T>) -> Self {
        Self::Deferred(k.into_deferred_key())
    }
}

/// An 'Any' that is the return type of a 'Deferred'. This is box cloneable, and castable.
pub trait AnyValue: Allocative + Any + Debug + Send + Sync + 'static {
    fn into_any(&self) -> &(dyn Any);

    fn type_name(&self) -> &str;
}

impl<T> AnyValue for T
where
    T: Allocative + Any + Debug + Send + Sync,
{
    fn into_any(&self) -> &dyn Any {
        self
    }

    fn type_name(&self) -> &str {
        type_name::<T>()
    }
}

/// Marker trait for output of deferred computations.
///
/// This trait has no semantic significance.
/// It's only used to help keep track of what types are used as a deferred output.
pub trait DeferredOutput: AnyValue {}

impl dyn DeferredOutput {
    pub(crate) fn downcast<T: Send + 'static>(&self) -> anyhow::Result<&T> {
        match self.into_any().downcast_ref::<T>() {
            Some(t) => Ok(t),
            None => Err(anyhow::anyhow!(
                "Cannot cast DeferredOutput of value type `{}` into type `{}`",
                self.type_name(),
                type_name::<T>()
            )),
        }
    }

    pub(crate) fn downcast_arc<T: Send + 'static>(
        self: Arc<dyn DeferredOutput>,
    ) -> anyhow::Result<Arc<T>> {
        self.downcast::<T>()?;
        // SAFETY: just checked type.
        Ok(unsafe { Arc::from_raw(Arc::into_raw(self) as *const T) })
    }
}

pub struct IsTriviallyDeferred;

/// untyped deferred
#[async_trait]
pub trait DeferredAny: Debug + Allocative + provider::Provider + Send + Sync + 'static {
    /// the set of 'Deferred's that should be computed first before executing this 'Deferred'
    fn inputs(&self) -> DeferredInputsRef<'_>;

    /// executes this 'Deferred', assuming all inputs and input artifacts are already computed
    async fn execute(
        &self,
        ctx: &mut dyn DeferredCtx,
        dice: &mut DiceComputations,
    ) -> anyhow::Result<Result<DeferredValueAny, IsTriviallyDeferred>>;

    fn as_any(&self) -> &dyn Any;

    fn type_name(&self) -> &str;

    /// An optional stage to wrap execution in.
    fn span(&self) -> Option<buck2_data::span_start_event::Data>;
}

pub(crate) async fn deferred_execute(
    deferred: ArcBorrow<'_, dyn DeferredAny>,
    ctx: &mut dyn DeferredCtx,
    dice: &mut DiceComputations<'_>,
) -> anyhow::Result<DeferredValueAny> {
    match deferred.execute(ctx, dice).await? {
        Ok(any) => Ok(any),
        Err(IsTriviallyDeferred) => Ok(DeferredValueAny::Ready(
            DeferredValueAnyReady::TrivialDeferred(ArcBorrow::clone_arc(deferred)),
        )),
    }
}

impl dyn DeferredAny {
    pub fn downcast<T: Send + 'static>(&self) -> anyhow::Result<&T> {
        match self.as_any().downcast_ref::<T>() {
            Some(t) => Ok(t),
            None => Err(anyhow::anyhow!(
                "Cannot cast Deferred of value type `{}` into type `{}`",
                self.type_name(),
                type_name::<T>()
            )),
        }
    }

    pub(crate) fn downcast_arc<T: Send + 'static>(
        self: Arc<dyn DeferredAny>,
    ) -> anyhow::Result<Arc<T>> {
        self.downcast::<T>()?;
        // SAFETY: just checked type.
        Ok(unsafe { Arc::from_raw(Arc::into_raw(self) as *const T) })
    }
}

#[async_trait]
impl<D, T> DeferredAny for D
where
    D: Deferred<Output = T> + Send + Sync + Any + 'static,
    T: DeferredOutput,
{
    fn inputs(&self) -> DeferredInputsRef<'_> {
        self.inputs()
    }

    async fn execute(
        &self,
        ctx: &mut dyn DeferredCtx,
        dice: &mut DiceComputations,
    ) -> anyhow::Result<Result<DeferredValueAny, IsTriviallyDeferred>> {
        match self.execute(ctx, dice).await? {
            DeferredValue::Ready(t) => Ok(Ok(DeferredValueAny::ready(t))),
            DeferredValue::Deferred(d) => Ok(Ok(DeferredValueAny::defer(d))),
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn type_name(&self) -> &str {
        type_name::<D>()
    }

    fn span(&self) -> Option<buck2_data::span_start_event::Data> {
        D::span(self)
    }
}

pub mod testing {

    use buck2_artifact::deferred::key::DeferredKey;
    use gazebo::variants::VariantName;

    use crate::deferred::types::AnyValue;
    use crate::deferred::types::DeferredResult;
    use crate::deferred::types::DeferredTable;
    use crate::deferred::types::DeferredValueAny;
    use crate::deferred::types::DeferredValueAnyReady;

    pub trait DeferredValueAnyExt {
        fn assert_ready(self) -> DeferredValueAnyReady;
        fn assert_deferred(self) -> DeferredKey;
    }

    impl DeferredValueAnyExt for DeferredValueAny {
        fn assert_ready(self) -> DeferredValueAnyReady {
            match self {
                DeferredValueAny::Ready(v) => v,
                x => panic!("Expected deferred to be Ready but was {}", x.variant_name()),
            }
        }

        fn assert_deferred(self) -> DeferredKey {
            match self {
                DeferredValueAny::Deferred(k) => k,
                x => panic!(
                    "Expected deferred to be Deferred but was {}",
                    x.variant_name()
                ),
            }
        }
    }

    pub trait AnyValueExt {
        /// tests if the any is equal to 't'
        fn eq<T: PartialEq + 'static>(&self, t: &T) -> bool;
    }

    impl AnyValueExt for dyn AnyValue {
        fn eq<T: PartialEq + 'static>(&self, t: &T) -> bool {
            self.into_any()
                .downcast_ref::<T>()
                .map_or(false, |x| x == t)
        }
    }

    pub trait DeferredAnalysisResultExt {
        fn get_registered(&self) -> &DeferredTable;
    }

    impl DeferredAnalysisResultExt for DeferredResult {
        fn get_registered(&self) -> &DeferredTable {
            &self.0.deferreds
        }
    }

    impl DeferredAnalysisResultExt for DeferredTable {
        fn get_registered(&self) -> &DeferredTable {
            self
        }
    }
}

#[cfg(test)]
mod tests {

    use std::fmt;
    use std::fmt::Debug;
    use std::fmt::Formatter;
    use std::sync::Arc;

    use allocative::Allocative;
    use buck2_artifact::deferred::id::DeferredId;
    use buck2_artifact::deferred::key::DeferredHolderKey;
    use buck2_artifact::deferred::key::DeferredKey;
    use buck2_core::base_deferred_key::BaseDeferredKey;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
    use buck2_execute::digest_config::DigestConfig;
    use dice::CancellationContext;
    use dice::DetectCycles;
    use dice::Dice;
    use dice::DiceComputations;
    use dice::DiceTransaction;
    use dupe::Dupe;
    use indexmap::IndexSet;

    use super::deferred_execute;
    use super::AnyValue;
    use super::DeferredInputsRef;
    use super::DeferredOutput;
    use super::TrivialDeferred;
    use crate::deferred::arc_borrow::ArcBorrow;
    use crate::deferred::types::testing::DeferredValueAnyExt;
    use crate::deferred::types::Deferred;
    use crate::deferred::types::DeferredCtx;
    use crate::deferred::types::DeferredInput;
    use crate::deferred::types::DeferredRegistry;
    use crate::deferred::types::DeferredValue;
    use crate::deferred::types::DeferredValueAny;
    use crate::deferred::types::DeferredValueAnyReady;
    use crate::deferred::types::ResolveDeferredCtx;

    #[derive(Clone, PartialEq, Eq, Allocative)]
    #[allocative(bound = "")]
    struct FakeDeferred<T> {
        inputs: IndexSet<DeferredInput>,
        #[allocative(skip)]
        val: T,
    }

    impl<T> Debug for FakeDeferred<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "FakeDeferred")
        }
    }

    impl<T: Clone> provider::Provider for FakeDeferred<T> {
        fn provide<'a>(&'a self, _demand: &mut provider::Demand<'a>) {}
    }

    impl<T: DeferredOutput + Clone> Deferred for FakeDeferred<T> {
        type Output = T;

        fn inputs(&self) -> DeferredInputsRef<'_> {
            DeferredInputsRef::IndexSet(&self.inputs)
        }

        async fn execute(
            &self,
            _ctx: &mut dyn DeferredCtx,
            _dice: &mut DiceComputations<'_>,
        ) -> anyhow::Result<DeferredValue<T>> {
            Ok(DeferredValue::Ready(self.val.clone()))
        }
    }

    #[derive(Allocative, Clone, Debug, Eq, PartialEq)]
    struct IntOutput(i32);

    impl DeferredOutput for IntOutput {}

    impl TrivialDeferred for FakeDeferred<IntOutput> {
        fn as_any_value(&self) -> &dyn AnyValue {
            self
        }

        fn provide<'a>(&'a self, _demand: &mut provider::Demand<'a>) {}
    }

    #[derive(Clone, Debug, PartialEq, Eq, Allocative)]
    #[allocative(bound = "")]
    struct TestDeferringDeferred<T> {
        inputs: IndexSet<DeferredInput>,
        #[allocative(skip)]
        defer: FakeDeferred<T>,
    }

    impl<T: Clone + Debug + Allocative + Send + Sync + 'static> provider::Provider
        for TestDeferringDeferred<T>
    {
        fn provide<'a>(&'a self, _demand: &mut provider::Demand<'a>) {}
    }

    impl<T: DeferredOutput + Clone> Deferred for TestDeferringDeferred<T> {
        type Output = T;

        fn inputs(&self) -> DeferredInputsRef<'_> {
            DeferredInputsRef::IndexSet(&self.inputs)
        }

        async fn execute(
            &self,
            ctx: &mut dyn DeferredCtx,
            _dice: &mut DiceComputations<'_>,
        ) -> anyhow::Result<DeferredValue<T>> {
            Ok(DeferredValue::Deferred(
                ctx.registry().defer(self.defer.clone()),
            ))
        }
    }

    fn dummy_base() -> BaseDeferredKey {
        BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
            "cell//pkg:foo",
            ConfigurationData::testing_new(),
        ))
    }

    fn dummy_project_filesystem() -> ProjectRoot {
        let cwd = if cfg!(windows) {
            AbsNormPath::new("c:/tmp").unwrap().to_owned()
        } else {
            AbsNormPath::new("/dev/null").unwrap().to_owned()
        };
        ProjectRoot::new_unchecked(cwd)
    }

    async fn dummy_dice_transaction() -> anyhow::Result<DiceTransaction> {
        let dice = Dice::modern().build(DetectCycles::Enabled);
        let res = dice.updater().commit().await;
        Ok(res)
    }

    #[tokio::test]
    async fn register_deferred() -> anyhow::Result<()> {
        let target = dummy_base();
        let mut registry = DeferredRegistry::new(DeferredHolderKey::Base(target.dupe()));

        let deferred = FakeDeferred {
            inputs: IndexSet::new(),
            val: IntOutput(2),
        };

        let deferred_data = registry.defer(deferred);

        let (result, _) = registry.take_result()?;

        let mut ctx = DeferredRegistry::new(DeferredHolderKey::Deferred(Arc::new(
            deferred_data.deferred_key().dupe(),
        )));

        let mut dummy_dice_transaction = dummy_dice_transaction().await?;

        CancellationContext::testing()
            .with_structured_cancellation(|observer| async move {
                assert_eq!(
                    *deferred_execute(
                        result
                            .lookup_deferred(deferred_data.deferred_key().id())
                            .unwrap()
                            .any,
                        &mut ResolveDeferredCtx::new(
                            deferred_data.deferred_key().action_key(),
                            Default::default(),
                            Default::default(),
                            Default::default(),
                            &mut ctx,
                            dummy_project_filesystem(),
                            DigestConfig::testing_default(),
                            observer
                        ),
                        &mut dummy_dice_transaction
                    )
                    .await
                    .unwrap()
                    .assert_ready()
                    .resolve(&deferred_data)?,
                    IntOutput(2)
                );
                Ok(())
            })
            .await
    }

    #[tokio::test]
    async fn register_nested_deferred() -> anyhow::Result<()> {
        let target =
            ConfiguredTargetLabel::testing_parse("cell//pkg:foo", ConfigurationData::testing_new());

        let id = DeferredId {
            id: 1,
            trivial: false,
        };

        let base = DeferredKey::Base(BaseDeferredKey::TargetLabel(target.dupe()), id);
        let mut registry = DeferredRegistry::new(DeferredHolderKey::Deferred(Arc::new(base)));

        let deferred = FakeDeferred {
            inputs: IndexSet::new(),
            val: IntOutput(2),
        };

        let deferring_deferred = TestDeferringDeferred {
            inputs: Default::default(),
            defer: deferred,
        };

        let deferring_deferred_data = registry.defer(deferring_deferred);
        let (result, _) = registry.take_result()?;

        let mut registry = DeferredRegistry::new(DeferredHolderKey::Deferred(Arc::new(
            deferring_deferred_data.deferred_key().dupe(),
        )));

        let mut dummy_dice_transaction = dummy_dice_transaction().await?;

        CancellationContext::testing()
            .with_structured_cancellation(|observer| async move {
                let exec_result = deferred_execute(
                    result
                        .lookup_deferred(deferring_deferred_data.deferred_key().id())
                        .unwrap()
                        .any,
                    &mut ResolveDeferredCtx::new(
                        deferring_deferred_data.deferred_key().action_key(),
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        &mut registry,
                        dummy_project_filesystem(),
                        DigestConfig::testing_default(),
                        observer.dupe(),
                    ),
                    &mut dummy_dice_transaction,
                )
                .await
                .unwrap();

                let deferred_key = match exec_result {
                    DeferredValueAny::Ready(_) => panic!("expected a deferred"),
                    DeferredValueAny::Deferred(deferred) => deferred,
                };

                assert_eq!(
                    deferred_key,
                    DeferredKey::Deferred(
                        Arc::new(deferring_deferred_data.deferred_key().dupe()),
                        DeferredId {
                            id: 0,
                            trivial: false
                        }
                    )
                );

                let (result, _) = registry.take_result()?;
                let deferred = result.lookup_deferred(deferred_key.id()).unwrap();

                let mut registry = DeferredRegistry::new(DeferredHolderKey::Deferred(Arc::new(
                    deferred_key.dupe(),
                )));
                assert_eq!(
                    deferred_execute(
                        deferred.any,
                        &mut ResolveDeferredCtx::new(
                            deferred_key.action_key(),
                            Default::default(),
                            Default::default(),
                            Default::default(),
                            &mut registry,
                            dummy_project_filesystem(),
                            DigestConfig::testing_default(),
                            observer,
                        ),
                        &mut dummy_dice_transaction
                    )
                    .await
                    .unwrap()
                    .assert_ready()
                    .downcast::<IntOutput>()?
                    .0,
                    2
                );

                Ok(())
            })
            .await
    }

    #[tokio::test]
    async fn reserving_deferred() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
            "cell//pkg:foo",
            ConfigurationData::testing_new(),
        ));
        let mut registry = DeferredRegistry::new(DeferredHolderKey::Base(base));

        let deferred = FakeDeferred {
            inputs: IndexSet::new(),
            val: IntOutput(2),
        };

        let reserved = registry.reserve();
        let reserved_deferred_data = reserved.data().dupe();

        let deferred_data = registry.bind(reserved, deferred);

        assert_eq!(deferred_data, reserved_deferred_data);

        let (result, _) = registry.take_result()?;

        let mut registry = DeferredRegistry::new(DeferredHolderKey::Deferred(Arc::new(
            deferred_data.deferred_key().dupe(),
        )));

        let mut dummy_dice_transaction = dummy_dice_transaction().await?;

        let key = deferred_data.deferred_key().dupe();

        CancellationContext::testing()
            .with_structured_cancellation(|observer| async move {
                assert_eq!(
                    *deferred_execute(
                        result
                            .lookup_deferred(deferred_data.deferred_key().id())
                            .unwrap()
                            .any,
                        &mut ResolveDeferredCtx::new(
                            key.action_key(),
                            Default::default(),
                            Default::default(),
                            Default::default(),
                            &mut registry,
                            dummy_project_filesystem(),
                            DigestConfig::testing_default(),
                            observer,
                        ),
                        &mut dummy_dice_transaction,
                    )
                    .await
                    .unwrap()
                    .assert_ready()
                    .resolve(&deferred_data)?,
                    IntOutput(2)
                );

                Ok(())
            })
            .await
    }

    #[test]
    fn reserving_deferred_unbound() {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
            "cell//pkg:foo",
            ConfigurationData::testing_new(),
        ));
        let mut registry = DeferredRegistry::new(DeferredHolderKey::Base(base));

        let _reserved = registry.reserve::<FakeDeferred<()>>();
        let _reserved1 = registry.reserve::<FakeDeferred<()>>();

        assert!(registry.take_result().is_err());
    }

    #[test]
    fn trivial_deferred() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
            "cell//pkg:foo",
            ConfigurationData::testing_new(),
        ));
        let mut registry = DeferredRegistry::new(DeferredHolderKey::Base(base));

        let deferred0 = FakeDeferred {
            inputs: IndexSet::new(),
            val: IntOutput(123),
        };

        let deferred1 = FakeDeferred {
            inputs: IndexSet::new(),
            val: IntOutput(456),
        };
        let deferred_data0 = registry.defer_trivial(deferred0.clone());
        let deferred_data1 = registry.reserve_trivial();
        let deferred_data1 = registry.bind_trivial(deferred_data1, deferred1.clone());

        let (result, _) = registry.take_result()?;

        assert_eq!(
            *DeferredValueAnyReady::TrivialDeferred(ArcBorrow::clone_arc(
                result
                    .lookup_deferred(deferred_data0.deferred_key().id())?
                    .any
            ))
            .resolve(&deferred_data0)?,
            deferred0
        );

        assert_eq!(
            *DeferredValueAnyReady::TrivialDeferred(ArcBorrow::clone_arc(
                result
                    .lookup_deferred(deferred_data1.deferred_key().id())?
                    .any
            ))
            .resolve(&deferred_data1)?,
            deferred1
        );

        Ok(())
    }
}
