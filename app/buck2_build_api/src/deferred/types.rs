/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any;
use std::any::type_name;
use std::any::Any;
use std::any::Demand;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::marker::PhantomData;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::deferred::data::DeferredData;
use buck2_artifact::deferred::id::DeferredId;
use buck2_artifact::deferred::key::DeferredKey;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_execute::digest_config::DigestConfig;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dice::DiceComputations;
use dupe::Dupe;
use gazebo::variants::VariantName;
use indexmap::indexset;
use indexmap::IndexSet;
use itertools::Itertools;
use more_futures::cancellable_future::CancellationObserver;
use once_cell::sync::Lazy;
use thiserror::Error;

/// An asynchronous chunk of work that will be executed when requested.
/// The 'Deferred' can have "inputs" which are values that will be guaranteed to be ready to use
/// before the 'Deferred' is actually executed. These can be 'Artifact's, which means that those
/// 'Artifact's will be materialized and its corresponding 'Action's executed, or other
/// 'DeferredData', which means those 'Deferred' will be computed first.
///
/// `any::Provider` can be used to obtain data for introspection.
#[async_trait]
pub trait Deferred: Allocative + any::Provider {
    type Output;

    /// the set of 'Deferred's that should be computed first before executing this 'Deferred'
    fn inputs(&self) -> &IndexSet<DeferredInput>;

    /// executes this 'Deferred', assuming all inputs and input artifacts are already computed
    async fn execute(
        &self,
        ctx: &mut dyn DeferredCtx,
        dice: &DiceComputations,
    ) -> anyhow::Result<DeferredValue<Self::Output>>;

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

    fn get_deferred_data(&self, key: &DeferredKey) -> Option<DeferredValueAnyReady>;

    fn get_materialized_artifact(&self, artifact: &Artifact) -> Option<&ProjectRelativePath>;

    fn registry(&mut self) -> &mut DeferredRegistry;

    fn project_filesystem(&self) -> &ProjectRoot;

    fn digest_config(&self) -> DigestConfig;

    fn liveness(&self) -> CancellationObserver;
}

/// DeferredCtx with already resolved values
pub struct ResolveDeferredCtx<'a> {
    key: DeferredKey,
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
        key: DeferredKey,
        configured_targets: HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
        deferreds: HashMap<DeferredKey, DeferredValueAnyReady>,
        materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
        registry: &'a mut DeferredRegistry,
        project_filesystem: ProjectRoot,
        digest_config: DigestConfig,
        liveness: CancellationObserver,
    ) -> Self {
        Self {
            key,
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
        self.key.action_key()
    }

    fn get_deferred_data(&self, key: &DeferredKey) -> Option<DeferredValueAnyReady> {
        self.deferreds.get(key).map(|b| b.dupe())
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
    MaterializedArtifact(Artifact),
}

/// The base key. We can actually get rid of this and just use 'DeferredKey' if rule analysis is an
/// 'Deferred' itself. This is used to construct the composed 'DeferredKey::Deferred' or
/// 'DeferredKey::Base' type.
#[derive(Allocative)]
pub enum BaseKey {
    Base(BaseDeferredKey),
    // While DeferredKey is Dupe, it has quite a lot of Arc's inside it, so maybe an Arc here makes sense?
    // Maybe not?
    #[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))]
    Deferred(Arc<DeferredKey>),
}

impl BaseKey {
    fn make_key(&self, id: DeferredId) -> DeferredKey {
        match self {
            BaseKey::Base(base) => DeferredKey::Base(base.dupe(), id),
            BaseKey::Deferred(base) => DeferredKey::Deferred(base.dupe(), id),
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
    fn provide<'a>(&'a self, demand: &mut Demand<'a>);
}

#[derive(Allocative)]
pub struct TrivialDeferredValue(pub Arc<dyn TrivialDeferred>);

impl any::Provider for TrivialDeferredValue {
    fn provide<'a>(&'a self, demand: &mut Demand<'a>) {
        self.0.provide(demand)
    }
}

#[async_trait]
impl DeferredAny for TrivialDeferredValue {
    fn inputs(&self) -> &IndexSet<DeferredInput> {
        static INPUTS: Lazy<IndexSet<DeferredInput>> = Lazy::new(IndexSet::new);
        &INPUTS
    }

    async fn execute(
        &self,
        _ctx: &mut dyn DeferredCtx,
        _dice: &DiceComputations,
    ) -> anyhow::Result<DeferredValueAny> {
        Ok(DeferredValueAny::Ready(
            DeferredValueAnyReady::TrivialDeferred(self.0.dupe()),
        ))
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
pub enum DeferredTableEntry {
    Trivial(TrivialDeferredValue),
    Complex(Box<dyn DeferredAny>),
}

impl any::Provider for DeferredTableEntry {
    fn provide<'a>(&'a self, demand: &mut Demand<'a>) {
        match self {
            Self::Trivial(v) => v.provide(demand),
            Self::Complex(v) => v.provide(demand),
        }
    }
}

#[async_trait]
impl DeferredAny for DeferredTableEntry {
    fn inputs(&self) -> &IndexSet<DeferredInput> {
        match self {
            Self::Trivial(v) => v.inputs(),
            Self::Complex(v) => v.inputs(),
        }
    }

    async fn execute(
        &self,
        ctx: &mut dyn DeferredCtx,
        dice: &DiceComputations,
    ) -> anyhow::Result<DeferredValueAny> {
        match self {
            Self::Trivial(v) => v.execute(ctx, dice).await,
            Self::Complex(v) => v.execute(ctx, dice).await,
        }
    }

    fn as_any(&self) -> &dyn Any {
        match self {
            Self::Trivial(v) => v.as_any(),
            Self::Complex(v) => v.as_any(),
        }
    }

    fn type_name(&self) -> &str {
        match self {
            Self::Trivial(v) => v.type_name(),
            Self::Complex(v) => v.type_name(),
        }
    }

    fn span(&self) -> Option<buck2_data::span_start_event::Data> {
        match self {
            Self::Trivial(..) => None,
            Self::Complex(v) => v.span(),
        }
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
    base_key: BaseKey,
    registry: Vec<DeferredRegistryEntry>,
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
    T: Clone + Send + Sync + 'static,
{
    fn new(id: DeferredData<T>) -> Self {
        Self(id)
    }

    pub fn data(&'a self) -> &'a DeferredData<T> {
        &self.0
    }
}

impl DeferredRegistry {
    pub fn new(base_key: BaseKey) -> Self {
        Self {
            base_key,
            registry: Vec::new(),
        }
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
        D: Clone + Send + Sync + 'static,
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
        T: Allocative + Clone + Debug + Send + Sync + 'static,
    {
        let id = reserved.0.deferred_key().id().as_usize();

        match self.registry.get_mut(id) {
            Some(entry @ DeferredRegistryEntry::Pending) => {
                *entry = DeferredRegistryEntry::Set(DeferredTableEntry::Complex(Box::new(d)));
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
        D: TrivialDeferred + Clone + 'static,
    {
        let id = reserved.0.deferred_key().id().as_usize();

        match self.registry.get_mut(id) {
            Some(entry @ DeferredRegistryEntry::Pending) => {
                *entry = DeferredRegistryEntry::Set(DeferredTableEntry::Trivial(
                    TrivialDeferredValue(Arc::new(d) as _),
                ));
            }
            _ => {
                panic!("the reserved should always be in pending");
            }
        }

        reserved.0
    }

    /// creates a new 'DeferredData'
    pub fn defer<
        D: Deferred<Output = T> + Send + Sync + 'static,
        T: Allocative + Clone + Debug + Send + Sync + 'static,
    >(
        &mut self,
        d: D,
    ) -> DeferredData<T> {
        let id = DeferredId {
            id: self.registry.len().try_into().unwrap(),
            trivial: false,
        };
        self.registry
            .push(DeferredRegistryEntry::Set(DeferredTableEntry::Complex(
                Box::new(d),
            )));
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
            .push(DeferredRegistryEntry::Set(DeferredTableEntry::Trivial(
                TrivialDeferredValue(Arc::new(d) as _),
            )));
        DeferredData::unchecked_new(self.base_key.make_key(id))
    }

    /// performs a mapping function over an 'DeferredData'
    pub fn map<T, U, F>(&mut self, orig: &DeferredData<T>, f: F) -> DeferredData<U>
    where
        T: Allocative + Clone + Debug + Send + Sync + 'static,
        U: Allocative + Clone + Debug + Send + Sync + 'static,
        F: Fn(&T, &mut dyn DeferredCtx) -> DeferredValue<U> + Send + Sync + 'static,
    {
        #[derive(Allocative)]
        #[allocative(bound = "")]
        struct Map<T, U, F> {
            orig: IndexSet<DeferredInput>,
            #[allocative(skip)]
            f: F,
            p: PhantomData<(T, U)>,
        }

        impl<T, U, F> any::Provider for Map<T, U, F>
        where
            T: Allocative + Send + Sync + 'static,
            F: Fn(&T, &mut dyn DeferredCtx) -> DeferredValue<U> + Send + Sync + 'static,
            U: Allocative + 'static,
        {
            fn provide<'a>(&'a self, _demand: &mut Demand<'a>) {}
        }

        #[async_trait]
        impl<T, U, F> Deferred for Map<T, U, F>
        where
            T: Allocative + Send + Sync + 'static,
            F: Fn(&T, &mut dyn DeferredCtx) -> DeferredValue<U> + Send + Sync + 'static,
            U: Allocative + Send + Sync + 'static,
        {
            type Output = U;

            fn inputs(&self) -> &IndexSet<DeferredInput> {
                &self.orig
            }

            async fn execute(
                &self,
                ctx: &mut dyn DeferredCtx,
                _dice: &DiceComputations,
            ) -> anyhow::Result<DeferredValue<Self::Output>> {
                let orig = match self.orig.iter().exactly_one() {
                    Ok(DeferredInput::Deferred(orig)) => orig,
                    _ => unreachable!(),
                };
                Ok((self.f)(
                    ctx.get_deferred_data(orig)
                        .unwrap()
                        .downcast::<T>()
                        .unwrap(),
                    ctx,
                ))
            }
        }

        self.defer(Map {
            orig: indexset![DeferredInput::Deferred(orig.deferred_key().dupe())],
            f,
            p: PhantomData,
        })
    }

    pub fn take_result(self) -> anyhow::Result<Vec<DeferredTableEntry>> {
        self.registry
            .into_iter()
            .enumerate()
            .map(|(i, e)| match e {
                DeferredRegistryEntry::Set(e) => anyhow::Ok(e),
                DeferredRegistryEntry::Pending => {
                    Err(DeferredErrors::UnboundReservedDeferred(i).into())
                }
            })
            .collect()
    }
}

#[derive(Debug, Error)]
pub enum DeferredErrors {
    #[error("no deferred found for deferred id `{0}`")]
    DeferredNotFound(u32),
    #[error("reserved deferred id of `{0:?}` was never bound")]
    UnboundReservedDeferred(usize),
}

pub enum DeferredLookup<'a> {
    Trivial(&'a TrivialDeferredValue),
    Complex(&'a (dyn DeferredAny + 'static)),
}

impl<'a> DeferredLookup<'a> {
    pub fn as_complex(&self) -> &'a (dyn DeferredAny + 'static) {
        match self {
            Self::Trivial(v) => *v as _,
            Self::Complex(v) => *v,
        }
    }

    pub fn as_trivial(&self) -> Option<&'a Arc<dyn TrivialDeferred>> {
        match self {
            Self::Trivial(v) => Some(&v.0),
            Self::Complex(..) => None,
        }
    }
}

/// Contains all the deferreds generated by analyzing a particular rule implementation
#[derive(Clone, Debug, Dupe, Allocative)]
pub struct DeferredResult(Arc<DeferredResultData>);

#[derive(Allocative)]
struct DeferredResultData {
    deferreds: Vec<DeferredTableEntry>,
    value: DeferredValueAny,
}

#[derive(Clone, Dupe, Allocative)]
pub struct DeferredTable(Arc<Vec<DeferredTableEntry>>);

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
        Self(Arc::new(deferreds))
    }

    /// looks up an 'Deferred' given the id
    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<DeferredLookup<'_>> {
        match self.0.get(id.as_usize()) {
            Some(DeferredTableEntry::Complex(deferred)) => Ok(DeferredLookup::Complex(&**deferred)),
            Some(DeferredTableEntry::Trivial(value)) => Ok(DeferredLookup::Trivial(value)),
            None => Err(anyhow::anyhow!(DeferredErrors::DeferredNotFound(id.id))),
        }
    }

    /// Iterator on the DeferredTable which converts a `DeferredTableEntry` to a `DeferredLookup`
    pub fn iter(&self) -> impl Iterator<Item = DeferredLookup<'_>> {
        self.0.iter().map(|deferred| match deferred {
            DeferredTableEntry::Complex(deferred) => DeferredLookup::Complex(&**deferred),
            DeferredTableEntry::Trivial(value) => DeferredLookup::Trivial(value),
        })
    }
}

impl DeferredResult {
    pub fn new(value: DeferredValueAny, deferreds: Vec<DeferredTableEntry>) -> Self {
        Self(Arc::new(DeferredResultData { deferreds, value }))
    }

    /// looks up an 'Deferred' given the id
    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<DeferredLookup<'_>> {
        match self.0.deferreds.get(id.as_usize()) {
            Some(DeferredTableEntry::Complex(deferred)) => Ok(DeferredLookup::Complex(&**deferred)),
            Some(DeferredTableEntry::Trivial(value)) => Ok(DeferredLookup::Trivial(value)),
            None => Err(anyhow::anyhow!(DeferredErrors::DeferredNotFound(id.id))),
        }
    }

    pub fn value(&self) -> &DeferredValueAny {
        &self.0.value
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
    AnyValue(Arc<dyn AnyValue>),
    TrivialDeferred(Arc<dyn TrivialDeferred>),
}

impl DeferredValueAnyReady {
    pub fn downcast<T: Send + 'static>(&self) -> anyhow::Result<&T> {
        match self {
            Self::AnyValue(v) => v.downcast(),
            Self::TrivialDeferred(v) => v.as_any_value().downcast(),
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
#[derive(Allocative, Debug, Dupe, Clone)]
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
    fn ready<T: Allocative + Clone + Debug + Send + Sync + 'static>(t: T) -> Self {
        Self::Ready(DeferredValueAnyReady::AnyValue(Arc::new(t)))
    }

    fn defer<T>(k: DeferredData<T>) -> Self {
        Self::Deferred(k.into_deferred_key())
    }
}

/// An 'Any' that is the return type of a 'Deferred'. This is box cloneable, and castable.
pub trait AnyValue: Allocative + Any + Debug + Send + Sync {
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

impl dyn AnyValue {
    pub(crate) fn downcast<T: Send + 'static>(&self) -> anyhow::Result<&T> {
        match self.into_any().downcast_ref::<T>() {
            Some(t) => Ok(t),
            None => Err(anyhow::anyhow!(
                "Cannot cast Deferred of value type `{}` into type `{}`",
                self.type_name(),
                type_name::<T>()
            )),
        }
    }
}

/// untyped deferred
#[async_trait]
pub trait DeferredAny: Allocative + any::Provider + Send + Sync {
    /// the set of 'Deferred's that should be computed first before executing this 'Deferred'
    fn inputs(&self) -> &IndexSet<DeferredInput>;

    /// executes this 'Deferred', assuming all inputs and input artifacts are already computed
    async fn execute(
        &self,
        ctx: &mut dyn DeferredCtx,
        dice: &DiceComputations,
    ) -> anyhow::Result<DeferredValueAny>;

    fn as_any(&self) -> &dyn Any;

    fn type_name(&self) -> &str;

    /// An optional stage to wrap execution in.
    fn span(&self) -> Option<buck2_data::span_start_event::Data>;
}

impl dyn DeferredAny {
    pub fn downcast<T: Deferred + Send + 'static>(&self) -> anyhow::Result<&T> {
        match self.as_any().downcast_ref::<T>() {
            Some(t) => Ok(t),
            None => Err(anyhow::anyhow!(
                "Cannot cast Deferred of value type `{}` into type `{}`",
                self.type_name(),
                type_name::<T>()
            )),
        }
    }
}

#[async_trait]
impl<D, T> DeferredAny for D
where
    D: Deferred<Output = T> + Send + Sync + Any + 'static,
    T: Allocative + Clone + Debug + Send + Sync + 'static,
{
    fn inputs(&self) -> &IndexSet<DeferredInput> {
        self.inputs()
    }

    async fn execute(
        &self,
        ctx: &mut dyn DeferredCtx,
        dice: &DiceComputations,
    ) -> anyhow::Result<DeferredValueAny> {
        match self.execute(ctx, dice).await? {
            DeferredValue::Ready(t) => Ok(DeferredValueAny::ready(t)),
            DeferredValue::Deferred(d) => Ok(DeferredValueAny::defer(d)),
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
    use crate::deferred::types::DeferredTableEntry;
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
        fn get_registered(&self) -> &Vec<DeferredTableEntry>;
    }

    impl DeferredAnalysisResultExt for DeferredResult {
        fn get_registered(&self) -> &Vec<DeferredTableEntry> {
            &self.0.deferreds
        }
    }

    impl DeferredAnalysisResultExt for DeferredTable {
        fn get_registered(&self) -> &Vec<DeferredTableEntry> {
            &self.0
        }
    }
}

#[cfg(test)]
mod tests {
    use std::any;
    use std::any::Demand;
    use std::fmt;
    use std::fmt::Debug;
    use std::fmt::Formatter;
    use std::marker::Send;
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_artifact::deferred::data::DeferredData;
    use buck2_artifact::deferred::id::DeferredId;
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
    use indexmap::indexset;
    use indexmap::IndexSet;

    use super::AnyValue;
    use super::TrivialDeferred;
    use crate::deferred::types::testing::DeferredValueAnyExt;
    use crate::deferred::types::BaseKey;
    use crate::deferred::types::Deferred;
    use crate::deferred::types::DeferredAny;
    use crate::deferred::types::DeferredCtx;
    use crate::deferred::types::DeferredInput;
    use crate::deferred::types::DeferredRegistry;
    use crate::deferred::types::DeferredTable;
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

    impl<T: Clone> any::Provider for FakeDeferred<T> {
        fn provide<'a>(&'a self, _demand: &mut Demand<'a>) {}
    }

    #[async_trait]
    impl<T: Clone + Send + Sync> Deferred for FakeDeferred<T> {
        type Output = T;

        fn inputs(&self) -> &IndexSet<DeferredInput> {
            &self.inputs
        }

        async fn execute(
            &self,
            _ctx: &mut dyn DeferredCtx,
            _dice: &DiceComputations,
        ) -> anyhow::Result<DeferredValue<T>> {
            Ok(DeferredValue::Ready(self.val.clone()))
        }
    }

    impl TrivialDeferred for FakeDeferred<i32> {
        fn as_any_value(&self) -> &dyn AnyValue {
            self
        }

        fn provide<'a>(&'a self, _demand: &mut Demand<'a>) {}
    }

    #[derive(Clone, Debug, PartialEq, Eq, Allocative)]
    #[allocative(bound = "")]
    struct DeferringDeferred<T> {
        inputs: IndexSet<DeferredInput>,
        #[allocative(skip)]
        defer: FakeDeferred<T>,
    }

    impl<T: Clone + Debug + Allocative + Send + Sync + 'static> any::Provider for DeferringDeferred<T> {
        fn provide<'a>(&'a self, _demand: &mut Demand<'a>) {}
    }

    #[async_trait]
    impl<T: Clone + Debug + Allocative + Send + Sync + 'static> Deferred for DeferringDeferred<T> {
        type Output = T;

        fn inputs(&self) -> &IndexSet<DeferredInput> {
            &self.inputs
        }

        async fn execute(
            &self,
            ctx: &mut dyn DeferredCtx,
            _dice: &DiceComputations,
        ) -> anyhow::Result<DeferredValue<T>> {
            Ok(DeferredValue::Deferred(
                ctx.registry().defer(self.defer.clone()),
            ))
        }
    }

    fn make_resolved<T: Clone + Debug + Allocative + Send + Sync + 'static>(
        data: &DeferredData<T>,
        deferred: &FakeDeferred<T>,
    ) -> (DeferredKey, DeferredValueAnyReady) {
        (
            data.deferred_key().dupe(),
            DeferredValueAnyReady::AnyValue(Arc::new(deferred.val.clone())),
        )
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
        let mut registry = DeferredRegistry::new(BaseKey::Base(target.dupe()));

        let deferred = FakeDeferred {
            inputs: IndexSet::new(),
            val: 2,
        };

        let deferred_data = registry.defer(deferred);

        let result = registry.take_result()?;

        let mut ctx = DeferredRegistry::new(BaseKey::Deferred(Arc::new(
            deferred_data.deferred_key().dupe(),
        )));

        let dummy_dice_transaction = dummy_dice_transaction().await?;

        CancellationContext::testing()
            .with_structured_cancellation(|observer| async move {
                assert_eq!(
                    *result
                        .get(deferred_data.deferred_key().id().as_usize())
                        .unwrap()
                        .execute(
                            &mut ResolveDeferredCtx::new(
                                deferred_data.deferred_key().dupe(),
                                Default::default(),
                                Default::default(),
                                Default::default(),
                                &mut ctx,
                                dummy_project_filesystem(),
                                DigestConfig::testing_default(),
                                observer
                            ),
                            &dummy_dice_transaction
                        )
                        .await
                        .unwrap()
                        .assert_ready()
                        .resolve(&deferred_data)?,
                    2
                );
                Ok(())
            })
            .await
    }

    #[tokio::test]
    async fn mapping_async_data() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
            "cell//pkg:foo",
            ConfigurationData::testing_new(),
        ));
        let mut registry = DeferredRegistry::new(BaseKey::Base(base.dupe()));

        let deferred = FakeDeferred {
            inputs: IndexSet::new(),
            val: 0,
        };

        let deferred_data = registry.defer(deferred.clone());
        let mapped = registry.map(&deferred_data, |x, _ctx| DeferredValue::Ready(x + 1));

        let result = registry.take_result()?;
        let mapped_deferred = result.get(mapped.deferred_key().id().as_usize()).unwrap();

        assert_eq!(
            mapped_deferred.inputs(),
            &indexset![DeferredInput::Deferred(deferred_data.deferred_key().dupe())]
        );

        let mut registry = DeferredRegistry::new(BaseKey::Deferred(Arc::new(
            deferred_data.deferred_key().dupe(),
        )));

        let dummy_dice_transaction = dummy_dice_transaction().await?;

        CancellationContext::testing()
            .with_structured_cancellation(|observer| async move {
                let mut resolved = ResolveDeferredCtx::new(
                    deferred_data.deferred_key().dupe(),
                    Default::default(),
                    vec![make_resolved(&deferred_data, &deferred)]
                        .into_iter()
                        .collect(),
                    Default::default(),
                    &mut registry,
                    dummy_project_filesystem(),
                    DigestConfig::testing_default(),
                    observer,
                );

                assert_eq!(
                    *mapped_deferred
                        .execute(&mut resolved, &dummy_dice_transaction)
                        .await
                        .unwrap()
                        .assert_ready()
                        .downcast::<i32>()?,
                    1
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
        let mut registry = DeferredRegistry::new(BaseKey::Deferred(Arc::new(base)));

        let deferred = FakeDeferred {
            inputs: IndexSet::new(),
            val: 2,
        };

        let deferring_deferred = DeferringDeferred {
            inputs: Default::default(),
            defer: deferred,
        };

        let deferring_deferred_data = registry.defer(deferring_deferred);
        let result = registry.take_result()?;

        let mut registry = DeferredRegistry::new(BaseKey::Deferred(Arc::new(
            deferring_deferred_data.deferred_key().dupe(),
        )));

        let dummy_dice_transaction = dummy_dice_transaction().await?;

        CancellationContext::testing()
            .with_structured_cancellation(|observer| async move {
                let exec_result = result
                    .get(deferring_deferred_data.deferred_key().id().as_usize())
                    .unwrap()
                    .execute(
                        &mut ResolveDeferredCtx::new(
                            deferring_deferred_data.deferred_key().dupe(),
                            Default::default(),
                            Default::default(),
                            Default::default(),
                            &mut registry,
                            dummy_project_filesystem(),
                            DigestConfig::testing_default(),
                            observer.dupe(),
                        ),
                        &dummy_dice_transaction,
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

                let result = registry.take_result()?;
                let deferred = result.get(deferred_key.id().as_usize()).unwrap();

                let mut registry =
                    DeferredRegistry::new(BaseKey::Deferred(Arc::new(deferred_key.dupe())));
                assert_eq!(
                    *deferred
                        .execute(
                            &mut ResolveDeferredCtx::new(
                                deferred_key,
                                Default::default(),
                                Default::default(),
                                Default::default(),
                                &mut registry,
                                dummy_project_filesystem(),
                                DigestConfig::testing_default(),
                                observer,
                            ),
                            &dummy_dice_transaction
                        )
                        .await
                        .unwrap()
                        .assert_ready()
                        .downcast::<i32>()?,
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
        let mut registry = DeferredRegistry::new(BaseKey::Base(base));

        let deferred = FakeDeferred {
            inputs: IndexSet::new(),
            val: 2,
        };

        let reserved = registry.reserve();
        let reserved_deferred_data = reserved.data().dupe();

        let deferred_data = registry.bind(reserved, deferred);

        assert_eq!(deferred_data, reserved_deferred_data);

        let result = registry.take_result()?;

        let mut registry = DeferredRegistry::new(BaseKey::Deferred(Arc::new(
            deferred_data.deferred_key().dupe(),
        )));

        let dummy_dice_transaction = dummy_dice_transaction().await?;

        let key = deferred_data.deferred_key().dupe();

        CancellationContext::testing()
            .with_structured_cancellation(|observer| async move {
                assert_eq!(
                    *result
                        .get(deferred_data.deferred_key().id().as_usize())
                        .unwrap()
                        .execute(
                            &mut ResolveDeferredCtx::new(
                                key,
                                Default::default(),
                                Default::default(),
                                Default::default(),
                                &mut registry,
                                dummy_project_filesystem(),
                                DigestConfig::testing_default(),
                                observer,
                            ),
                            &dummy_dice_transaction,
                        )
                        .await
                        .unwrap()
                        .assert_ready()
                        .resolve(&deferred_data)?,
                    2
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
        let mut registry = DeferredRegistry::new(BaseKey::Base(base));

        let _reserved = registry.reserve::<FakeDeferred<()>>();
        let _reserved1 = registry.reserve::<FakeDeferred<()>>();

        assert_eq!(registry.take_result().is_err(), true);
    }

    #[test]
    fn trivial_deferred() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_parse(
            "cell//pkg:foo",
            ConfigurationData::testing_new(),
        ));
        let mut registry = DeferredRegistry::new(BaseKey::Base(base));

        let deferred0 = FakeDeferred {
            inputs: IndexSet::new(),
            val: 123,
        };

        let deferred1 = FakeDeferred {
            inputs: IndexSet::new(),
            val: 456,
        };
        let deferred_data0 = registry.defer_trivial(deferred0.clone());
        let deferred_data1 = registry.reserve_trivial();
        let deferred_data1 = registry.bind_trivial(deferred_data1, deferred1.clone());

        let result = DeferredTable::new(registry.take_result()?);

        assert_eq!(
            *DeferredValueAnyReady::TrivialDeferred(
                result
                    .lookup_deferred(deferred_data0.deferred_key().id())?
                    .as_trivial()
                    .unwrap()
                    .dupe()
            )
            .resolve(&deferred_data0)?,
            deferred0
        );

        assert_eq!(
            *DeferredValueAnyReady::TrivialDeferred(
                result
                    .lookup_deferred(deferred_data1.deferred_key().id())?
                    .as_trivial()
                    .unwrap()
                    .dupe()
            )
            .resolve(&deferred_data1)?,
            deferred1
        );

        Ok(())
    }
}
