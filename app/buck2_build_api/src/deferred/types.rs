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
use std::marker::PhantomData;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use derivative::Derivative;
use derive_more::Display;
use dupe::Clone_;
use dupe::Dupe;
use gazebo::variants::VariantName;
use indexmap::indexset;
use indexmap::IndexSet;
use itertools::Itertools;
use once_cell::sync::Lazy;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::artifact::Artifact;
use crate::deferred::base_deferred_key::BaseDeferredKey;

/// An asynchronous chunk of work that will be executed when requested.
/// The 'Deferred' can have "inputs" which are values that will be guaranteed to be ready to use
/// before the 'Deferred' is actually executed. These can be 'Artifact's, which means that those
/// 'Artifact's will be materialized and its corresponding 'Action's executed, or other
/// 'DeferredData', which means those 'Deferred' will be computed first.
pub trait Deferred: Allocative {
    type Output;

    /// the set of 'Deferred's that should be computed first before executing this 'Deferred'
    fn inputs(&self) -> &IndexSet<DeferredInput>;

    /// executes this 'Deferred', assuming all inputs and input artifacts are already computed
    fn execute(&self, ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValue<Self::Output>>;

    /// returns a vec of output artifacts if the object has them.
    fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>>;
}

/// The context for executing a 'Deferred'.
pub trait DeferredCtx {
    fn get_configured_target(&self, label: &ConfiguredTargetLabel)
    -> Option<&ConfiguredTargetNode>;

    fn get_action_key(&self) -> String;

    fn get_deferred_data(&self, key: &DeferredKey) -> Option<DeferredValueAnyReady>;

    fn get_artifact(&self, artifact: &Artifact) -> Option<&ArtifactValue>;

    fn get_materialized_artifact(&self, artifact: &Artifact) -> Option<&ProjectRelativePath>;

    fn registry(&mut self) -> &mut DeferredRegistry;

    fn project_filesystem(&self) -> &ProjectRoot;

    fn digest_config(&self) -> DigestConfig;
}

/// DeferredCtx with already resolved values
pub struct ResolveDeferredCtx<'a> {
    key: DeferredKey,
    configured_targets: HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
    deferreds: HashMap<DeferredKey, DeferredValueAnyReady>,
    artifacts: HashMap<Artifact, ArtifactValue>,
    materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
    registry: &'a mut DeferredRegistry,
    project_filesystem: ProjectRoot,
    digest_config: DigestConfig,
}

impl<'a> ResolveDeferredCtx<'a> {
    pub fn new(
        key: DeferredKey,
        configured_targets: HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
        deferreds: HashMap<DeferredKey, DeferredValueAnyReady>,
        artifacts: HashMap<Artifact, ArtifactValue>,
        materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
        registry: &'a mut DeferredRegistry,
        project_filesystem: ProjectRoot,
        digest_config: DigestConfig,
    ) -> Self {
        Self {
            key,
            configured_targets,
            deferreds,
            artifacts,
            materialized_artifacts,
            registry,
            project_filesystem,
            digest_config,
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

    fn get_artifact(&self, artifact: &Artifact) -> Option<&ArtifactValue> {
        self.artifacts.get(artifact)
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
}

/// input to a deferred that needs to be computed first before executing
#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative)]
pub enum DeferredInput {
    ConfiguredTarget(ConfiguredTargetLabel),
    Deferred(DeferredKey),
    Artifact(Artifact),
    MaterializedArtifact(Artifact),
}

/// A value to be stored in 'Provider' fields representing an asynchronously computed value
#[derive(Clone_, Dupe, Display, Derivative, Allocative)]
#[derivative(Debug, Eq, Hash, PartialEq)]
#[display(fmt = "{}", key)]
#[allocative(bound = "")]
pub struct DeferredData<T> {
    key: DeferredKey,
    // by invariant of construction, the key is uniquely identifying of the 'DeferredData'
    #[derivative(Debug = "ignore", Hash = "ignore", PartialEq = "ignore")]
    p: PhantomData<T>,
}

impl<T: Send + Sync + 'static> DeferredData<T> {
    fn new(key: DeferredKey) -> Self {
        Self {
            key,
            p: PhantomData,
        }
    }

    pub fn deferred_key(&self) -> &DeferredKey {
        &self.key
    }

    pub fn resolve(&self, val: DeferredValueAnyReady) -> anyhow::Result<DeferredValueReady<T>> {
        val.downcast_into()
    }
}

/// A key to lookup a 'Deferred' of any result type
#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))] // Recursive type
pub enum DeferredKey {
    /// Base means it's the first deferred registered that can be looked up via the ID based on
    /// analysis of the 'ConfiguredTargetLabel'.
    #[display(fmt = "(target: `{}`, id: `{}`)", _0, _1)]
    Base(BaseDeferredKey, DeferredId),
    /// Points to a 'Deferred' that is generated from another 'Deferred'. The 'DeferredID' can only
    /// be looked up based on the results of executing the deferred at 'DeferredKey'
    #[display(fmt = "(target: `{}`, id: `{}`)", _0, _1)]
    Deferred(Arc<DeferredKey>, DeferredId),
}

impl DeferredKey {
    pub fn id(&self) -> DeferredId {
        *match self {
            DeferredKey::Base(_, id) | DeferredKey::Deferred(_, id) => id,
        }
    }

    /// Create action_key information from the ids, uniquely
    /// identifying this action within this target.
    pub fn action_key(&self) -> String {
        let mut ids = Vec::new();
        let mut x = self;
        loop {
            match x {
                DeferredKey::Base(_, id) => {
                    ids.push(id);
                    break;
                }
                DeferredKey::Deferred(base, id) => {
                    ids.push(id);
                    x = base
                }
            }
        }
        // FIXME(ndmitchell): We'd like to have some kind of user supplied name/category here,
        // rather than using the usize ids, so things are a bit more stable and as these strings
        // are likely to come up in error messages users might see (e.g. with paths).
        ids.iter().rev().map(|x| x.as_usize().to_string()).join("_")
    }

    pub fn owner(&self) -> &BaseDeferredKey {
        let mut x = self;
        loop {
            match x {
                DeferredKey::Base(base, _) => return base,
                DeferredKey::Deferred(base, _) => x = base,
            }
        }
    }
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

    /// Returns a vec of `BuildArtifact`s, if exists.
    fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>>;
}

#[derive(Allocative)]
pub struct TrivialDeferredValue(pub Arc<dyn TrivialDeferred>);

impl DeferredAny for TrivialDeferredValue {
    fn inputs(&self) -> &IndexSet<DeferredInput> {
        static INPUTS: Lazy<IndexSet<DeferredInput>> = Lazy::new(IndexSet::new);
        &INPUTS
    }

    fn execute(&self, _ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValueAny> {
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

    fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>> {
        self.0.debug_artifact_outputs()
    }
}

#[derive(Allocative)]
pub enum DeferredTableEntry {
    Trivial(TrivialDeferredValue),
    Complex(Box<dyn DeferredAny>),
}

impl DeferredAny for DeferredTableEntry {
    fn inputs(&self) -> &IndexSet<DeferredInput> {
        match self {
            Self::Trivial(v) => v.inputs(),
            Self::Complex(v) => v.inputs(),
        }
    }

    fn execute(&self, ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValueAny> {
        match self {
            Self::Trivial(v) => v.execute(ctx),
            Self::Complex(v) => v.execute(ctx),
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

    fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>> {
        match self {
            Self::Trivial(v) => v.debug_artifact_outputs(),
            Self::Complex(v) => v.debug_artifact_outputs(),
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
        ReservedDeferredData::new(DeferredData::new(self.base_key.make_key(id)))
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
        ReservedTrivialDeferredData::new(DeferredData::new(self.base_key.make_key(id)))
    }

    /// binds a reserved 'ReservedDeferredData' to a 'Deferred'
    pub fn bind<D, T>(&mut self, reserved: ReservedDeferredData<T>, d: D) -> DeferredData<T>
    where
        D: Deferred<Output = T> + Send + Sync + 'static,
        T: Allocative + Clone + Debug + Send + Sync + 'static,
    {
        let id = reserved.0.key.id().as_usize();

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
        let id = reserved.0.key.id().as_usize();

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
        DeferredData::new(self.base_key.make_key(id))
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
        DeferredData::new(self.base_key.make_key(id))
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

        impl<T, U, F> Deferred for Map<T, U, F>
        where
            T: Allocative + Send + Sync + 'static,
            F: Fn(&T, &mut dyn DeferredCtx) -> DeferredValue<U> + 'static,
            U: Allocative + 'static,
        {
            type Output = U;

            fn inputs(&self) -> &IndexSet<DeferredInput> {
                &self.orig
            }

            fn execute(
                &self,
                ctx: &mut dyn DeferredCtx,
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

            fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>> {
                Ok(None)
            }
        }

        self.defer(Map {
            orig: indexset![DeferredInput::Deferred(orig.key.dupe())],
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
        Self::Deferred(k.key)
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
pub trait DeferredAny: Allocative + Send + Sync {
    /// the set of 'Deferred's that should be computed first before executing this 'Deferred'
    fn inputs(&self) -> &IndexSet<DeferredInput>;

    fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>>;

    /// executes this 'Deferred', assuming all inputs and input artifacts are already computed
    fn execute(&self, ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValueAny>;

    fn as_any(&self) -> &dyn Any;

    fn type_name(&self) -> &str;
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

/// An id to look up the deferred work
#[derive(Clone, Copy, Debug, Dupe, Display, Allocative)]
// comment because linters and fmt don't agree
#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
#[display(fmt = "{}", id)]
pub struct DeferredId {
    id: u32,
    trivial: bool,
}

impl DeferredId {
    /// Gets the underlying ID for this DeferredId. This should be used for logging only.
    pub(crate) fn as_usize(self) -> usize {
        self.id as _
    }

    pub(crate) fn is_trivial(self) -> bool {
        self.trivial
    }
}

impl<D, T> DeferredAny for D
where
    D: Deferred<Output = T> + Send + Sync + Any + 'static,
    T: Allocative + Clone + Debug + Send + Sync + 'static,
{
    fn inputs(&self) -> &IndexSet<DeferredInput> {
        self.inputs()
    }

    fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>> {
        D::debug_artifact_outputs(self)
    }

    fn execute(&self, ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValueAny> {
        match self.execute(ctx)? {
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
}

pub mod testing {
    use gazebo::variants::VariantName;

    use crate::deferred::types::AnyValue;
    use crate::deferred::types::DeferredData;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredKey;
    use crate::deferred::types::DeferredResult;
    use crate::deferred::types::DeferredTable;
    use crate::deferred::types::DeferredTableEntry;
    use crate::deferred::types::DeferredValueAny;
    use crate::deferred::types::DeferredValueAnyReady;

    pub trait DeferredDataExt<T> {
        fn testing_new(key: DeferredKey) -> DeferredData<T>;
    }

    impl<T: Send + Sync + 'static> DeferredDataExt<T> for DeferredData<T> {
        fn testing_new(key: DeferredKey) -> DeferredData<T> {
            DeferredData::new(key)
        }
    }

    pub trait DeferredIdExt {
        fn testing_new(id: u32) -> DeferredId;
    }

    impl DeferredIdExt for DeferredId {
        fn testing_new(id: u32) -> DeferredId {
            DeferredId { id, trivial: false }
        }
    }

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
    use std::fmt;
    use std::fmt::Debug;
    use std::fmt::Formatter;
    use std::sync::Arc;

    use allocative::Allocative;
    use buck2_core::configuration::ConfigurationData;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::package::PackageLabel;
    use buck2_core::target::label::ConfiguredTargetLabel;
    use buck2_core::target::name::TargetName;
    use buck2_execute::digest_config::DigestConfig;
    use dupe::Dupe;
    use indexmap::indexset;
    use indexmap::IndexSet;

    use super::AnyValue;
    use super::TrivialDeferred;
    use crate::actions::artifact::build_artifact::BuildArtifact;
    use crate::deferred::base_deferred_key::BaseDeferredKey;
    use crate::deferred::types::testing::DeferredValueAnyExt;
    use crate::deferred::types::BaseKey;
    use crate::deferred::types::Deferred;
    use crate::deferred::types::DeferredAny;
    use crate::deferred::types::DeferredCtx;
    use crate::deferred::types::DeferredData;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredInput;
    use crate::deferred::types::DeferredKey;
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

    impl<T: Clone> Deferred for FakeDeferred<T> {
        type Output = T;

        fn inputs(&self) -> &IndexSet<DeferredInput> {
            &self.inputs
        }

        fn execute(&self, _ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValue<T>> {
            Ok(DeferredValue::Ready(self.val.clone()))
        }

        fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>> {
            Ok(None)
        }
    }

    impl TrivialDeferred for FakeDeferred<i32> {
        fn as_any_value(&self) -> &dyn AnyValue {
            self
        }

        fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>> {
            Ok(None)
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq, Allocative)]
    #[allocative(bound = "")]
    struct DeferringDeferred<T> {
        inputs: IndexSet<DeferredInput>,
        #[allocative(skip)]
        defer: FakeDeferred<T>,
    }

    impl<T: Clone + Debug + Allocative + Send + Sync + 'static> Deferred for DeferringDeferred<T> {
        type Output = T;

        fn inputs(&self) -> &IndexSet<DeferredInput> {
            &self.inputs
        }

        fn execute(&self, ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValue<T>> {
            Ok(DeferredValue::Deferred(
                ctx.registry().defer(self.defer.clone()),
            ))
        }

        fn debug_artifact_outputs(&self) -> anyhow::Result<Option<Vec<BuildArtifact>>> {
            Ok(None)
        }
    }

    fn make_resolved<T: Clone + Debug + Allocative + Send + Sync + 'static>(
        data: &DeferredData<T>,
        deferred: &FakeDeferred<T>,
    ) -> (DeferredKey, DeferredValueAnyReady) {
        (
            data.key.dupe(),
            DeferredValueAnyReady::AnyValue(Arc::new(deferred.val.clone())),
        )
    }

    fn dummy_base() -> BaseDeferredKey {
        BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            PackageLabel::testing_parse("cell//pkg"),
            TargetName::unchecked_new("foo"),
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

    #[test]
    fn register_deferred() -> anyhow::Result<()> {
        let target = dummy_base();
        let mut registry = DeferredRegistry::new(BaseKey::Base(target.dupe()));

        let deferred = FakeDeferred {
            inputs: IndexSet::new(),
            val: 2,
        };

        let deferred_data = registry.defer(deferred);

        let result = registry.take_result()?;

        let mut ctx = DeferredRegistry::new(BaseKey::Deferred(Arc::new(deferred_data.key.dupe())));

        assert_eq!(
            *deferred_data.resolve(
                result
                    .get(deferred_data.key.id().as_usize())
                    .unwrap()
                    .execute(&mut ResolveDeferredCtx::new(
                        deferred_data.key.dupe(),
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        &mut ctx,
                        dummy_project_filesystem(),
                        DigestConfig::testing_default(),
                    ))
                    .unwrap()
                    .assert_ready()
            )?,
            2
        );

        Ok(())
    }

    #[test]
    fn mapping_async_data() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            PackageLabel::testing_parse("cell//pkg"),
            TargetName::unchecked_new("foo"),
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
        let mapped_deferred = result.get(mapped.key.id().as_usize()).unwrap();

        assert_eq!(
            mapped_deferred.inputs(),
            &indexset![DeferredInput::Deferred(deferred_data.key.dupe())]
        );

        let mut registry =
            DeferredRegistry::new(BaseKey::Deferred(Arc::new(deferred_data.key.dupe())));
        let mut resolved = ResolveDeferredCtx::new(
            deferred_data.key.dupe(),
            Default::default(),
            vec![make_resolved(&deferred_data, &deferred)]
                .into_iter()
                .collect(),
            Default::default(),
            Default::default(),
            &mut registry,
            dummy_project_filesystem(),
            DigestConfig::testing_default(),
        );

        assert_eq!(
            *mapped_deferred
                .execute(&mut resolved)
                .unwrap()
                .assert_ready()
                .downcast::<i32>()?,
            1
        );

        Ok(())
    }

    #[test]
    fn register_nested_deferred() -> anyhow::Result<()> {
        let target = ConfiguredTargetLabel::testing_new(
            PackageLabel::testing_parse("cell//pkg"),
            TargetName::unchecked_new("foo"),
            ConfigurationData::testing_new(),
        );
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
            deferring_deferred_data.key.dupe(),
        )));
        let exec_result = result
            .get(deferring_deferred_data.key.id().as_usize())
            .unwrap()
            .execute(&mut ResolveDeferredCtx::new(
                deferring_deferred_data.key.dupe(),
                Default::default(),
                Default::default(),
                Default::default(),
                Default::default(),
                &mut registry,
                dummy_project_filesystem(),
                DigestConfig::testing_default(),
            ))
            .unwrap();

        let deferred_key = match exec_result {
            DeferredValueAny::Ready(_) => panic!("expected a deferred"),
            DeferredValueAny::Deferred(deferred) => deferred,
        };

        assert_eq!(
            deferred_key,
            DeferredKey::Deferred(
                Arc::new(deferring_deferred_data.key),
                DeferredId {
                    id: 0,
                    trivial: false
                }
            )
        );

        let result = registry.take_result()?;
        let deferred = result.get(deferred_key.id().as_usize()).unwrap();

        let mut registry = DeferredRegistry::new(BaseKey::Deferred(Arc::new(deferred_key.dupe())));
        assert_eq!(
            *deferred
                .execute(&mut ResolveDeferredCtx::new(
                    deferred_key,
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    &mut registry,
                    dummy_project_filesystem(),
                    DigestConfig::testing_default(),
                ))
                .unwrap()
                .assert_ready()
                .downcast::<i32>()?,
            2
        );

        Ok(())
    }

    #[test]
    fn reserving_deferred() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            PackageLabel::testing_parse("cell//pkg"),
            TargetName::unchecked_new("foo"),
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

        let mut registry =
            DeferredRegistry::new(BaseKey::Deferred(Arc::new(deferred_data.key.dupe())));

        let key = deferred_data.key.dupe();
        assert_eq!(
            *deferred_data.resolve(
                result
                    .get(deferred_data.key.id().as_usize())
                    .unwrap()
                    .execute(&mut ResolveDeferredCtx::new(
                        key,
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        &mut registry,
                        dummy_project_filesystem(),
                        DigestConfig::testing_default(),
                    ))
                    .unwrap()
                    .assert_ready()
            )?,
            2
        );

        Ok(())
    }

    #[test]
    fn reserving_deferred_unbound() {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            PackageLabel::testing_parse("cell//pkg"),
            TargetName::unchecked_new("foo"),
            ConfigurationData::testing_new(),
        ));
        let mut registry = DeferredRegistry::new(BaseKey::Base(base));

        let _reserved = registry.reserve::<FakeDeferred<()>>();
        let _reserved1 = registry.reserve::<FakeDeferred<()>>();

        assert_eq!(registry.take_result().is_err(), true);
    }

    #[test]
    fn trivial_deferred() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            PackageLabel::testing_parse("cell//pkg"),
            TargetName::unchecked_new("foo"),
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
            *deferred_data0.resolve(DeferredValueAnyReady::TrivialDeferred(
                result
                    .lookup_deferred(deferred_data0.deferred_key().id())?
                    .as_trivial()
                    .unwrap()
                    .dupe()
            ))?,
            deferred0
        );

        assert_eq!(
            *deferred_data1.resolve(DeferredValueAnyReady::TrivialDeferred(
                result
                    .lookup_deferred(deferred_data1.deferred_key().id())?
                    .as_trivial()
                    .unwrap()
                    .dupe()
            ))?,
            deferred1
        );

        Ok(())
    }
}
