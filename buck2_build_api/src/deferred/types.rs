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
use std::collections::HashSet;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::marker::PhantomData;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::base_deferred_key::BaseDeferredKey;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;
use gazebo::variants::VariantName;
use indexmap::indexset;
use indexmap::IndexSet;
use itertools::Itertools;
use owning_ref::ArcRef;
use thiserror::Error;

use crate::actions::artifact::Artifact;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;

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
}

/// The context for executing a 'Deferred'.
pub trait DeferredCtx {
    fn get_configured_target(&self, label: &ConfiguredTargetLabel)
    -> Option<&ConfiguredTargetNode>;

    fn get_action_key(&self) -> String;

    fn get_provider(
        &self,
        label: &ConfiguredProvidersLabel,
    ) -> Option<&FrozenProviderCollectionValue>;

    fn get_deferred_data(&self, key: &DeferredKey) -> Option<Arc<dyn AnyValue>>;

    fn get_artifact(&self, artifact: &Artifact) -> Option<&ArtifactValue>;

    fn get_materialized_artifact(&self, artifact: &Artifact) -> Option<&ProjectRelativePath>;

    fn registry(&mut self) -> &mut DeferredRegistry;

    fn project_filesystem(&self) -> &ProjectRoot;
}

/// DeferredCtx with already resolved values
pub struct ResolveDeferredCtx<'a> {
    key: DeferredKey,
    configured_targets: HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
    providers: HashMap<ConfiguredProvidersLabel, FrozenProviderCollectionValue>,
    deferreds: HashMap<DeferredKey, Arc<dyn AnyValue>>,
    artifacts: HashMap<Artifact, ArtifactValue>,
    materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
    registry: &'a mut DeferredRegistry,
    project_filesystem: ProjectRoot,
}

impl<'a> ResolveDeferredCtx<'a> {
    pub fn new(
        key: DeferredKey,
        configured_targets: HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
        providers: HashMap<ConfiguredProvidersLabel, FrozenProviderCollectionValue>,
        deferreds: HashMap<DeferredKey, Arc<dyn AnyValue>>,
        artifacts: HashMap<Artifact, ArtifactValue>,
        materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
        registry: &'a mut DeferredRegistry,
        project_filesystem: ProjectRoot,
    ) -> Self {
        Self {
            key,
            configured_targets,
            providers,
            deferreds,
            artifacts,
            materialized_artifacts,
            registry,
            project_filesystem,
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

    fn get_provider(
        &self,
        label: &ConfiguredProvidersLabel,
    ) -> Option<&FrozenProviderCollectionValue> {
        self.providers.get(label)
    }

    fn get_deferred_data(&self, key: &DeferredKey) -> Option<Arc<dyn AnyValue>> {
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
}

/// input to a deferred that needs to be computed first before executing
#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative)]
pub enum DeferredInput {
    ConfiguredTarget(ConfiguredTargetLabel),
    Provider(ConfiguredProvidersLabel),
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

    pub fn resolve(&self, val: Arc<dyn AnyValue>) -> anyhow::Result<ArcRef<dyn AnyValue, T>> {
        ArcRef::new(val).try_map(|v| v.downcast::<T>())
    }

    /// zip/merges two 'DeferredData' into one deferred returning a tuple of their results
    pub fn zip<V: Send + 'static>(
        data1: &DeferredData<T>,
        data2: &DeferredData<V>,
    ) -> impl Deferred<Output = (ArcRef<dyn AnyValue, T>, ArcRef<dyn AnyValue, V>)> {
        #[derive(Allocative)]
        #[allocative(bound = "")]
        struct Combined<U, V> {
            inputs: IndexSet<DeferredInput>,
            u: DeferredKey,
            v: DeferredKey,
            p: PhantomData<(U, V)>,
        }

        impl<U: Send + 'static, V: Send + 'static> Deferred for Combined<U, V> {
            type Output = (ArcRef<dyn AnyValue, U>, ArcRef<dyn AnyValue, V>);

            fn inputs(&self) -> &IndexSet<DeferredInput> {
                &self.inputs
            }

            fn execute(
                &self,
                ctx: &mut dyn DeferredCtx,
            ) -> anyhow::Result<DeferredValue<Self::Output>> {
                Ok(DeferredValue::Ready((
                    ArcRef::new(ctx.get_deferred_data(&self.u).unwrap())
                        .map(|u| u.downcast().unwrap()),
                    ArcRef::new(ctx.get_deferred_data(&self.v).unwrap())
                        .map(|v| v.downcast().unwrap()),
                )))
            }
        }

        let inputs = indexset![
            DeferredInput::Deferred(data1.key.dupe()),
            DeferredInput::Deferred(data2.key.dupe()),
        ];

        Combined {
            inputs,
            u: data1.key.dupe(),
            v: data2.key.dupe(),
            p: PhantomData,
        }
    }
}

/// A key to lookup a 'Deferred' of any result type
#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))] // Recusive type
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

/// The registry for creating 'DeferredData's and registering 'Deferred's
#[derive(Allocative)]
pub struct DeferredRegistry {
    base_key: BaseKey,
    registry: HashMap<DeferredId, Box<dyn DeferredAny>>,
    pending: HashSet<DeferredId>,
    id: usize,
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

impl DeferredRegistry {
    pub fn new(base_key: BaseKey) -> Self {
        Self {
            base_key,
            registry: HashMap::new(),
            pending: HashSet::new(),
            id: 0,
        }
    }

    fn next_id(&mut self) -> DeferredId {
        let id = DeferredId(self.id);
        self.id += 1;
        id
    }

    /// Reserves a 'DeferredData', with it's underlying key, on the promise that it should be bound
    /// to a 'Deferred' before 'take_result' is called.
    pub fn reserve<T>(&mut self) -> ReservedDeferredData<T>
    where
        T: Clone + Send + Sync + 'static,
    {
        let id = self.next_id();
        assert!(self.pending.insert(id), "id's should never be duplicate");
        ReservedDeferredData::new(DeferredData::new(self.base_key.make_key(id)))
    }

    /// binds a reserved 'ReservedDeferredData' to a 'Deferred'
    pub fn bind<D, T>(&mut self, reserved: ReservedDeferredData<T>, d: D) -> DeferredData<T>
    where
        D: Deferred<Output = T> + Send + Sync + 'static,
        T: Allocative + Clone + Debug + Send + Sync + 'static,
    {
        assert!(
            self.pending.remove(&reserved.0.key.id()),
            "the reserved should always be in pending"
        );
        self.registry.insert(reserved.0.key.id(), box d);

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
        let id = self.next_id();
        self.registry.insert(id, box d);

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
        }

        self.defer(Map {
            orig: indexset![DeferredInput::Deferred(orig.key.dupe())],
            f,
            p: PhantomData,
        })
    }

    pub fn take_result(self) -> anyhow::Result<HashMap<DeferredId, Box<dyn DeferredAny>>> {
        if self.pending.is_empty() {
            Ok(self.registry)
        } else {
            Err(anyhow::anyhow!(DeferredErrors::UnboundReservedDeferred(
                self.pending
            )))
        }
    }
}

#[derive(Debug, Error)]
pub enum DeferredErrors {
    #[error("no deferred found for deferred id `{0}`")]
    DeferredNotFound(usize),
    #[error("reserved deferred ids of `{0:?}` was never bound")]
    UnboundReservedDeferred(HashSet<DeferredId>),
}

/// Contains all the deferreds generated by analyzing a particular rule implementation
#[derive(Clone, Debug, Dupe, Allocative)]
pub struct DeferredResult(Arc<DeferredResultData>);

#[derive(Allocative)]
struct DeferredResultData {
    deferreds: HashMap<DeferredId, Box<dyn DeferredAny>>,
    value: DeferredValueAny,
}

#[derive(Clone, Dupe, Allocative)]
pub struct DeferredTable(Arc<HashMap<DeferredId, Box<dyn DeferredAny>>>);

impl Debug for DeferredResultData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "DeferredResult(value=`{:?}`, deferreds=`{:?}`)",
            self.deferreds.keys(),
            self.value
        )
    }
}

impl Debug for DeferredTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "DeferredTable(deferreds=`{:?}`)", self.0.keys(),)
    }
}
impl DeferredTable {
    pub fn new(deferreds: HashMap<DeferredId, Box<dyn DeferredAny>>) -> Self {
        Self(Arc::new(deferreds))
    }

    /// looks up an 'Deferred' given the id
    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<&(dyn DeferredAny + 'static)> {
        match self.0.get(&id) {
            Some(deferred) => Ok(&**deferred),
            None => Err(anyhow::anyhow!(DeferredErrors::DeferredNotFound(id.0))),
        }
    }
}

impl DeferredResult {
    pub fn new(
        value: DeferredValueAny,
        deferreds: HashMap<DeferredId, Box<dyn DeferredAny>>,
    ) -> Self {
        Self(Arc::new(DeferredResultData { deferreds, value }))
    }

    /// looks up an 'Deferred' given the id
    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<&(dyn DeferredAny + 'static)> {
        match self.0.deferreds.get(&id) {
            Some(deferred) => Ok(&**deferred),
            None => Err(anyhow::anyhow!(DeferredErrors::DeferredNotFound(id.0))),
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

/// untyped value computed by the deferred. This is same as 'DeferredValue', but with 'T' as an
/// 'ValueAny'
#[derive(Debug, VariantName, Clone, Dupe, Allocative)]
pub enum DeferredValueAny {
    Ready(Arc<dyn AnyValue>),
    Deferred(DeferredKey),
}

impl DeferredValueAny {
    fn ready<T: Allocative + Clone + Debug + Send + Sync + 'static>(t: T) -> Self {
        Self::Ready(Arc::new(t))
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

    /// executes this 'Deferred', assuming all inputs and input artifacts are already computed
    fn execute(&self, ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValueAny>;
}

/// An id to look up the deferred work
#[derive(Clone, Copy, Debug, Dupe, Display, Allocative)]
// comment because linters and fmt don't agree
#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DeferredId(usize);

impl DeferredId {
    /// Gets the underlying ID for this DeferredId. This should be used for logging only.
    pub(crate) fn as_usize(self) -> usize {
        self.0
    }
}

impl<D, T> DeferredAny for D
where
    D: Deferred<Output = T> + Send + Sync + 'static,
    T: Allocative + Clone + Debug + Send + Sync + 'static,
{
    fn inputs(&self) -> &IndexSet<DeferredInput> {
        self.inputs()
    }

    fn execute(&self, ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValueAny> {
        match self.execute(ctx)? {
            DeferredValue::Ready(t) => Ok(DeferredValueAny::ready(t)),
            DeferredValue::Deferred(d) => Ok(DeferredValueAny::defer(d)),
        }
    }
}

pub mod testing {
    use std::collections::hash_map::RandomState;
    use std::collections::HashMap;
    use std::sync::Arc;

    use gazebo::variants::VariantName;

    use crate::deferred::types::AnyValue;
    use crate::deferred::types::DeferredAny;
    use crate::deferred::types::DeferredData;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredKey;
    use crate::deferred::types::DeferredResult;
    use crate::deferred::types::DeferredTable;
    use crate::deferred::types::DeferredValueAny;

    pub trait DeferredDataExt<T> {
        fn testing_new(key: DeferredKey) -> DeferredData<T>;
    }

    impl<T: Send + Sync + 'static> DeferredDataExt<T> for DeferredData<T> {
        fn testing_new(key: DeferredKey) -> DeferredData<T> {
            DeferredData::new(key)
        }
    }

    pub trait DeferredIdExt {
        fn testing_new(id: usize) -> DeferredId;
    }

    impl DeferredIdExt for DeferredId {
        fn testing_new(id: usize) -> DeferredId {
            DeferredId(id)
        }
    }

    pub trait DeferredValueAnyExt {
        fn assert_ready(self) -> Arc<dyn AnyValue>;
        fn assert_deferred(self) -> DeferredKey;
    }

    impl DeferredValueAnyExt for DeferredValueAny {
        fn assert_ready(self) -> Arc<dyn AnyValue> {
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
        fn get_registered(&self) -> &HashMap<DeferredId, Box<dyn DeferredAny>>;
    }

    impl DeferredAnalysisResultExt for DeferredResult {
        fn get_registered(&self) -> &HashMap<DeferredId, Box<dyn DeferredAny>, RandomState> {
            &self.0.deferreds
        }
    }

    impl DeferredAnalysisResultExt for DeferredTable {
        fn get_registered(&self) -> &HashMap<DeferredId, Box<dyn DeferredAny>, RandomState> {
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
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_core::target::testing::ConfiguredTargetLabelExt;
    use buck2_core::target::ConfiguredTargetLabel;
    use buck2_core::target::TargetName;
    use buck2_execute::base_deferred_key::BaseDeferredKey;
    use gazebo::prelude::*;
    use indexmap::indexset;
    use indexmap::IndexSet;

    use crate::deferred::types::testing::DeferredValueAnyExt;
    use crate::deferred::types::AnyValue;
    use crate::deferred::types::BaseKey;
    use crate::deferred::types::Deferred;
    use crate::deferred::types::DeferredCtx;
    use crate::deferred::types::DeferredData;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredInput;
    use crate::deferred::types::DeferredKey;
    use crate::deferred::types::DeferredRegistry;
    use crate::deferred::types::DeferredValue;
    use crate::deferred::types::DeferredValueAny;
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
    }

    fn make_resolved<T: Clone + Debug + Allocative + Send + Sync + 'static>(
        data: &DeferredData<T>,
        deferred: &FakeDeferred<T>,
    ) -> (DeferredKey, Arc<dyn AnyValue>) {
        (data.key.dupe(), Arc::new(deferred.val.clone()))
    }

    fn dummy_base() -> BaseDeferredKey {
        BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        ))
    }

    fn dummy_project_filesystem() -> ProjectRoot {
        let cwd = if cfg!(windows) {
            AbsNormPath::new("c:/tmp").unwrap().to_owned()
        } else {
            AbsNormPath::new("/dev/null").unwrap().to_owned()
        };
        ProjectRoot::new(cwd)
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
                    .get(&deferred_data.key.id())
                    .unwrap()
                    .execute(&mut ResolveDeferredCtx::new(
                        deferred_data.key.dupe(),
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        &mut ctx,
                        dummy_project_filesystem()
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
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        ));
        let mut registry = DeferredRegistry::new(BaseKey::Base(base.dupe()));

        let deferred = FakeDeferred {
            inputs: IndexSet::new(),
            val: 0,
        };

        let deferred_data = registry.defer(deferred.clone());
        let mapped = registry.map(&deferred_data, |x, _ctx| DeferredValue::Ready(x + 1));

        let result = registry.take_result()?;
        let mapped_deferred = result.get(&mapped.key.id()).unwrap();

        assert_eq!(
            mapped_deferred.inputs(),
            &indexset![DeferredInput::Deferred(deferred_data.key.dupe())]
        );

        let mut registry =
            DeferredRegistry::new(BaseKey::Deferred(Arc::new(deferred_data.key.dupe())));
        let mut resolved = ResolveDeferredCtx::new(
            deferred_data.key.dupe(),
            Default::default(),
            Default::default(),
            vec![make_resolved(&deferred_data, &deferred)]
                .into_iter()
                .collect(),
            Default::default(),
            Default::default(),
            &mut registry,
            dummy_project_filesystem(),
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
    fn combined_asyncs() -> anyhow::Result<()> {
        let base = BaseDeferredKey::TargetLabel(ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        ));
        let mut registry = DeferredRegistry::new(BaseKey::Base(base));

        let deferred0 = FakeDeferred {
            inputs: IndexSet::new(),
            val: 4,
        };

        let deferred_data0 = registry.defer(deferred0.clone());

        let deferred1 = FakeDeferred {
            inputs: IndexSet::new(),
            val: "foo".to_owned(),
        };

        let deferred_data1 = registry.defer(deferred1.clone());

        let combined_data = registry.defer(DeferredData::zip(&deferred_data0, &deferred_data1));

        let result = registry.take_result()?;

        let mut registry =
            DeferredRegistry::new(BaseKey::Deferred(Arc::new(combined_data.key.dupe())));
        let mut resolved = ResolveDeferredCtx::new(
            combined_data.key.dupe(),
            Default::default(),
            Default::default(),
            vec![
                make_resolved(&deferred_data0, &deferred0),
                make_resolved(&deferred_data1, &deferred1),
            ]
            .into_iter()
            .collect(),
            Default::default(),
            Default::default(),
            &mut registry,
            dummy_project_filesystem(),
        );

        let combined_deferred = result.get(&combined_data.key.id()).unwrap();
        let (u, v) = &*combined_data.resolve(
            combined_deferred
                .execute(&mut resolved)
                .unwrap()
                .assert_ready(),
        )?;
        assert_eq!((&**u, &**v), (&4, &"foo".to_owned()));

        Ok(())
    }

    #[test]
    fn register_nested_deferred() -> anyhow::Result<()> {
        let target = ConfiguredTargetLabel::testing_new(
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        );
        let id = DeferredId(1);

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
            .get(&deferring_deferred_data.key.id())
            .unwrap()
            .execute(&mut ResolveDeferredCtx::new(
                deferring_deferred_data.key.dupe(),
                Default::default(),
                Default::default(),
                Default::default(),
                Default::default(),
                Default::default(),
                &mut registry,
                dummy_project_filesystem(),
            ))
            .unwrap();

        let deferred_key = match exec_result {
            DeferredValueAny::Ready(_) => panic!("expected a deferred"),
            DeferredValueAny::Deferred(deferred) => deferred,
        };

        assert_eq!(
            deferred_key,
            DeferredKey::Deferred(Arc::new(deferring_deferred_data.key), DeferredId(0))
        );

        let result = registry.take_result()?;
        let deferred = result.get(&deferred_key.id()).unwrap();

        let mut registry = DeferredRegistry::new(BaseKey::Deferred(Arc::new(deferred_key.dupe())));
        assert_eq!(
            *deferred
                .execute(&mut ResolveDeferredCtx::new(
                    deferred_key,
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    &mut registry,
                    dummy_project_filesystem()
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
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
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
                    .get(&deferred_data.key.id())
                    .unwrap()
                    .execute(&mut ResolveDeferredCtx::new(
                        key,
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        Default::default(),
                        &mut registry,
                        dummy_project_filesystem()
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
            Package::testing_new("cell", "pkg"),
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        ));
        let mut registry = DeferredRegistry::new(BaseKey::Base(base));

        let _reserved = registry.reserve::<FakeDeferred<()>>();
        let _reserved1 = registry.reserve::<FakeDeferred<()>>();

        assert_eq!(registry.take_result().is_err(), true);
    }
}
