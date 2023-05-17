/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::future::Future;
use std::io::Write;
use std::pin::Pin;
use std::sync::Arc;

use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::target::label::TargetLabel;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_query::query::environment::LabeledNode;
use buck2_query::query::environment::NodeLabel;
use buck2_query::query::environment::QueryTarget;
use buck2_util::late_binding::LateBinding;
use derivative::Derivative;
use dice::DiceComputations;
use dupe::Dupe;
use gazebo::variants::VariantName;
use indexmap::IndexMap;
use internment::ArcIntern;
use ref_cast::RefCast;
use serde::Serialize;
use serde::Serializer;

use crate::actions::key::ActionKey;
use crate::actions::RegisteredAction;
use crate::analysis::AnalysisResult;
use crate::artifact_groups::TransitiveSetProjectionKey;

impl NodeLabel for ActionKey {}

#[derive(Debug, derive_more::Display, RefCast, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ActionAttr(pub(crate) str);

impl ActionAttr {
    pub(crate) fn new(x: &str) -> &Self {
        ActionAttr::ref_cast(x)
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct SetProjectionInputsData {
    key: TransitiveSetProjectionKey,
    pub direct: Vec<ActionKey>,
    pub(crate) children: Vec<SetProjectionInputs>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SetProjectionInputs {
    pub node: ArcIntern<SetProjectionInputsData>,
}

// ArcIntern doesn't impl Dupe, but is cheap to copy, so we need to explicitly impl it.
impl Dupe for SetProjectionInputs {}

impl SetProjectionInputs {
    pub fn new(
        key: TransitiveSetProjectionKey,
        direct: Vec<ActionKey>,
        children: Vec<SetProjectionInputs>,
    ) -> Self {
        Self {
            node: ArcIntern::new(SetProjectionInputsData {
                key,
                direct,
                children,
            }),
        }
    }
}

#[derive(Debug)]
pub enum ActionInput {
    ActionKey(ActionKey),
    IndirectInputs(SetProjectionInputs),
}

#[derive(Derivative, Clone, Dupe)]
#[derivative(Debug)]
pub struct ActionQueryNode {
    action: Arc<RegisteredAction>,
    deps: Arc<Vec<ActionInput>>,
    #[derivative(Debug = "ignore")]
    fs: Arc<ArtifactFs>,
}

impl ActionQueryNode {
    pub fn new(action: Arc<RegisteredAction>, deps: Vec<ActionInput>, fs: Arc<ArtifactFs>) -> Self {
        Self {
            action,
            deps: Arc::new(deps),
            fs,
        }
    }

    pub fn attrs(&self) -> IndexMap<String, String> {
        let mut attrs = self.action.action().aquery_attributes(&ExecutorFs::new(
            &self.fs,
            self.action.execution_config().options.path_separator,
        ));
        attrs.insert(
            "executor_configuration".to_owned(),
            self.action.execution_config().executor.to_string(),
        );
        attrs
    }

    pub fn action(&self) -> Arc<RegisteredAction> {
        self.action.dupe()
    }
}

impl LabeledNode for ActionQueryNode {
    type NodeRef = ActionKey;

    fn node_ref(&self) -> &Self::NodeRef {
        self.action.key()
    }
}

impl QueryTarget for ActionQueryNode {
    type Attr<'a> = ActionAttr;

    fn rule_type(&self) -> Cow<str> {
        Cow::Owned(self.action.kind().variant_name().to_ascii_lowercase())
    }

    /// Return the path to the buildfile that defines this target, e.g. `fbcode//foo/bar/TARGETS`
    fn buildfile_path(&self) -> &BuildFilePath {
        // TODO(cjhopman): In addition to implementing this, we should be able to return an anyhow::Error here rather than panicking.
        unimplemented!("buildfile not yet implemented in aquery")
    }

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        struct Iter<'a> {
            visited: HashSet<&'a SetProjectionInputs>,
            queue: VecDeque<&'a SetProjectionInputs>,
        }

        impl<'a> Iter<'a> {
            fn new<From: Iterator<Item = &'a SetProjectionInputs>>(iter: From) -> Self {
                let mut visited = HashSet::new();
                let mut queue = VecDeque::new();
                for it in iter {
                    if visited.insert(it) {
                        queue.push_back(it);
                    }
                }
                Self { visited, queue }
            }
        }

        impl<'a> Iterator for Iter<'a> {
            type Item = &'a SetProjectionInputs;

            fn next(&mut self) -> Option<Self::Item> {
                self.queue.pop_front().map(|node| {
                    for child in &*node.node.children {
                        if self.visited.insert(child) {
                            self.queue.push_back(child);
                        }
                    }

                    node
                })
            }
        }

        let direct = self.deps.iter().filter_map(|input| match input {
            ActionInput::ActionKey(action_key) => Some(action_key),
            ActionInput::IndirectInputs(..) => None,
        });

        let indirect = Iter::new(self.deps.iter().filter_map(|input| match input {
            ActionInput::ActionKey(..) => None,
            ActionInput::IndirectInputs(val) => Some(val),
        }));

        let indirect = Iter::new(indirect);

        Box::new(direct.chain(indirect.flat_map(|v| v.node.direct.iter())))
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        Box::new(std::iter::empty())
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        self.deps()
    }

    fn attr_any_matches(
        attr: &Self::Attr<'_>,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        filter(&attr.0)
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        mut _func: F,
    ) -> Result<(), E> {
        Ok(())
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        func("kind", ActionAttr::new(&self.rule_type()))?;
        func("category", ActionAttr::new(self.action.category().as_str()))?;
        func(
            "identifier",
            ActionAttr::new(self.action.identifier().unwrap_or("")),
        )?;
        // TODO(cjhopman): impl inputs/outputs for actions in aquery
        func("inputs", ActionAttr::new(""))?;
        func("outputs", ActionAttr::new(""))?;

        for (k, v) in self.attrs() {
            func(&k, ActionAttr::new(&v))?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, key: &str, mut func: F) -> R {
        let mut res = None;

        self.attrs_for_each(|k, attr| {
            if k == key {
                res = Some(func(Some(attr)));
            }
            Ok::<(), anyhow::Error>(())
        })
        .unwrap();
        match res {
            Some(v) => v,
            None => func(None),
        }
    }

    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(
        &self,
        mut _func: F,
    ) -> Result<(), E> {
        // TODO(cjhopman): In addition to implementing this, we should be able to return an anyhow::Error here rather than panicking.
        unimplemented!("inputs not yet implemented in aquery")
    }

    fn call_stack(&self) -> Option<String> {
        None
    }

    fn attr_to_string_alternate(&self, attr: &Self::Attr<'_>) -> String {
        format!("{:#}", attr)
    }

    fn attr_serialize<S: Serializer>(
        &self,
        attr: &Self::Attr<'_>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        attr.serialize(serializer)
    }
}

pub static FIND_MATCHING_ACTION: LateBinding<
    for<'c> fn(
        &'c DiceComputations,
        // Working dir.
        &'c ProjectRelativePath,
        // global_target_platform
        Option<TargetLabel>,
        &'c AnalysisResult,
        // path_after_target_name
        ForwardRelativePathBuf,
    ) -> Pin<
        Box<dyn Future<Output = anyhow::Result<Option<ActionQueryNode>>> + Send + 'c>,
    >,
> = LateBinding::new("FIND_MATCHING_ACTION");

/// Hook to link printer in `buck2_server_commands` from `buck2_audit_server`.
pub static PRINT_ACTION_NODE: LateBinding<
    for<'a> fn(
        stdout: &'a mut (dyn Write + Send),
        action: ActionQueryNode,
        json: bool,
        output_attributes: &'a [String],
        cell_resolver: &'a CellResolver,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<()>> + Send + 'a>>,
> = LateBinding::new("PRINT_ACTION_NODE");
