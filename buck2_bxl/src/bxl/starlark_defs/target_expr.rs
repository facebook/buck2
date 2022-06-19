use std::borrow::Cow;

use buck2_build_api::{
    calculation::Calculation,
    nodes::{configured::ConfiguredTargetNode, unconfigured::TargetNode},
    query::cquery::environment::CqueryEnvironment,
};
use buck2_core::target::TargetLabel;
use buck2_interpreter::types::target_label::{StarlarkConfiguredTargetLabel, StarlarkTargetLabel};
use buck2_query::query::{
    environment::{QueryEnvironment, QueryTarget},
    syntax::simple::eval::set::TargetSet,
};
use either::Either;
use gazebo::prelude::*;
use starlark::{
    eval::Evaluator,
    values::{list::List, StarlarkValue, Value, ValueLike},
};
use thiserror::Error;

use crate::bxl::starlark_defs::{
    context::BxlContext,
    nodes::{configured::StarlarkConfiguredTargetNode, unconfigured::StarlarkTargetNode},
    targetset::{NodeLike, StarlarkTargetSet},
};

/// TargetExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be target sets. It will accept a
/// literal (like `//some:target`) or list of literals or a TargetSet Value (from one of the
/// bql functions that return them). It can be resolved to a `&TargetSet` with
/// the help of the `targets!()` macro.
pub enum TargetExpr<'v, Node: QueryTarget> {
    Node(Node),
    Label(Cow<'v, Node::NodeRef>),
    Iterable(Vec<Either<Node, Cow<'v, Node::NodeRef>>>),
    TargetSet(Cow<'v, TargetSet<Node>>),
}

impl<'v, Node: NodeLike> TargetExpr<'v, Node> {
    pub async fn get<QueryEnv: QueryEnvironment<Target = Node>>(
        self,
        env: &QueryEnv,
    ) -> anyhow::Result<Cow<'v, TargetSet<Node>>> {
        match self {
            TargetExpr::Node(val) => {
                let mut set = TargetSet::new();
                set.insert(val);
                Ok(Cow::Owned(set))
            }
            TargetExpr::Label(label) => {
                let node = env.get_node(&*label).await?;
                let mut set = TargetSet::new();
                set.insert(node);
                Ok(Cow::Owned(set))
            }
            TargetExpr::Iterable(val) => {
                let mut set = TargetSet::new();
                let futs = val.into_iter().map(|node_or_ref| async {
                    match node_or_ref {
                        Either::Left(node) => Ok(node),
                        Either::Right(node_ref) => env.get_node(&*node_ref).await,
                    }
                });

                for node in futures::future::join_all(futs).await {
                    set.insert(node?);
                }

                Ok(Cow::Owned(set))
            }
            TargetExpr::TargetSet(val) => Ok(val),
        }
    }
}

#[derive(Debug, Error)]
pub enum TargetExprError {
    #[error("Expected a list of target like items, but was `{0}`")]
    NotAListOfTargets(String),
    #[error("Expected a single target like item, but was `{0}`")]
    NotATarget(String),
}

impl<'v> TargetExpr<'v, ConfiguredTargetNode> {
    pub async fn unpack(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContext<'v>,
        env: &CqueryEnvironment<'v>,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<TargetExpr<'v, ConfiguredTargetNode>> {
        Ok(
            if let Some(resolved) = Self::unpack_literal(value, target_platform, ctx, env).await? {
                resolved
            } else if let Some(resolved) =
                Self::unpack_iterable(value, target_platform, ctx, env, eval).await?
            {
                resolved
            } else {
                return Err(anyhow::anyhow!(TargetExprError::NotAListOfTargets(
                    value.to_repr()
                )));
            },
        )
    }

    async fn unpack_literal(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContext<'v>,
        env: &CqueryEnvironment<'v>,
    ) -> anyhow::Result<Option<TargetExpr<'v, ConfiguredTargetNode>>> {
        if let Some(configured_target) = value.downcast_ref::<StarlarkConfiguredTargetNode>() {
            Ok(Some(Self::Node(configured_target.0.dupe())))
        } else if let Some(configured_target) =
            value.downcast_ref::<StarlarkConfiguredTargetLabel>()
        {
            Ok(Some(Self::Label(Cow::Borrowed(configured_target.label()))))
        } else if let Some(s) = value.unpack_str() {
            let targets = env.eval_literals(&[s]).await?;
            Ok(Some(Self::TargetSet(Cow::Owned(targets))))
        } else {
            #[allow(clippy::manual_map)] // `if else if` looks better here
            let maybe_unconfigured =
                if let Some(target) = value.downcast_ref::<StarlarkTargetLabel>() {
                    Some(Cow::Borrowed(target.label()))
                } else if let Some(node) = value.downcast_ref::<StarlarkTargetNode>() {
                    Some(Cow::Borrowed(node.0.label()))
                } else {
                    None
                };

            match maybe_unconfigured {
                None => Ok(None),
                Some(label) => Ok(Some(Self::Label(Cow::Owned(
                    ctx.async_ctx
                        .0
                        .get_configured_target(&*label, target_platform.as_ref())
                        .await?,
                )))),
            }
        }
    }

    async fn unpack_iterable(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContext<'v>,
        env: &CqueryEnvironment<'v>,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Option<TargetExpr<'v, ConfiguredTargetNode>>> {
        if let Some(s) = value.downcast_ref::<StarlarkTargetSet<ConfiguredTargetNode>>() {
            return Ok(Some(Self::TargetSet(Cow::Borrowed(s))));
        }

        #[allow(clippy::manual_map)] // `if else if` looks better here
        let items = if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
            Some(Either::Left(s.iterate(eval.heap())?))
        } else if let Some(iterable) = List::from_value(value) {
            Some(Either::Right(iterable.iter()))
        } else {
            None
        }
        .ok_or_else(|| TargetExprError::NotATarget(value.to_repr()))?;

        let mut resolved = vec![];

        for item in items {
            let unpacked = Self::unpack_literal(item, target_platform, ctx, env).await?;

            match unpacked {
                Some(TargetExpr::Node(node)) => resolved.push(Either::Left(node)),
                Some(TargetExpr::Label(label)) => resolved.push(Either::Right(label)),
                Some(TargetExpr::TargetSet(set)) => match set {
                    Cow::Borrowed(s) => itertools::Either::Left(s.iter().duped()),
                    Cow::Owned(s) => itertools::Either::Right(s.into_iter()),
                }
                .for_each(|t| resolved.push(Either::Left(t))),
                _ => return Err(anyhow::anyhow!(TargetExprError::NotATarget(item.to_repr()))),
            }
        }

        Ok(Some(Self::Iterable(resolved)))
    }
}
