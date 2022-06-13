use std::borrow::Cow;

use buck2_build_api::{
    calculation::Calculation,
    interpreter::rule_defs::target_label::{StarlarkConfiguredTargetLabel, StarlarkTargetLabel},
    nodes::{configured::ConfiguredTargetNode, unconfigured::TargetNode},
    query::cquery::environment::CqueryEnvironment,
};
use buck2_core::target::TargetLabel;
use buck2_query::query::{
    environment::{QueryEnvironment, QueryTarget},
    syntax::simple::eval::set::TargetSet,
};
use either::Either;
use gazebo::prelude::*;
use starlark::{
    eval::Evaluator,
    values::{list::List, StarlarkValue, UnpackValue, Value, ValueLike},
};
use thiserror::Error;

use crate::bxl::starlark_defs::{
    context::BxlContext,
    nodes::{configured::StarlarkConfiguredTargetNode, unconfigured::StarlarkTargetNode},
    targetset::NodeLike,
};

/// Converts a TargetExpr to a &TargetSet.
macro_rules! targets {
    ($env:expr, $e:expr) => {
        &*($e.get($env).await?)
    };
}
pub(crate) use targets;

use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

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
    // TODO remove
    IterableValue(Value<'v>),
    Value(&'v str),
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
            TargetExpr::IterableValue(val) => {
                let list = List::from_value(val).unwrap();
                let targets = list
                    .iter()
                    .map(|val| val.unpack_str().unwrap())
                    .collect::<Vec<_>>();

                let targets = env.eval_literals(&targets).await?;
                Ok(Cow::Owned(targets))
            }
            TargetExpr::Value(val) => {
                let targets = env.eval_literals(&[val]).await?;
                Ok(Cow::Owned(targets))
            }
        }
    }

    // This will unpack a Value to a TargetExpr, but doesn't accept as single string literal,
    // only a TargetSet or a list of string literals.
    pub(crate) fn unpack_set(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.downcast_ref::<StarlarkTargetSet<Node>>() {
            return Some(TargetExpr::TargetSet(Cow::Borrowed(s)));
        } else if let Some(iterable) = List::from_value(value) {
            let mut good = true;
            for val in iterable.iter() {
                if val.unpack_str().is_none() {
                    good = false;
                    break;
                }
            }
            if good {
                return Some(TargetExpr::IterableValue(value));
            }
        }
        None
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
            if let Some(target) = value.downcast_ref::<StarlarkTargetLabel>() {
                Some(Cow::Borrowed(target.label()))
            } else if let Some(node) = value.downcast_ref::<StarlarkTargetNode>() {
                Some(Cow::Borrowed(node.0.label()))
            } else {
                None
            }
            .map_or(Ok(None), |label| {
                let result: anyhow::Result<_> = try {
                    Self::Label(Cow::Owned(ctx.async_ctx.via_dice(|ctx| {
                        ctx.get_configured_target(&*label, target_platform.as_ref())
                    })?))
                };
                result.map(Some)
            })
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

impl<'v, Node: NodeLike> UnpackValue<'v> for TargetExpr<'v, Node> {
    fn expected() -> String {
        "str or target set".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.unpack_str() {
            Some(TargetExpr::Value(s))
        } else {
            TargetExpr::unpack_set(value)
        }
    }
}
