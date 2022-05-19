use std::borrow::Cow;

use buck2_query::query::environment::{QueryEnvironment, QueryTarget};
use starlark::values::{list::List, UnpackValue, Value, ValueLike};

use crate::bxl::{starlark_defs::targetset::NodeLike, StarlarkTargetSet};

/// Converts a TargetExpr to a &TargetSet.
macro_rules! targets {
    ($env:expr, $e:expr) => {
        &*($e.get($env).await?)
    };
}
pub(crate) use targets;

/// TargetExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be target sets. It will accept a
/// literal (like `//some:target`) or list of literals or a TargetSet Value (from one of the
/// bql functions that return them). It can be resolved to a `&TargetSet` with
/// the help of the `targets!()` macro.
pub enum TargetExpr<'v, Node: QueryTarget> {
    Literal(&'v str),
    Iterable(Value<'v>),
    TargetSet(&'v StarlarkTargetSet<Node>),
}

impl<'v, Node: NodeLike> TargetExpr<'v, Node> {
    pub async fn get<QueryEnv: QueryEnvironment<Target = Node>>(
        self,
        env: &QueryEnv,
    ) -> anyhow::Result<Cow<'v, StarlarkTargetSet<Node>>> {
        let targets = match self {
            TargetExpr::Literal(val) => {
                vec![val]
            }
            TargetExpr::Iterable(val) => {
                let list = List::from_value(val).unwrap();
                list.iter().map(|val| val.unpack_str().unwrap()).collect()
            }
            TargetExpr::TargetSet(val) => return Ok(Cow::Borrowed(val)),
        };
        let targets = env.eval_literals(&targets).await?;
        Ok(Cow::Owned(StarlarkTargetSet::from(targets)))
    }

    // This will unpack a Value to a TargetExpr, but doesn't accept as single string literal,
    // only a TargetSet or a list of string literals.
    pub(crate) fn unpack_set(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.downcast_ref::<StarlarkTargetSet<Node>>() {
            return Some(TargetExpr::TargetSet(s));
        } else if let Some(iterable) = List::from_value(value) {
            let mut good = true;
            for val in iterable.iter() {
                if val.unpack_str().is_none() {
                    good = false;
                    break;
                }
            }
            if good {
                return Some(TargetExpr::Iterable(value));
            }
        }
        None
    }
}

impl<'v, Node: NodeLike> UnpackValue<'v> for TargetExpr<'v, Node> {
    fn expected() -> String {
        "str or target set".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.unpack_str() {
            Some(TargetExpr::Literal(s))
        } else {
            TargetExpr::unpack_set(value)
        }
    }
}
