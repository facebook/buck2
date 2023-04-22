/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::fmt;
use std::fmt::Display;

use allocative::Allocative;
use starlark_derive::starlark_module;
use starlark_derive::NoSerialize;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::coerce;
use crate::coerce::Coerce;
use crate::collections::symbol_map::Symbol;
use crate::environment::GlobalsBuilder;
use crate::eval::runtime::arguments::ArgNames;
use crate::eval::runtime::arguments::ArgumentsFull;
use crate::eval::runtime::rust_loc::rust_loc;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::slice_vec_ext::SliceExt;
use crate::slice_vec_ext::VecExt;
use crate::starlark_complex_values;
use crate::starlark_type;
use crate::values::dict::DictRef;
use crate::values::function::FUNCTION_TYPE;
use crate::values::layout::typed::string::StringValueLike;
use crate::values::types::tuple::value::Tuple;
use crate::values::Freeze;
use crate::values::Freezer;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;
use crate::values::StarlarkValue;
use crate::values::StringValue;
use crate::values::Trace;
use crate::values::Value;
use crate::values::ValueLike;

#[starlark_module]
pub fn partial(builder: &mut GlobalsBuilder) {
    /// Construct a partial application. In almost all cases it is simpler to use a `lamdba`.
    fn partial<'v>(
        #[starlark(require = pos)] func: Value<'v>,
        #[starlark(args)] args: Value<'v>,
        #[starlark(kwargs)] kwargs: DictRef<'v>,
    ) -> anyhow::Result<Partial<'v>> {
        debug_assert!(Tuple::from_value(args).is_some());
        let names = kwargs
            .keys()
            .map(|x| {
                let x = StringValue::new(x).unwrap();
                (
                    // We duplicate string here.
                    // If this becomes hot, we should do better.
                    Symbol::new_hashed(x.as_str_hashed()),
                    x,
                )
            })
            .collect();
        Ok(Partial {
            func,
            pos: args,
            named: kwargs.values().collect(),
            names,
        })
    }
}

#[derive(Debug, Coerce, Trace, NoSerialize, ProvidesStaticType, Allocative)]
#[repr(C)]
struct PartialGen<V, S> {
    func: V,
    // Always references a tuple.
    pos: V,
    named: Vec<V>,
    names: Vec<(Symbol, S)>,
}

impl<'v, V: ValueLike<'v>, S> PartialGen<V, S> {
    fn pos_content(&self) -> &'v [Value<'v>] {
        Tuple::from_value(self.pos.to_value()).unwrap().content()
    }
}

impl<'v, V: ValueLike<'v>, S> Display for PartialGen<V, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "partial({}, *[", self.func)?;
        for (i, v) in self.pos_content().iter().enumerate() {
            if i != 0 {
                write!(f, ",")?;
            }
            v.fmt(f)?;
        }
        write!(f, "], **{{")?;
        for (i, (k, v)) in self.names.iter().zip(self.named.iter()).enumerate() {
            if i != 0 {
                write!(f, ",")?;
            }
            write!(f, "{}:", k.0.as_str())?;
            v.to_value().fmt(f)?;
        }
        write!(f, "}})")
    }
}

type Partial<'v> = PartialGen<Value<'v>, StringValue<'v>>;
type FrozenPartial = PartialGen<FrozenValue, FrozenStringValue>;
starlark_complex_values!(Partial);

impl<'v> Freeze for Partial<'v> {
    type Frozen = FrozenPartial;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Ok(FrozenPartial {
            func: self.func.freeze(freezer)?,
            pos: freezer.freeze(self.pos)?,
            named: self.named.try_map(|x| x.freeze(freezer))?,
            names: self
                .names
                .into_try_map(|(s, x)| anyhow::Ok((s, x.freeze(freezer)?)))?,
        })
    }
}

impl<'v, V: ValueLike<'v> + 'v, S: StringValueLike<'v> + 'v> StarlarkValue<'v> for PartialGen<V, S>
where
    Self: ProvidesStaticType,
{
    starlark_type!(FUNCTION_TYPE);

    fn name_for_call_stack(&self, _me: Value<'v>) -> String {
        "partial".to_owned()
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        // apply the partial arguments first, then the remaining arguments I was given

        let self_pos = self.pos_content();
        let self_named = coerce(&self.named);
        let self_names = coerce(&self.names);

        eval.alloca_concat(self_pos, args.0.pos, |pos, eval| {
            eval.alloca_concat(self_named, args.0.named, |named, eval| {
                eval.alloca_concat(self_names, args.0.names.names(), |names, eval| {
                    let params = Arguments(ArgumentsFull {
                        pos,
                        named,
                        names: ArgNames::new(names),
                        args: args.0.args,
                        kwargs: args.0.kwargs,
                    });
                    self.func
                        .to_value()
                        .invoke_with_loc(Some(rust_loc!()), &params, eval)
                })
            })
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_partial() {
        assert::pass(
            r#"
def sum(a, b, *args, **kwargs):
    # print("a=%s b=%s args=%s kwargs=%s" % (a, b, args, kwargs))
    args = (a, b) + args
    return [args, kwargs]

# simple test
assert_eq(
    [(1, 2, 3), {"other": True, "third": None}],
    (partial(sum, 1, other=True))(2, 3, third=None))

# passing *args **kwargs to partial
assert_eq(
    [(1, 2, 3), {"other": True, "third": None}],
    (partial(sum, *[1], **{"other": True}))(2, 3, third=None))

# passing *args **kwargs to returned func
assert_eq(
    [(1, 2, 3), {"other": True, "third": None}],
    (partial(sum, other=True))(*[1, 2, 3], **{"third": None}))

# no args to partial
assert_eq(
    [(1, 2, 3), {"other": True, "third": None}],
    (partial(sum))(1, 2, 3, third=None, **{"other": True}))
"#,
        );
    }
}
