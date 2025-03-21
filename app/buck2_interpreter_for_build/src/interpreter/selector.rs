/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use allocative::Allocative;
use buck2_error::BuckErrorContext;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_complex_value;
use starlark::starlark_module;
use starlark::values::dict::Dict;
use starlark::values::dict::DictRef;
use starlark::values::dict::DictType;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Tracer;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUncheckedGeneric;

/// Representation of `select()` in Starlark.
#[derive(Debug, ProvidesStaticType, NoSerialize, Allocative)] // TODO selector should probably support serializing
#[repr(C)]
pub enum StarlarkSelectorGen<V: ValueLifetimeless> {
    /// Simplest form, backed by dictionary representation
    /// wrapped into `select` function call.
    Primary(ValueOfUncheckedGeneric<V, DictType<FrozenStringValue, FrozenValue>>),
    Sum(V, V),
}

impl<V: ValueLifetimeless> Display for StarlarkSelectorGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StarlarkSelectorGen::Primary(v) => {
                f.write_str("select(")?;
                Display::fmt(v, f)?;
                f.write_str(")")
            }
            StarlarkSelectorGen::Sum(l, r) => {
                Display::fmt(l, f)?;
                f.write_str(" + ")?;
                Display::fmt(r, f)
            }
        }
    }
}

unsafe impl<From: Coerce<To> + ValueLifetimeless, To: ValueLifetimeless>
    Coerce<StarlarkSelectorGen<To>> for StarlarkSelectorGen<From>
{
}

starlark_complex_value!(pub StarlarkSelector);

impl<'v> StarlarkSelector<'v> {
    pub fn new(d: ValueOf<'v, DictType<StringValue<'v>, Value<'v>>>) -> Self {
        StarlarkSelector::Primary(d.as_unchecked().cast())
    }

    fn sum(left: Value<'v>, right: Value<'v>, heap: &'v Heap) -> Value<'v> {
        heap.alloc(StarlarkSelector::Sum(left, right))
    }

    pub fn from_concat<I>(iter: I, heap: &'v Heap) -> buck2_error::Result<Value<'v>>
    where
        I: IntoIterator<Item = Value<'v>>,
    {
        fn values_to_selector<'v, I>(
            selector: Option<StarlarkSelector<'v>>,
            values: &mut I,
            heap: &'v Heap,
        ) -> buck2_error::Result<NoneOr<StarlarkSelector<'v>>>
        where
            I: Iterator<Item = Value<'v>>,
        {
            match (selector, values.next()) {
                (None, None) => Ok(NoneOr::None),
                (None, Some(v)) => {
                    if let Some(next_v) = values.next() {
                        let head = StarlarkSelector::Sum(v, next_v);
                        values_to_selector(Some(head), values, heap)
                    } else {
                        let v = ValueOf::unpack_value_err(v.to_value())
                            .internal_error("validated at construction")?;
                        Ok(NoneOr::Other(StarlarkSelector::new(v)))
                    }
                }
                (Some(s), None) => Ok(NoneOr::Other(s)),
                (Some(s), Some(v)) => {
                    let head = Some(StarlarkSelector::Sum(heap.alloc(s), v));
                    values_to_selector(head, values, heap)
                }
            }
        }
        let selector = values_to_selector(None, &mut iter.into_iter(), heap)?;
        Ok(heap.alloc(selector))
    }

    fn select_map<'a>(
        val: Value<'a>,
        eval: &mut Evaluator<'a, '_, '_>,
        func: Value<'a>,
    ) -> starlark::Result<Value<'a>> {
        fn invoke<'v>(
            eval: &mut Evaluator<'v, '_, '_>,
            func: Value<'v>,
            val: Value<'v>,
        ) -> starlark::Result<Value<'v>> {
            eval.eval_function(func, &[val], &[])
        }

        if let Some(selector) = StarlarkSelector::from_value(val) {
            match *selector {
                StarlarkSelectorGen::Primary(selector) => {
                    let selector = DictRef::from_value(selector.get()).unwrap();
                    let mut mapped = SmallMap::with_capacity(selector.len());
                    for (k, v) in selector.iter_hashed() {
                        mapped.insert_hashed(k, invoke(eval, func, v)?);
                    }
                    Ok(eval.heap().alloc(StarlarkSelector::new(
                        ValueOf::unpack_value_err(eval.heap().alloc(Dict::new(mapped)))
                            .internal_error("validated at construction")?,
                    )))
                }
                StarlarkSelectorGen::Sum(left, right) => {
                    Ok(eval.heap().alloc(StarlarkSelectorGen::Sum(
                        Self::select_map(left, eval, func)?,
                        Self::select_map(right, eval, func)?,
                    )))
                }
            }
        } else {
            invoke(eval, func, val)
        }
    }

    fn select_test<'a>(
        val: Value<'a>,
        eval: &mut Evaluator<'a, '_, '_>,
        func: Value<'a>,
    ) -> starlark::Result<bool> {
        fn invoke<'v>(
            eval: &mut Evaluator<'v, '_, '_>,
            func: Value<'v>,
            val: Value<'v>,
        ) -> starlark::Result<bool> {
            eval.eval_function(func, &[val], &[])?
                .unpack_bool()
                .ok_or_else(|| {
                    starlark::Error::new_kind(starlark::ErrorKind::Native(
                        buck2_error::buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "Expected testing function to have a boolean return type"
                        )
                        .into(),
                    ))
                })
        }

        if let Some(selector) = StarlarkSelector::from_value(val) {
            match *selector {
                StarlarkSelectorGen::Primary(selector) => {
                    let selector = DictRef::from_value(selector.get()).unwrap();
                    for v in selector.values() {
                        let result = invoke(eval, func, v)?;
                        if result {
                            return Ok(true);
                        }
                    }
                    Ok(false)
                }
                StarlarkSelectorGen::Sum(left, right) => {
                    Ok(Self::select_test(left, eval, func)?
                        || Self::select_test(right, eval, func)?)
                }
            }
        } else {
            invoke(eval, func, val)
        }
    }
}

trait StarlarkSelectorBase<'v> {
    type Item: ValueLike<'v>;
}

impl<'v> StarlarkSelectorBase<'v> for StarlarkSelector<'v> {
    type Item = Value<'v>;
}

unsafe impl<'v> Trace<'v> for StarlarkSelector<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        match self {
            Self::Primary(a) => a.trace(tracer),
            Self::Sum(a, b) => {
                tracer.trace(a);
                tracer.trace(b);
            }
        }
    }
}

impl<'v> Freeze for StarlarkSelector<'v> {
    type Frozen = FrozenStarlarkSelector;
    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        Ok(match self {
            StarlarkSelector::Primary(v) => FrozenStarlarkSelector::Primary(v.freeze(freezer)?),
            StarlarkSelector::Sum(l, r) => {
                FrozenStarlarkSelector::Sum(l.freeze(freezer)?, r.freeze(freezer)?)
            }
        })
    }
}

impl StarlarkSelectorBase<'_> for FrozenStarlarkSelector {
    type Item = FrozenValue;
}

#[starlark_value(type = "selector")] // TODO(nga): rename to `"Select"` to match constant name.
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkSelectorGen<V>
where
    Self: ProvidesStaticType<'v> + StarlarkSelectorBase<'v, Item = V>,
{
    fn to_bool(&self) -> bool {
        true
    }

    fn radd(&self, left: Value<'v>, heap: &'v Heap) -> Option<starlark::Result<Value<'v>>> {
        let right = heap.alloc(match self {
            StarlarkSelectorGen::Primary(x) => StarlarkSelectorGen::Primary(x.to_value()),
            StarlarkSelectorGen::Sum(x, y) => StarlarkSelectorGen::Sum(x.to_value(), y.to_value()),
        });
        Some(Ok(StarlarkSelector::sum(left, right, heap)))
    }

    fn add(&self, other: Value<'v>, heap: &'v Heap) -> Option<starlark::Result<Value<'v>>> {
        match self {
            Self::Primary(ref v) => match ValueOf::unpack_value_err(v.get().to_value()) {
                Ok(v) => {
                    let this = heap.alloc(StarlarkSelector::new(v));
                    Some(Ok(StarlarkSelector::sum(this, other, heap)))
                }
                Err(e) => Some(Err(e.into())),
            },
            Self::Sum(ref l, ref r) => {
                let this = StarlarkSelector::sum(l.to_value(), r.to_value(), heap);
                Some(Ok(StarlarkSelector::sum(this, other, heap)))
            }
        }
    }
}

#[starlark_module]
pub fn register_select(globals: &mut GlobalsBuilder) {
    const Select: StarlarkValueAsType<StarlarkSelector> = StarlarkValueAsType::new();

    fn select<'v>(
        #[starlark(require = pos)] d: ValueOf<'v, DictType<StringValue<'v>, Value<'v>>>,
    ) -> starlark::Result<StarlarkSelector<'v>> {
        Ok(StarlarkSelector::new(d))
    }

    /// Maps a selector.
    ///
    /// Each value within a selector map and on each side of an addition will be passed to the
    /// mapping function. The returned selector will have the same structure as this one.
    ///
    /// Ex:
    /// ```python
    /// def increment_items(a):
    ///     return [v + 1 for v in a]
    ///
    /// select_map([1, 2] + select({"c": [2]}), increment_items) == [2, 3] + select({"c": [3]})
    /// ```
    fn select_map<'v>(
        #[starlark(require = pos)] d: Value<'v>,
        #[starlark(require = pos)] func: Value<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        StarlarkSelector::select_map(d, eval, func)
    }

    /// Test values in the select expression using the given function.
    ///
    /// Returns True, if any value in the select passes, else False.
    ///
    /// Ex:
    /// ```python
    /// select_test([1] + select({"c": [1]}), lambda a: len(a) > 1) == False
    /// select_test([1, 2] + select({"c": [1]}), lambda a: len(a) > 1) == True
    /// select_test([1] + select({"c": [1, 2]}), lambda a: len(a) > 1) == True
    /// ```
    fn select_test<'v>(
        #[starlark(require = pos)] d: Value<'v>,
        #[starlark(require = pos)] func: Value<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<bool> {
        StarlarkSelector::select_test(d, eval, func)
    }
}
#[starlark_module]
pub fn register_select_internal(globals: &mut GlobalsBuilder) {
    /// Tests that two selects are equal to each other. For testing use only.
    /// We simply compare their string representations.
    fn select_equal<'v>(
        #[starlark(require = pos)] left: Value<'v>,
        #[starlark(require = pos)] right: Value<'v>,
    ) -> starlark::Result<bool> {
        Ok(left.to_repr() == right.to_repr())
    }
}
