/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A type [`StarlarkPromise`] which provides basic promise-like functionality.
use std::cell::Cell;
use std::cell::RefCell;
use std::mem;

use allocative::Allocative;
use derivative::Derivative;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use thiserror::Error;

/// A type that corresponds to a Rust promise.
#[derive(
    ProvidesStaticType,
    NoSerialize,
    StarlarkDocs,
    Display,
    Derivative,
    Trace,
    Allocative
)]
#[derivative(Debug)]
#[display(fmt = "promise()")]
pub struct StarlarkPromise<'v> {
    /// The value of the promise.
    #[allocative(skip)]
    value: Cell<PromiseValue<'v>>,
    /// Everyone who is downstream of this promise and needs executing when it changes.
    /// These will all be `.map` style nodes.
    /// Note that as a consequence of this back pointer, we can't garbage collect promises
    /// that are generated from this promise, even if they aren't used.
    #[derivative(Debug = "ignore")]
    downstream: RefCell<Vec<ValueTyped<'v, StarlarkPromise<'v>>>>,
    /// Things we want to validate whenever the value of this promise is resolved.
    #[derivative(Debug = "ignore")]
    validate: RefCell<Vec<Validate<'v>>>,
}

#[derive(Allocative, Trace)]
struct Validate<'v>(#[trace(unsafe_ignore)] fn(Value<'v>) -> anyhow::Result<()>);

#[derive(Copy, Clone, Dupe, Debug, Trace, Allocative)]
enum PromiseValue<'v> {
    Unresolved,
    Resolved(Value<'v>),
    Map(ValueTyped<'v, StarlarkPromise<'v>>, Value<'v>),
}

#[derive(Debug, Error)]
enum PromiseError {
    #[error("Can't .resolve on a promise produced with .map")]
    CantResolveMap,
    #[error("Can't .resolve on a promise which already has a value")]
    CantResolveTwice,
}

impl<'v> StarlarkPromise<'v> {
    /// The result of calling `type()` on promise.
    pub const TYPE: &'static str = "promise";

    /// Create a new unresolved promise.
    /// Must have [`StarlarkPromise::resolve`] called on it later.
    pub fn new_unresolved() -> Self {
        Self {
            value: Cell::new(PromiseValue::Unresolved),
            downstream: RefCell::new(Vec::new()),
            validate: RefCell::new(Vec::new()),
        }
    }

    /// Create a new resolved promise.
    pub fn new_resolved(value: Value<'v>) -> Self {
        Self {
            value: Cell::new(PromiseValue::Resolved(value)),
            ..Self::new_unresolved()
        }
    }

    /// Get the value from a resolved promise, or [`None`] otherwise.
    pub fn get(&self) -> Option<Value<'v>> {
        match self.value.get() {
            PromiseValue::Resolved(x) => Some(x),
            _ => None,
        }
    }

    fn apply(
        f: Value<'v>,
        x: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        eval.eval_function(f, &[x], &[])
    }

    fn map(
        x: ValueTyped<'v, StarlarkPromise<'v>>,
        f: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkPromise<'v>>> {
        match x.value.get() {
            PromiseValue::Resolved(x) => Ok(eval
                .heap()
                .alloc_typed(Self::new_resolved(Self::apply(f, x, eval)?))),
            _ => {
                let res = eval.heap().alloc_typed(Self {
                    value: Cell::new(PromiseValue::Map(x, f)),
                    ..Self::new_unresolved()
                });
                x.downstream.borrow_mut().push(res);
                Ok(res)
            }
        }
    }

    /// Validate the type of a promise. Will execute once the promise is resolved.
    pub fn validate(&self, f: fn(Value<'v>) -> anyhow::Result<()>) -> anyhow::Result<()> {
        match self.value.get() {
            PromiseValue::Resolved(x) => f(x),
            _ => {
                self.validate.borrow_mut().push(Validate(f));
                Ok(())
            }
        }
    }

    /// Resolve a promise. Errors if the promise was produced by `.map` or the promise has
    /// already been resolved.
    pub fn resolve(&self, x: Value<'v>, eval: &mut Evaluator<'v, '_>) -> anyhow::Result<()> {
        if matches!(self.value.get(), PromiseValue::Map(..)) {
            return Err(PromiseError::CantResolveMap.into());
        }
        self.resolve_rec(x, eval)
    }

    fn resolve_rec(&self, x: Value<'v>, eval: &mut Evaluator<'v, '_>) -> anyhow::Result<()> {
        if matches!(self.value.get(), PromiseValue::Resolved(_)) {
            return Err(PromiseError::CantResolveTwice.into());
        }
        for f in self.validate.borrow().iter() {
            f.0(x)?;
        }
        self.value.set(PromiseValue::Resolved(x));
        // We clear validate/downstream because we don't need them anymore, so might as well save the memory
        self.validate.borrow_mut().clear();
        let mut downstream = Vec::new();
        mem::swap(&mut *self.downstream.borrow_mut(), &mut downstream);
        for d in downstream {
            let map = match d.value.get() {
                PromiseValue::Map(_, f) => f,
                _ => panic!("Impossible to reach a downstream promise that is not a map"),
            };
            let x2 = Self::apply(map, x, eval)?;
            d.resolve_rec(x2, eval)?;
        }
        Ok(())
    }

    /// Downcast the value.
    pub fn from_value(x: Value<'v>) -> Option<&'v Self> {
        x.downcast_ref()
    }
}

// We can't use starlark_complex_value! because there is no frozen form of a promise

impl<'v> AllocValue<'v> for StarlarkPromise<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        // FIXME: need to be able to freeze things that are resolved
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v StarlarkPromise<'v> {
    fn starlark_type_repr() -> String {
        StarlarkPromise::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v StarlarkPromise<'v> {
    fn unpack_value(x: Value<'v>) -> Option<Self> {
        StarlarkPromise::from_value(x)
    }
}

impl<'v> StarlarkValue<'v> for StarlarkPromise<'v> {
    starlark_type!(StarlarkPromise::TYPE);

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(promise_methods)
    }
}

#[starlark_module]
fn promise_methods(builder: &mut MethodsBuilder) {
    fn map<'v>(
        this: ValueTyped<'v, StarlarkPromise<'v>>,
        #[starlark(require = pos)] func: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkPromise<'v>>> {
        StarlarkPromise::map(this, func, eval)
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use gazebo::any::ProvidesStaticType;
    use starlark::environment::GlobalsBuilder;
    use starlark::environment::Module;
    use starlark::syntax::AstModule;
    use starlark::syntax::Dialect;
    use starlark::values::none::NoneType;
    use starlark::values::tuple::Tuple;
    use starlark::values::Value;

    use super::*;

    #[derive(
        ProvidesStaticType,
        NoSerialize,
        Trace,
        Default,
        Debug,
        Display,
        Allocative
    )]
    #[display(fmt = "{:?}", self)]
    struct Promises<'v>(RefCell<Vec<(String, ValueTyped<'v, StarlarkPromise<'v>>)>>);

    impl<'v> StarlarkValue<'v> for Promises<'v> {
        starlark_type!("promises");
    }

    #[starlark_module]
    fn helpers(builder: &mut GlobalsBuilder) {
        fn promise_unresolved<'v>(
            name: String,
            eval: &mut Evaluator<'v, '_>,
        ) -> anyhow::Result<ValueTyped<'v, StarlarkPromise<'v>>> {
            let promises = get_promises(eval.module());
            let promise = eval.heap().alloc_typed(StarlarkPromise::new_unresolved());
            promises.0.borrow_mut().push((name, promise));
            Ok(promise)
        }

        fn promise_resolved<'v>(value: Value<'v>) -> anyhow::Result<StarlarkPromise<'v>> {
            Ok(StarlarkPromise::new_resolved(value))
        }

        fn promise_validate<'v>(
            promise: ValueTyped<'v, StarlarkPromise<'v>>,
        ) -> anyhow::Result<NoneType> {
            promise.validate(|x| {
                if x.unpack_str() == Some("ok") {
                    Ok(())
                } else {
                    Err(anyhow::anyhow!("VALIDATE_FAILED"))
                }
            })?;
            Ok(NoneType)
        }
    }

    fn alloc_promises<'v>(modu: &'v Module) {
        modu.set(
            "__promises__",
            modu.heap().alloc_complex_no_freeze(Promises::default()),
        )
    }

    fn get_promises<'v>(modu: &'v Module) -> &'v Promises<'v> {
        modu.get("__promises__").unwrap().downcast_ref().unwrap()
    }

    fn assert_promise<'v>(modu: &'v Module, content: &str) -> anyhow::Result<Value<'v>> {
        alloc_promises(modu);
        let globals = GlobalsBuilder::extended().with(helpers).build();
        let ast = AstModule::parse("test.bzl", content.to_owned(), &Dialect::Extended)?;
        let mut eval = Evaluator::new(modu);
        let res = eval.eval_module(ast, &globals)?;
        let promises = get_promises(modu);
        for (key, promise) in promises.0.borrow().iter() {
            promise.resolve(modu.heap().alloc(key), &mut eval)?;
        }
        Ok(res)
    }

    fn assert_promise_err<'v>(modu: &'v Module, content: &str, err: &str) -> anyhow::Error {
        match assert_promise(modu, content) {
            Ok(_) => panic!("Expected an error, got a result"),
            Err(e) => {
                if format!("{:#}", e).contains(err) {
                    e
                } else {
                    panic!(
                        "Wrong error message, expected to see `{}`, got `{:?}`",
                        err, e
                    )
                }
            }
        }
    }

    #[test]
    fn test_promise() {
        let modu = Module::new();
        let res = assert_promise(
            &modu,
            r#"
a = promise_unresolved("test")
b = a.map(lambda x: x.upper())
c = b.map(lambda x: x + "!")
d = a.map(lambda x: x.title())
e = promise_resolved("more")
f = e.map(lambda x: x.upper())
(a,b,c,d,e,f)
"#,
        )
        .unwrap();
        let wants = &["test", "TEST", "TEST!", "Test", "more", "MORE"];
        for (want, got) in wants.iter().zip(Tuple::from_value(res).unwrap().content()) {
            assert_eq!(
                StarlarkPromise::from_value(*got)
                    .unwrap()
                    .get()
                    .unwrap()
                    .unpack_str()
                    .unwrap(),
                *want
            );
        }
    }

    #[test]
    fn test_promise_validate() {
        let modu = Module::new();
        assert_promise(
            &modu,
            r#"
p = promise_unresolved("ok")
promise_validate(p)
p
"#,
        )
        .unwrap();
        assert_promise_err(
            &modu,
            r#"
p = promise_unresolved("test")
promise_validate(p)
p
"#,
            "VALIDATE_FAILED",
        );
    }
}
