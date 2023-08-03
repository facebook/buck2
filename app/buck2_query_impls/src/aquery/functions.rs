/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::marker::PhantomData;

use buck2_query::query::syntax::simple::functions::helpers::QueryBinaryOp;
use buck2_query::query::syntax::simple::functions::helpers::QueryFunction;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use buck2_query::query::syntax::simple::functions::QueryFunctions;
use buck2_query::query_module;
use buck2_query_parser::BinaryOp;

use crate::aquery::environment::AqueryEnvironment;

pub fn aquery_functions<'a>() -> impl QueryFunctions<Env = AqueryEnvironment<'a>> {
    struct Functions<'a> {
        defaults: DefaultQueryFunctionsModule<AqueryEnvironment<'a>>,
        extra_functions: AqueryFunctions<'a>,
    }

    impl Debug for Functions<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Functions").finish_non_exhaustive()
        }
    }

    impl<'a> QueryFunctions for Functions<'a> {
        type Env = AqueryEnvironment<'a>;

        fn get(&self, name: &str) -> Option<&dyn QueryFunction<AqueryEnvironment<'a>>> {
            if let Some(v) = self.extra_functions.get(name) {
                Some(v)
            } else {
                self.defaults.get(name)
            }
        }

        fn get_op(&self, op: BinaryOp) -> Option<&dyn QueryBinaryOp<AqueryEnvironment<'a>>> {
            if let Some(v) = self.extra_functions.get_op(op) {
                Some(v)
            } else {
                self.defaults.get_op(op)
            }
        }
    }

    Functions {
        defaults: DefaultQueryFunctionsModule::new(),
        extra_functions: AqueryFunctions(PhantomData),
    }
}

#[derive(Debug)]
struct AqueryFunctions<'a>(PhantomData<&'a ()>);

#[query_module(AqueryEnvironment<'a>)]
impl<'a> AqueryFunctions<'a> {}
