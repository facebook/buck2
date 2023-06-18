/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use starlark::docs::get_registered_starlark_docs;
use starlark::environment::Globals;
use starlark::environment::LibraryExtension;
use starlark::typing::*;

pub(crate) fn oracle_buck(globals: &Globals) -> Arc<dyn TypingOracle + Send + Sync> {
    let registered_docs = get_registered_starlark_docs();
    let mut docs = OracleDocs::new();
    docs.add_module(&globals.documentation());
    docs.add_docs(&registered_docs);

    let mut docs2 = OracleDocs::new();
    docs2.add_docs(&registered_docs);

    Arc::new(OracleSeq(vec![
        // TODO(nga): this should only accept globals from enabled extensions, not all extensions.
        Box::new(OracleStandard::new(LibraryExtension::all()))
            as Box<dyn TypingOracle + Send + Sync>,
        Box::new(CustomBuck),
        Box::new(docs),
        Box::new(AddErrors(docs2)),
    ]))
}

struct CustomBuck;

impl TypingOracle for CustomBuck {
    fn attribute(&self, ty: &Ty, attr: &str) -> Option<Result<Ty, ()>> {
        match ty.as_name()? {
            "dependency" => match attr {
                // We can index by providers, which either appear as functions (for builtin providers)
                // or as Any (for user providers)
                "__index__" => Some(Ok(Ty::function(vec![Param::pos_only(Ty::Any)], Ty::Any))),
                "__in__" => Some(Ok(Ty::function(vec![Param::pos_only(Ty::Any)], Ty::bool()))),
                _ => None,
            },
            _ => None,
        }
    }

    fn builtin(&self, name: &str) -> Option<Result<Ty, ()>> {
        match name {
            // A provider can be used as an index and can be called.
            // Our type checker only supports calling Ty::function, so we erase the result
            // and return Ty::Any so it doesn't get upset.
            "provider" => Some(Ok(Ty::function(
                vec![
                    Param::name_only("doc", Ty::string()).optional(),
                    Param::name_only(
                        "fields",
                        Ty::union2(Ty::list(Ty::string()), Ty::dict(Ty::string(), Ty::string())),
                    ),
                ],
                Ty::Any,
            ))),
            // Again, we can't call a rule, so make it return a Any.
            // We can't return just a function because some people do give it types.
            "rule" => Some(Ok(Ty::function(
                vec![
                    Param::name_only("impl", Ty::Any),
                    Param::name_only("attrs", Ty::dict(Ty::string(), Ty::name("attribute"))),
                    Param::name_only("cfg", Ty::Any).optional(),
                    Param::name_only("doc", Ty::string()).optional(),
                    Param::name_only("is_configuration_rule", Ty::bool()).optional(),
                    Param::name_only("is_toolchain_rule", Ty::bool()).optional(),
                ],
                Ty::Any,
            ))),
            _ => None,
        }
    }

    fn subtype(&self, require: &TyName, got: &TyName) -> bool {
        match require.as_str() {
            "provider" => got.as_str().ends_with("Info"),
            _ => false,
        }
    }
}

struct AddErrors(OracleDocs);

impl TypingOracle for AddErrors {
    fn attribute(&self, ty: &Ty, _attr: &str) -> Option<Result<Ty, ()>> {
        if self.0.known_object(ty.as_name()?) {
            Some(Err(()))
        } else {
            None
        }
    }
}
