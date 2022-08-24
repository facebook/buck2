/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::package_listing::listing::testing::PackageListingExt;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::CellAlias;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellName;
use buck2_core::package::Package;
use buck2_interpreter::common::StarlarkPath;
use buck2_interpreter::extra::cell_info::InterpreterCellInfo;
use buck2_interpreter::extra::BuildContext;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_query::query::syntax::simple::eval::values::QueryResult;
use buck2_query::query::syntax::simple::functions::QueryFunctionsExt;
use buck2_query::query::syntax::simple::functions::QueryLiteralVisitor;
use buck2_query_parser::spanned::Spanned;
use buck2_query_parser::Expr;
use starlark::environment::Globals;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::syntax::AstModule;
use starlark::syntax::Dialect;
use starlark::values::Value;

use crate::attrs::coerce::ctx::BuildAttrCoercionContext;

pub trait CoercedAttrExt {
    fn from_literal(lit: AttrLiteral<CoercedAttr>) -> Self;
}

impl CoercedAttrExt for CoercedAttr {
    fn from_literal(lit: AttrLiteral<CoercedAttr>) -> Self {
        CoercedAttr::new_literal(lit)
    }
}

pub trait ConfiguredAttrExt {
    fn from_literal(lit: AttrLiteral<ConfiguredAttr>) -> Self;
}

impl ConfiguredAttrExt for ConfiguredAttr {
    fn from_literal(lit: AttrLiteral<ConfiguredAttr>) -> Self {
        ConfiguredAttr(lit)
    }
}

pub fn coercion_ctx() -> impl AttrCoercionContext {
    coercion_ctx_listing(PackageListing::testing_empty())
}

pub fn coercion_ctx_listing(package_listing: PackageListing) -> impl AttrCoercionContext {
    let root_cell_name = CellName::unchecked_new("root".to_owned());
    let aliases = hashmap![
        CellAlias::new("".to_owned()) => root_cell_name.clone(),
        CellAlias::new("cell1".to_owned()) => CellName::unchecked_new("cell1".to_owned()),
    ];

    let package = Package::new(
        &root_cell_name,
        CellRelativePath::unchecked_new("package/subdir"),
    );

    struct NoFunctions;
    impl QueryFunctionsExt for NoFunctions {
        fn visit_literals(
            &self,
            _visitor: &mut dyn QueryLiteralVisitor,
            _expr: &Spanned<Expr>,
        ) -> QueryResult<()> {
            panic!("not needed in tests")
        }
    }

    BuildAttrCoercionContext::new_with_package(
        CellAliasResolver::new(Arc::new(aliases)).unwrap(),
        (package, package_listing),
        false,
        box NoFunctions,
    )
}

fn cell_alias_resolver() -> CellAliasResolver {
    CellAliasResolver::new(Arc::new(HashMap::from_iter([(
        CellAlias::new("".to_owned()),
        CellName::unchecked_new("root".to_owned()),
    )])))
    .unwrap()
}

pub(crate) fn to_value<'v>(env: &'v Module, globals: &Globals, content: &str) -> Value<'v> {
    let import_path = ImportPath::unchecked_new("", "", "defs.bzl");
    let ast = AstModule::parse(
        import_path.id().as_str(),
        content.to_owned(),
        &Dialect::Extended,
    )
    .unwrap_or_else(|err| panic!("Failed parsing `{}`. Error: `{}`", content, err));
    let mut eval = Evaluator::new(env);

    let cell_info = InterpreterCellInfo::new(
        BuildFileCell::new(CellName::unchecked_new("".to_owned())),
        &LegacyBuckConfig::empty(),
        cell_alias_resolver(),
    )
    .unwrap();
    let buckconfig = LegacyBuckConfig::empty();
    let build_ctx = BuildContext::new_for_module(
        env,
        &cell_info,
        &buckconfig,
        StarlarkPath::LoadFile(&import_path),
        None,
        InterpreterHostPlatform::Linux,
        InterpreterHostArchitecture::X86_64,
        None,
        false,
    );
    eval.extra = Some(&build_ctx);

    eval.eval_module(ast, globals)
        .unwrap_or_else(|err| panic!("Failed interpreting `{}`. Error: `{}`", content, err))
}
