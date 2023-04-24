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
use buck2_core::cells::alias::CellAlias;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellAliasResolver;
use buck2_core::package::PackageLabel;
use buck2_interpreter::extra::cell_info::InterpreterCellInfo;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use maplit::hashmap;
use starlark::environment::Globals;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::syntax::AstModule;
use starlark::syntax::Dialect;
use starlark::values::Value;

use crate::attrs::coerce::ctx::BuildAttrCoercionContext;
use crate::interpreter::build_context::BuildContext;
use crate::interpreter::build_context::PerFileTypeContext;
use crate::interpreter::functions::host_info::HostInfo;

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
    let package = PackageLabel::testing();
    let aliases = hashmap![
        CellAlias::new("cell1".to_owned()) => CellName::testing_new("cell1"),
    ];

    BuildAttrCoercionContext::new_with_package(
        CellAliasResolver::new(package.cell_name(), Arc::new(aliases)).unwrap(),
        (package, package_listing),
        false,
    )
}

fn cell_alias_resolver() -> CellAliasResolver {
    CellAliasResolver::new(CellName::testing_new("root"), Arc::new(HashMap::new())).unwrap()
}

pub fn to_value<'v>(env: &'v Module, globals: &Globals, content: &str) -> Value<'v> {
    let import_path = ImportPath::testing_new("root//:defs.bzl");
    let ast = AstModule::parse(
        &import_path.to_string(),
        content.to_owned(),
        &Dialect::Extended,
    )
    .unwrap_or_else(|err| panic!("Failed parsing `{}`. Error: `{}`", content, err));
    let cell_info = InterpreterCellInfo::new(
        BuildFileCell::new(CellName::testing_new("root")),
        &LegacyBuckConfig::empty(),
        cell_alias_resolver(),
    )
    .unwrap();
    let buckconfig = LegacyBuckConfig::empty();
    let root_buckconfig = LegacyBuckConfig::empty();
    let host_platform = InterpreterHostPlatform::Linux;
    let host_architecture = InterpreterHostArchitecture::X86_64;
    let host_info = HostInfo::new(host_platform, host_architecture, None);
    let build_ctx = BuildContext::new_for_module(
        env,
        &cell_info,
        &buckconfig,
        &root_buckconfig,
        &host_info,
        PerFileTypeContext::Bzl(import_path),
        false,
    );

    let mut eval = Evaluator::new(env);
    eval.extra = Some(&build_ctx);
    eval.eval_module(ast, globals)
        .unwrap_or_else(|err| panic!("Failed interpreting `{}`. Error: `{}`", content, err))
}
