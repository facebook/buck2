/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::package_listing::listing::testing::PackageListingExt;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::alias::NonEmptyCellAlias;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::package::PackageLabel;
use buck2_interpreter::extra::cell_info::InterpreterCellInfo;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
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

pub fn coercion_ctx() -> impl AttrCoercionContext {
    coercion_ctx_listing(PackageListing::testing_empty())
}

pub fn coercion_ctx_listing(package_listing: PackageListing) -> impl AttrCoercionContext {
    let package = PackageLabel::testing();
    let aliases = hashmap![
        NonEmptyCellAlias::new("cell1".to_owned()).unwrap() => CellName::testing_new("cell1"),
    ];

    let cell_resolver = CellResolver::testing_with_names_and_paths_with_alias(&[
        (
            package.cell_name(),
            CellRootPathBuf::testing_new(""),
            aliases,
        ),
        (
            CellName::testing_new("cell1"),
            CellRootPathBuf::testing_new("cell1"),
            HashMap::new(),
        ),
    ]);

    BuildAttrCoercionContext::new_with_package(cell_resolver, (package, package_listing), false)
}

fn cell_resolver() -> CellResolver {
    CellResolver::testing_with_name_and_path(
        CellName::testing_new("root"),
        CellRootPathBuf::testing_new(""),
    )
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
        cell_resolver(),
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
