/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_common::package_listing::listing::PackageListing;
use buck2_common::package_listing::listing::testing::PackageListingExt;
use buck2_core::cells::cell_path_with_allowed_relative_dir::CellPathWithAllowedRelativeDir;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::package::PackageLabel;
use buck2_core::package::package_relative_path::PackageRelativePathBuf;
use buck2_core::plugins::PluginKindSet;
use buck2_core::target::label::interner::ConcurrentTargetLabelInterner;
use buck2_interpreter_for_build::attrs::coerce::attr_type::AttrTypeExt;
use buck2_interpreter_for_build::attrs::coerce::ctx::BuildAttrCoercionContext;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use buck2_interpreter_for_build::interpreter::testing::cells;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::hacks::value_to_string;
use buck2_node::provider_id_set::ProviderIdSet;
use dupe::Dupe;
use indoc::indoc;
use starlark::values::Heap;

#[test]
fn string_works() -> buck2_error::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.run_starlark_bzl_test(indoc!(
        r#"
        frozen = attrs.string(default="something", doc = "foo")
        def test():
            assert_eq('attrs.string(default="something")', repr(attrs.string(default="something", doc = "foo")))
            assert_eq('attrs.string(default="something")', repr(frozen))
        "#
    ))
}

#[test]
fn boolean_works() -> buck2_error::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.run_starlark_bzl_test(indoc!(
        r#"
        frozen = attrs.bool(default=False)
        def test():
            assert_eq('attrs.bool(default=True)', repr(attrs.bool(default=True, doc = "foo")))
            assert_eq('attrs.bool(default=False)', repr(frozen))
        "#
    ))
}

#[test]
fn test_attr_module_registered() -> buck2_error::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.run_starlark_bzl_test(indoc!(
        r#"
        def test():
            assert_eq(True, getattr(attrs, "string") != None)
        "#
    ))
}

#[test]
fn list_works() -> buck2_error::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.run_starlark_bzl_test(indoc!(
        r#"
        frozen = attrs.list(
            attrs.string(default = "something", doc = "foo"),
            default=["1", "2"],
            doc = "foo",
        )
        def test():
            not_frozen = attrs.list(
                attrs.string(default = "something", doc = "foo"),
                default=[],
                doc = "foo",
            )

            assert_eq('attrs.list(attrs.string(), default=[])', repr(not_frozen))
            assert_eq('attrs.list(attrs.string(), default=["1", "2"])', repr(frozen))
        "#
    ))
}

#[test]
fn enum_works() -> buck2_error::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.run_starlark_bzl_test(indoc!(
        r#"
        frozen = attrs.enum(["red", "green", "blue"])
        def test():
            not_frozen = attrs.enum(["yes", "no"], default="no")
            assert_eq('attrs.enum(["red","green","blue"])', repr(frozen))
            assert_eq('attrs.enum(["yes","no"], default="no")', repr(not_frozen))
        "#
    ))
}

#[test]
fn attr_coercer_coerces() -> buck2_error::Result<()> {
    Heap::temp(|heap| {
        let some_cells = cells(None)?;
        let cell_resolver = some_cells.1;
        let cell_alias_resolver = some_cells.0;
        let package = PackageLabel::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("foo"),
        )?;
        let enclosing_package = (package.dupe(), PackageListing::testing_empty());
        let coercer_ctx = BuildAttrCoercionContext::new_with_package(
            cell_resolver,
            cell_alias_resolver,
            enclosing_package,
            false,
            Arc::new(ConcurrentTargetLabelInterner::default()),
            CellPathWithAllowedRelativeDir::backwards_relative_not_supported(
                package.as_cell_path().to_owned(),
            ),
        );
        let label_coercer = AttrType::dep(ProviderIdSet::EMPTY, PluginKindSet::EMPTY);
        let string_coercer = AttrType::string();
        let enum_coercer = AttrType::enumeration(vec![
            "red".to_owned(),
            "green".to_owned(),
            "blue".to_owned(),
        ])?;
        assert!(AttrType::enumeration(vec!["UPPER".to_owned()]).is_err());
        assert!(
            AttrType::enumeration(vec![
                "repeated".to_owned(),
                "and".to_owned(),
                "repeated".to_owned()
            ])
            .is_err()
        );

        let label_value1 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc("root//foo:bar"),
        )?;
        let label_value2 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc("root//foo:bar[baz]"),
        )?;
        let label_value3 =
            label_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc(":bar"))?;
        let label_value4 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc(":bar[baz]"),
        )?;
        let invalid_label_value1 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc("root//foo/..."),
        );
        let invalid_label_value2 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc("root//foo:"),
        );
        let invalid_label_value3 =
            label_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("1"));

        assert_eq!(
            "root//foo:bar",
            value_to_string(&label_value1, package.dupe())?
        );
        assert_eq!(
            "root//foo:bar[baz]",
            value_to_string(&label_value2, package.dupe())?
        );
        assert_eq!(
            "root//foo:bar",
            value_to_string(&label_value3, package.dupe())?
        );
        assert_eq!(
            "root//foo:bar[baz]",
            value_to_string(&label_value4, package.dupe())?
        );
        assert!(invalid_label_value1.is_err());
        assert!(invalid_label_value2.is_err());
        assert!(invalid_label_value3.is_err());

        let string_value1 =
            string_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("str"))?;
        assert_eq!("str", value_to_string(&string_value1, package.dupe())?);

        let enum_valid1 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("red"))?;
        let enum_valid2 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("green"))?;
        let enum_valid3 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("RED"))?;
        let enum_invalid1 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("orange"));
        let enum_invalid2 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc(false));
        assert_eq!("red", value_to_string(&enum_valid1, package.dupe())?);
        assert_eq!("green", value_to_string(&enum_valid2, package.dupe())?);
        assert_eq!("red", value_to_string(&enum_valid3, package.dupe())?);
        assert!(enum_invalid1.is_err());
        assert!(enum_invalid2.is_err());

        Ok(())
    })
}

#[test]
fn dep_works() -> buck2_error::Result<()> {
    let mut t = Tester::new().unwrap();
    t.run_starlark_bzl_test(indoc!(
        r#"
        frozen1 = attrs.dep(default="root//foo:bar")
        frozen2 = attrs.dep(default="//foo:bar")
        def test():
            assert_eq('attrs.dep(default="root//foo:bar")', repr(attrs.dep(default="//foo:bar")))
            assert_eq('attrs.dep(default="root//foo:bar")', repr(frozen1))
            assert_eq('attrs.dep(default="root//foo:bar")', repr(frozen2))
        "#
    ))?;

    let mut t = Tester::new().unwrap();
    t.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            attrs.dep(default="notatarget")
        "#
        ),
        "Invalid target pattern",
    );

    // Relative targets are disallowed; there is no build file for them to be relative to
    let mut t = Tester::new().unwrap();
    t.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            attrs.dep(default=":reltarget")
        "#
        ),
        "Must be absolute",
    );
    Ok(())
}

#[test]
fn source_works() -> buck2_error::Result<()> {
    let mut t = Tester::new().unwrap();
    t.run_starlark_bzl_test(indoc!(
        r#"
        frozen1 = attrs.source(default="root//foo:bar")
        frozen2 = attrs.source(default="//foo:bar")
        def test():
            assert_eq('attrs.source(default="root//foo:bar")', repr(attrs.source(default="root//foo:bar")))
            assert_eq('attrs.source(default="root//foo:bar")', repr(frozen1))
            assert_eq('attrs.source(default="root//foo:bar")', repr(frozen2))
        "#
    ))?;

    // Relative targets are disallowed; there is no build file for them to be relative to
    let mut t = Tester::new().unwrap();
    t.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            attrs.source(default=":reltarget")
        "#
        ),
        "Must be absolute",
    );
    Ok(())
}

#[test]
fn coercing_src_to_path_works() -> buck2_error::Result<()> {
    let cell_resolver = cells(None).unwrap().1;
    let cell_alias_resolver = cells(None).unwrap().0;
    let package = PackageLabel::new(
        CellName::testing_new("root"),
        CellRelativePath::unchecked_new("foo/bar"),
    )?;
    let package_ctx = BuildAttrCoercionContext::new_with_package(
        cell_resolver.dupe(),
        cell_alias_resolver.dupe(),
        (
            package.dupe(),
            PackageListing::testing_files(&["baz/quz.cpp"]),
        ),
        false,
        Arc::new(ConcurrentTargetLabelInterner::default()),
        CellPathWithAllowedRelativeDir::backwards_relative_not_supported(
            package.as_cell_path().to_owned(),
        ),
    );
    let no_package_ctx = BuildAttrCoercionContext::new_no_package(
        cell_resolver,
        CellName::testing_new("root"),
        cell_alias_resolver,
        Arc::new(ConcurrentTargetLabelInterner::default()),
    );

    let err = no_package_ctx
        .coerce_path("baz/quz.cpp", false)
        .unwrap_err();
    assert!(err.to_string().contains("Expected a package"));

    let err = package_ctx
        .coerce_path("/invalid/absolute/path", false)
        .unwrap_err();
    assert!(format!("{err:#}").contains("absolute path"), "{err:?}");

    let err = package_ctx
        .coerce_path("../upward/traversal", false)
        .unwrap_err();
    assert!(err.to_string().contains("normalized path"));

    let expected = PackageRelativePathBuf::unchecked_new("baz/quz.cpp".to_owned());
    assert_eq!(
        expected.as_path(),
        &**package_ctx
            .coerce_path("baz/quz.cpp", false)
            .unwrap()
            .path()
    );
    Ok(())
}
