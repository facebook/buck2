/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;

use buck2_common::package_listing::listing::PackageListing;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path_with_allowed_relative_dir::CellPathWithAllowedRelativeDir;
use buck2_core::cells::name::CellName;
use buck2_core::package::PackageLabel;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::package_relative_path::PackageRelativePathBuf;
use buck2_core::pattern::pattern::InferTargetNames;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern::TargetParsingRel;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::soft_error;
use buck2_node::attrs::arena::CoercedAttrArenaBuilder;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coerced_path::CoercedDirectory;
use buck2_node::attrs::coerced_path::CoercedPath;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::coercion_context::CoerceMemoKey;
use buck2_node::attrs::coercion_context::CoerceMemoTable;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use buck2_node::query::query_functions::CONFIGURED_GRAPH_QUERY_FUNCTIONS;
use buck2_query::query::syntax::simple::eval::error::QueryError;
use buck2_query::query::syntax::simple::functions::QueryLiteralVisitor;
use buck2_query_parser::Expr;
use buck2_query_parser::spanned::Spanned;
use buck2_util::arc_str::ArcStr;
use bumpalo::Bump;
use dupe::Dupe;
use dupe::IterDupedExt;
use hashbrown::HashTable;
use hashbrown::hash_table;
use tracing::info;

use crate::attrs::coerce::arc_str_interner::ArcStrInterner;
use crate::attrs::coerce::str_hash::str_hash;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum BuildAttrCoercionContextError {
    #[error("Expected a label, got the pattern `{0}`.")]
    RequiredLabel(String),
    #[error("Expected a package: `{0}` can only be specified in a build file.")]
    NotBuildFileContext(String),
    #[error("Expected file, but got a directory for path `{1}` in package `{0}`.")]
    SourceFileIsDirectory(PackageLabel, String),
    #[error("Source file `{1}` does not exist as a member of package `{0}`.")]
    SourceFileMissing(PackageLabel, String),
    #[error(
        "Directory `{1}` of package `{0}` may not cover any subpackages, but includes subpackage `{2}`."
    )]
    SourceDirectoryIncludesSubPackage(PackageLabel, String, PackageRelativePathBuf),
}

/// An incomplete attr coercion context. Will be replaced with a real one later.
pub struct BuildAttrCoercionContext {
    /// Used to coerce targets
    cell_resolver: CellResolver,
    cell_name: CellName,
    cell_alias_resolver: CellAliasResolver,
    /// Used to resolve relative targets. This is present when a build file
    /// is being evaluated, however it is absent if an extension file is being
    /// evaluated. The latter case occurs when default values for attributes
    /// are coerced when a UDR is declared.
    enclosing_package: Option<(PackageLabel, PackageListing)>,
    /// This defines the limited scope in which we allow parsing patterns beginning with `../`
    current_dir_with_allowed_relative_dirs: CellPathWithAllowedRelativeDir,
    /// Does this package (if present) have a package boundary exception on it.
    package_boundary_exception: bool,
    /// Whether to infer a target name when a pattern does not provide one,
    /// making `//foo/bar` equivalent to `//foo/bar:bar`. Controlled by the
    /// `buck2.infer_target_names` buckconfig.
    infer_target_names: InferTargetNames,
    /// Allocator for `label_cache`.
    alloc: Bump,
    /// Label coercion cache. We use `RawTable` where because `HashMap` API
    /// requires either computing hash twice (for get, then for insert) or
    /// allocating a key to perform a query using `entry` API.
    /// Strings are owned by `alloc`, using bump allocator makes evaluation 0.5% faster.
    label_cache: RefCell<HashTable<(u64, *const str, ProvidersLabel)>>,
    /// Interns strings embedded as `ArcStr` fields inside composite values
    /// (macro `Arg` parts, `select_fail`/`select_incompatible` messages),
    /// which have no arena node of their own.
    str_interner: ArcStrInterner,
    /// The node constructors (`build_*_attr`) encode attribute values into
    /// this arena as coercion proceeds. Package contexts seal it into the
    /// package's shared storage; bzl contexts (rule defaults) seal it into
    /// the value's own storage.
    attr_arena_builder: RefCell<CoercedAttrArenaBuilder>,
}

impl Debug for BuildAttrCoercionContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BuildAttrCoercionContext")
            .finish_non_exhaustive()
    }
}

impl BuildAttrCoercionContext {
    fn new(
        cell_resolver: CellResolver,
        cell_name: CellName,
        cell_alias_resolver: CellAliasResolver,
        enclosing_package: Option<(PackageLabel, PackageListing)>,
        package_boundary_exception: bool,
        current_dir_with_allowed_relative_dirs: CellPathWithAllowedRelativeDir,
        infer_target_names: InferTargetNames,
    ) -> Self {
        Self {
            cell_resolver,
            cell_name,
            cell_alias_resolver,
            enclosing_package,
            current_dir_with_allowed_relative_dirs,
            package_boundary_exception,
            infer_target_names,
            alloc: Bump::new(),
            label_cache: RefCell::new(HashTable::new()),
            str_interner: ArcStrInterner::new(),
            attr_arena_builder: RefCell::new(CoercedAttrArenaBuilder::default()),
        }
    }

    pub(crate) fn with_attr_arena_builder<R>(
        &self,
        f: impl FnOnce(&mut CoercedAttrArenaBuilder) -> R,
    ) -> R {
        f(&mut self.attr_arena_builder.borrow_mut())
    }

    pub(crate) fn with_attr_arena_ref<R>(
        &self,
        f: impl FnOnce(&CoercedAttrArenaBuilder) -> R,
    ) -> R {
        f(&self.attr_arena_builder.borrow())
    }

    pub(crate) fn take_attr_arena_builder(self) -> CoercedAttrArenaBuilder {
        self.attr_arena_builder.into_inner()
    }

    pub fn new_no_package(
        cell_resolver: CellResolver,
        cell_name: CellName,
        cell_alias_resolver: CellAliasResolver,
        infer_target_names: InferTargetNames,
    ) -> Self {
        Self::new(
            cell_resolver,
            cell_name,
            cell_alias_resolver,
            None,
            false,
            CellPathWithAllowedRelativeDir::backwards_relative_not_supported(CellPath::empty(
                cell_name,
            )),
            infer_target_names,
        )
    }

    pub fn new_with_package(
        cell_resolver: CellResolver,
        cell_alias_resolver: CellAliasResolver,
        enclosing_package: (PackageLabel, PackageListing),
        package_boundary_exception: bool,
        current_dir_with_allowed_relative_dirs: CellPathWithAllowedRelativeDir,
        infer_target_names: InferTargetNames,
    ) -> Self {
        Self::new(
            cell_resolver,
            enclosing_package.0.cell_name(),
            cell_alias_resolver,
            Some(enclosing_package),
            package_boundary_exception,
            current_dir_with_allowed_relative_dirs,
            infer_target_names,
        )
    }

    pub fn parse_pattern<P: PatternType>(
        &self,
        value: &str,
    ) -> buck2_error::Result<ParsedPattern<P>> {
        let target_parsing_rel = match self.enclosing_package.as_ref().map(|x| x.0.as_cell_path()) {
            Some(package) => {
                if self
                    .current_dir_with_allowed_relative_dirs
                    .has_allowed_relative_dir()
                {
                    TargetParsingRel::AllowRelative(
                        &self.current_dir_with_allowed_relative_dirs,
                        None,
                    )
                } else {
                    TargetParsingRel::AllowLimitedRelative(package)
                }
            }
            None => TargetParsingRel::RequireAbsolute(self.cell_name),
        };
        ParsedPattern::parse_not_relaxed(
            value,
            target_parsing_rel,
            &self.cell_resolver,
            &self.cell_alias_resolver,
            self.infer_target_names,
        )
    }

    fn coerce_label_no_cache(&self, value: &str) -> buck2_error::Result<ProvidersLabel> {
        // TODO(nmj): Make this take an import path / package
        match self.parse_pattern::<ProvidersPatternExtra>(value)? {
            ParsedPattern::Target(package, target_name, providers) => {
                Ok(providers.into_providers_label(package, target_name.as_ref()))
            }
            _ => Err(BuildAttrCoercionContextError::RequiredLabel(value.to_owned()).into()),
        }
    }

    fn require_enclosing_package(
        &self,
        msg: &str,
    ) -> buck2_error::Result<&(PackageLabel, PackageListing)> {
        self.enclosing_package.as_ref().ok_or_else(|| {
            BuildAttrCoercionContextError::NotBuildFileContext(msg.to_owned()).into()
        })
    }
}

impl<'v> AttrCoercionContext<'v> for BuildAttrCoercionContext {
    fn coerce_providers_label(&self, value: &str) -> buck2_error::Result<ProvidersLabel> {
        let hash = str_hash(value);
        let mut label_cache = self.label_cache.borrow_mut();

        match label_cache.entry(
            hash,
            |(h, v, _)| *h == hash && value == unsafe { &**v },
            |(h, _, _)| *h,
        ) {
            hash_table::Entry::Occupied(e) => Ok(e.get().2.dupe()),
            hash_table::Entry::Vacant(e) => {
                let label = self.coerce_label_no_cache(value)?;

                e.insert((hash, self.alloc.alloc_str(value), label.dupe()));
                Ok(label)
            }
        }
    }

    fn build_string_attr(&self, value: &str) -> buck2_error::Result<CoercedAttr> {
        self.attr_arena_builder.borrow_mut().encode_string(value)
    }

    fn build_list_attr(&self, items: Vec<CoercedAttr>) -> buck2_error::Result<CoercedAttr> {
        self.attr_arena_builder.borrow_mut().encode_list(&items)
    }

    fn build_tuple_attr(&self, items: Vec<CoercedAttr>) -> buck2_error::Result<CoercedAttr> {
        self.attr_arena_builder.borrow_mut().encode_tuple(&items)
    }

    fn build_dict_attr(
        &self,
        pairs: Vec<(CoercedAttr, CoercedAttr)>,
    ) -> buck2_error::Result<CoercedAttr> {
        self.attr_arena_builder.borrow_mut().encode_dict(&pairs)
    }

    fn build_one_of_attr(&self, inner: CoercedAttr, idx: u32) -> buck2_error::Result<CoercedAttr> {
        self.attr_arena_builder
            .borrow_mut()
            .encode_one_of(idx, &inner)
    }

    fn build_selector_attr(
        &self,
        entries: Vec<(ConfigurationSettingKey, CoercedAttr)>,
        default: Option<CoercedAttr>,
    ) -> buck2_error::Result<CoercedAttr> {
        self.attr_arena_builder
            .borrow_mut()
            .encode_selector(&entries, default.as_ref())
    }

    fn intern_str(&self, value: &str) -> ArcStr {
        self.str_interner.intern(value)
    }

    fn coerce_path(&self, value: &str, allow_directory: bool) -> buck2_error::Result<CoercedPath> {
        let path = <&PackageRelativePath>::try_from(value)?;
        let (package, listing) = self.require_enclosing_package(value)?;

        if let Some(path) = listing.get_file(path) {
            return Ok(CoercedPath::File(path));
        }

        // TODO: Make the warnings below into errors
        if let Some(path) = listing.get_dir(path) {
            if !allow_directory {
                return Err(BuildAttrCoercionContextError::SourceFileIsDirectory(
                    package.dupe(),
                    value.to_owned(),
                )
                .into());
            } else if let Some(subpackage) = listing.subpackages_within(&path).next() {
                let e = BuildAttrCoercionContextError::SourceDirectoryIncludesSubPackage(
                    package.dupe(),
                    value.to_owned(),
                    subpackage.to_owned(),
                );
                if self.package_boundary_exception {
                    info!("{} (could be due to a package boundary violation)", e);
                } else {
                    soft_error!("source_directory_includes_subpackage", e.into(), error_on_oss: true)?;
                }
            }
            let files = listing.files_within(&path).duped().collect();
            Ok(CoercedPath::Directory(Box::new(CoercedDirectory {
                dir: path,
                files,
            })))
        } else {
            let e =
                BuildAttrCoercionContextError::SourceFileMissing(package.dupe(), value.to_owned());
            if self.package_boundary_exception {
                info!("{} (could be due to a package boundary violation)", e);
            } else {
                soft_error!("source_file_missing", e.into(), quiet: true, error_on_oss: true)?;
            }

            Ok(CoercedPath::File(path.to_arc()))
        }
    }

    fn coerce_target_pattern(
        &self,
        pattern: &str,
    ) -> buck2_error::Result<ParsedPattern<TargetPatternExtra>> {
        self.parse_pattern(pattern)
    }

    fn visit_query_function_literals<'q>(
        &self,
        visitor: &mut dyn QueryLiteralVisitor<'q>,
        expr: &Spanned<Expr<'q>>,
        query: &'q str,
    ) -> buck2_error::Result<()> {
        CONFIGURED_GRAPH_QUERY_FUNCTIONS
            .get()?
            .visit_literals(visitor, expr)
            .map_err(|e| QueryError::convert_error(e, query))?;
        Ok(())
    }
}

/// Coercion context for one buildfile evaluation: delegates to the package's
/// `BuildAttrCoercionContext` and memoizes per-value results in the module's
/// heap-scoped table, which is what proves the memo cannot outlive the
/// values it keys.
pub struct MemoizedCoercionCtx<'c, 'v> {
    pub base: &'c BuildAttrCoercionContext,
    pub memo: &'v CoerceMemoTable<'v>,
}

impl<'c, 'v> AttrCoercionContext<'v> for MemoizedCoercionCtx<'c, 'v> {
    fn coerce_providers_label(&self, value: &str) -> buck2_error::Result<ProvidersLabel> {
        AttrCoercionContext::coerce_providers_label(self.base, value)
    }

    fn coerce_memo_lookup(&self, key: CoerceMemoKey<'v>) -> Option<CoercedAttr> {
        self.memo.lookup(key)
    }

    fn coerce_memo_insert(&self, key: CoerceMemoKey<'v>, value: &CoercedAttr) {
        self.memo.insert(key, value);
    }

    fn build_string_attr(&self, value: &str) -> buck2_error::Result<CoercedAttr> {
        AttrCoercionContext::build_string_attr(self.base, value)
    }

    fn build_list_attr(&self, items: Vec<CoercedAttr>) -> buck2_error::Result<CoercedAttr> {
        AttrCoercionContext::build_list_attr(self.base, items)
    }

    fn build_tuple_attr(&self, items: Vec<CoercedAttr>) -> buck2_error::Result<CoercedAttr> {
        AttrCoercionContext::build_tuple_attr(self.base, items)
    }

    fn build_dict_attr(
        &self,
        pairs: Vec<(CoercedAttr, CoercedAttr)>,
    ) -> buck2_error::Result<CoercedAttr> {
        AttrCoercionContext::build_dict_attr(self.base, pairs)
    }

    fn build_one_of_attr(&self, inner: CoercedAttr, idx: u32) -> buck2_error::Result<CoercedAttr> {
        AttrCoercionContext::build_one_of_attr(self.base, inner, idx)
    }

    fn build_selector_attr(
        &self,
        entries: Vec<(ConfigurationSettingKey, CoercedAttr)>,
        default: Option<CoercedAttr>,
    ) -> buck2_error::Result<CoercedAttr> {
        AttrCoercionContext::build_selector_attr(self.base, entries, default)
    }

    fn intern_str(&self, value: &str) -> ArcStr {
        AttrCoercionContext::intern_str(self.base, value)
    }

    fn coerce_path(&self, value: &str, allow_directory: bool) -> buck2_error::Result<CoercedPath> {
        AttrCoercionContext::coerce_path(self.base, value, allow_directory)
    }

    fn coerce_target_pattern(
        &self,
        pattern: &str,
    ) -> buck2_error::Result<ParsedPattern<TargetPatternExtra>> {
        AttrCoercionContext::coerce_target_pattern(self.base, pattern)
    }

    fn visit_query_function_literals<'q>(
        &self,
        visitor: &mut dyn QueryLiteralVisitor<'q>,
        expr: &Spanned<Expr<'q>>,
        query: &'q str,
    ) -> buck2_error::Result<()> {
        AttrCoercionContext::visit_query_function_literals(self.base, visitor, expr, query)
    }
}

#[cfg(test)]
mod tests {
    use buck2_common::package_listing::listing::PackageListing;
    use buck2_common::package_listing::listing::testing::PackageListingExt;
    use buck2_core::provider::label::ProvidersLabel;
    use buck2_core::provider::label::testing::ProvidersLabelTestExt;
    use buck2_node::attrs::attr_type::bool::BoolLiteral;
    use buck2_node::attrs::coerced_attr::CoercedAttr;
    use buck2_node::attrs::coercion_context::AttrCoercionContext;
    use buck2_node::configuration::resolved::ConfigurationSettingKey;

    use crate::attrs::coerce::ctx::BuildAttrCoercionContext;
    use crate::attrs::coerce::testing::EnumCoercionCtx;
    use crate::attrs::coerce::testing::build_coercion_ctx;

    fn decode(ctx: &BuildAttrCoercionContext, attr: CoercedAttr) -> CoercedAttr {
        match attr {
            CoercedAttr::Encoded(r, _) => {
                ctx.with_attr_arena_ref(|b| b.view(r).to_coerced()).unwrap()
            }
            other => panic!("arena context should return `Encoded`, got `{other:?}`"),
        }
    }

    fn key(name: &str) -> ConfigurationSettingKey {
        ConfigurationSettingKey(ProvidersLabel::testing_new("root", "cfg", name, None))
    }

    // Children built through each context's own path: the arena side
    // exercises `Encoded` child refs inside every container.
    fn items<'v>(ctx: &impl AttrCoercionContext<'v>) -> Vec<CoercedAttr> {
        vec![
            ctx.build_string_attr("a").unwrap(),
            CoercedAttr::Int(7),
            CoercedAttr::Bool(BoolLiteral(true)),
        ]
    }

    fn pairs<'v>(ctx: &impl AttrCoercionContext<'v>) -> Vec<(CoercedAttr, CoercedAttr)> {
        vec![(ctx.build_string_attr("k").unwrap(), CoercedAttr::Int(3))]
    }

    fn entries<'v>(
        ctx: &impl AttrCoercionContext<'v>,
    ) -> Vec<(ConfigurationSettingKey, CoercedAttr)> {
        vec![
            (key("a"), ctx.build_string_attr("on-a").unwrap()),
            (key("b"), CoercedAttr::None),
        ]
    }

    /// The arena constructor overrides must encode the same value the
    /// trait's default (plain-enum) constructors produce, including with
    /// already-`Encoded` children.
    #[test]
    fn test_arena_constructors_match_enum_path() {
        let plain = EnumCoercionCtx(build_coercion_ctx(PackageListing::testing_empty()));
        let arena = build_coercion_ctx(PackageListing::testing_empty());

        let expect = plain.build_string_attr("foo").unwrap();
        let got = arena.build_string_attr("foo").unwrap();
        assert_eq!(expect, decode(&arena, got), "string constructor differs");

        let expect = plain.build_list_attr(items(&plain)).unwrap();
        let got = AttrCoercionContext::build_list_attr(&arena, items(&arena)).unwrap();
        assert_eq!(expect, decode(&arena, got), "list constructor differs");

        let expect = plain.build_tuple_attr(items(&plain)).unwrap();
        let got = AttrCoercionContext::build_tuple_attr(&arena, items(&arena)).unwrap();
        assert_eq!(expect, decode(&arena, got), "tuple constructor differs");

        let expect = plain.build_dict_attr(pairs(&plain)).unwrap();
        let got = AttrCoercionContext::build_dict_attr(&arena, pairs(&arena)).unwrap();
        assert_eq!(expect, decode(&arena, got), "dict constructor differs");

        let expect = plain
            .build_one_of_attr(plain.build_string_attr("variant").unwrap(), 2)
            .unwrap();
        let got = AttrCoercionContext::build_one_of_attr(
            &arena,
            AttrCoercionContext::build_string_attr(&arena, "variant").unwrap(),
            2,
        )
        .unwrap();
        assert_eq!(expect, decode(&arena, got), "one_of constructor differs");

        for default in [None, Some(CoercedAttr::Int(9))] {
            let expect = plain
                .build_selector_attr(entries(&plain), default.clone())
                .unwrap();
            let got =
                AttrCoercionContext::build_selector_attr(&arena, entries(&arena), default).unwrap();
            assert_eq!(expect, decode(&arena, got), "selector constructor differs");
        }
    }
}
