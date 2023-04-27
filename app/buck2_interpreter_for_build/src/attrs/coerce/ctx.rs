/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;

use buck2_common::package_listing::listing::PackageListing;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::package_relative_path::PackageRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::soft_error;
use buck2_core::target::label::TargetLabel;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coerced_path::CoercedDirectory;
use buck2_node::attrs::coerced_path::CoercedPath;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_query::query::syntax::simple::eval::error::QueryError;
use buck2_query::query::syntax::simple::functions::QueryLiteralVisitor;
use buck2_query_parser::spanned::Spanned;
use buck2_query_parser::Expr;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use bumpalo::Bump;
use dupe::Dupe;
use dupe::IterDupedExt;
use hashbrown::raw::RawTable;
use tracing::info;
use twox_hash::xxh3;

use super::interner::AttrCoercionInterner;
use crate::attrs::coerce::query_functions::QUERY_FUNCTIONS;

#[derive(Debug, thiserror::Error)]
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
    /// Used to resolve relative targets. This is present when a build file
    /// is being evaluated, however it is absent if an extension file is being
    /// evaluated. The latter case occurs when default values for attributes
    /// are coerced when a UDR is declared.
    enclosing_package: Option<(PackageLabel, PackageListing)>,
    /// Does this package (if present) have a package boundary exception on it.
    package_boundary_exception: bool,
    /// Allocator for `label_cache`.
    alloc: Bump,
    /// Label coercion cache. We use `RawTable` where because `HashMap` API
    /// requires either computing hash twice (for get, then for insert) or
    /// allocating a key to perform a query using `entry` API.
    /// Strings are owned by `alloc`, using bump allocator makes evaluation 0.5% faster.
    label_cache: RefCell<RawTable<(u64, *const str, ProvidersLabel)>>,
    str_interner: AttrCoercionInterner<ArcStr>,
    list_interner: AttrCoercionInterner<ArcSlice<CoercedAttr>>,
    // TODO(scottcao): Dict and selects need separate interners right now because
    // they have different key types. We can optimize this by interning keys and values
    // separately and use the same interner for dict and select values. This will also
    // reduce key duplication in selects since select keys are more likely to be deduplicated
    // than select values
    dict_interner: AttrCoercionInterner<ArcSlice<(CoercedAttr, CoercedAttr)>>,
    select_interner: AttrCoercionInterner<ArcSlice<(TargetLabel, CoercedAttr)>>,
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
        enclosing_package: Option<(PackageLabel, PackageListing)>,
        package_boundary_exception: bool,
    ) -> Self {
        Self {
            cell_resolver,
            cell_name,
            enclosing_package,
            package_boundary_exception,
            alloc: Bump::new(),
            label_cache: RefCell::new(RawTable::new()),
            str_interner: AttrCoercionInterner::new(),
            list_interner: AttrCoercionInterner::new(),
            dict_interner: AttrCoercionInterner::new(),
            select_interner: AttrCoercionInterner::new(),
        }
    }

    pub fn new_no_package(cell_resolver: CellResolver, cell_name: CellName) -> Self {
        Self::new(cell_resolver, cell_name, None, false)
    }

    pub fn new_with_package(
        cell_resolver: CellResolver,
        enclosing_package: (PackageLabel, PackageListing),
        package_boundary_exception: bool,
    ) -> Self {
        Self::new(
            cell_resolver,
            enclosing_package.0.cell_name(),
            Some(enclosing_package),
            package_boundary_exception,
        )
    }

    pub fn parse_pattern<P: PatternType>(&self, value: &str) -> anyhow::Result<ParsedPattern<P>> {
        let cell_alias_resolver = self
            .cell_resolver
            .get(self.cell_name)?
            .cell_alias_resolver();
        ParsedPattern::parsed_opt_absolute(
            cell_alias_resolver,
            self.enclosing_package.as_ref().map(|x| x.0.as_cell_path()),
            value,
        )
    }

    fn coerce_label_no_cache(&self, value: &str) -> anyhow::Result<ProvidersLabel> {
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
    ) -> anyhow::Result<&(PackageLabel, PackageListing)> {
        self.enclosing_package.as_ref().ok_or_else(|| {
            BuildAttrCoercionContextError::NotBuildFileContext(msg.to_owned()).into()
        })
    }

    fn compute_hash(s: &str) -> u64 {
        xxh3::hash64(s.as_bytes())
    }
}

impl AttrCoercionContext for BuildAttrCoercionContext {
    fn coerce_label(&self, value: &str) -> anyhow::Result<ProvidersLabel> {
        let hash = Self::compute_hash(value);
        let mut label_cache = self.label_cache.borrow_mut();

        if let Some((_h, _v, label)) = label_cache.get(hash, |(_h, v, _)| value == unsafe { &**v })
        {
            return Ok(label.clone());
        }

        let label = self.coerce_label_no_cache(value)?;
        label_cache.insert(
            hash,
            (hash, self.alloc.alloc_str(value), label.clone()),
            |(h, _v, _)| *h,
        );
        Ok(label)
    }

    fn intern_str(&self, value: &str) -> ArcStr {
        if value.is_empty() {
            return ArcStr::default();
        }

        self.str_interner.intern(value)
    }

    fn intern_list(&self, value: Vec<CoercedAttr>) -> ArcSlice<CoercedAttr> {
        self.list_interner.intern(value)
    }

    fn intern_dict(
        &self,
        value: Vec<(CoercedAttr, CoercedAttr)>,
    ) -> ArcSlice<(CoercedAttr, CoercedAttr)> {
        self.dict_interner.intern(value)
    }

    fn intern_select(
        &self,
        value: Vec<(TargetLabel, CoercedAttr)>,
    ) -> ArcSlice<(TargetLabel, CoercedAttr)> {
        self.select_interner.intern(value)
    }

    fn coerce_path(&self, value: &str, allow_directory: bool) -> anyhow::Result<CoercedPath> {
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
                    soft_error!("source_directory_includes_subpackage", e.into())?;
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
                soft_error!("source_file_missing", e.into())?;
            }

            Ok(CoercedPath::File(path.to_arc()))
        }
    }

    fn coerce_target_pattern(
        &self,
        pattern: &str,
    ) -> anyhow::Result<ParsedPattern<TargetPatternExtra>> {
        self.parse_pattern(pattern)
    }

    fn visit_query_function_literals(
        &self,
        visitor: &mut dyn QueryLiteralVisitor,
        expr: &Spanned<Expr>,
        query: &str,
    ) -> anyhow::Result<()> {
        QUERY_FUNCTIONS
            .get()?
            .visit_literals(visitor, expr)
            .map_err(|e| QueryError::convert_error(e, query))?;
        Ok(())
    }
}
