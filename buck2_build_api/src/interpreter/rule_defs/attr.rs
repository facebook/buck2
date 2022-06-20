/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    cell::RefCell,
    fmt::{self, Display},
    hash::Hash,
    sync::Arc,
};

use anyhow::Context;
use buck2_core::{
    buck_path::BuckPath,
    cells::CellAliasResolver,
    package::{Package, PackageRelativePathBuf},
    provider::ProvidersLabel,
    soft_error,
    target::TargetLabel,
};
use buck2_interpreter::{
    extra::BuildContext,
    package_listing::listing::PackageListing,
    pattern::{ParsedPattern, PatternType, ProvidersPattern},
};
use bumpalo::Bump;
use gazebo::{any::ProvidesStaticType, prelude::*};
use hashbrown::raw::RawTable;
use starlark::{
    environment::GlobalsBuilder,
    eval::Evaluator,
    starlark_type,
    values::{
        docs::{DocString, DocStringKind},
        none::NoneType,
        NoSerialize, StarlarkValue, Value, ValueError,
    },
};
use thiserror::Error;
use tracing::{error, info};
use twox_hash::xxh3;

use crate::{
    attrs::{
        attr_type::{any::AnyAttrType, AttrType},
        coerced_attr::CoercedAttr,
        AttrCoercionContext, CoercedPath,
    },
    interpreter::rule_defs::{
        provider::{callable::ValueAsProviderCallableLike, ProviderId},
        rule::RuleError,
        transition::starlark::Transition,
    },
};

const OPTION_NONE_EXPLANATION: &str = "`None` as an attribute value always picks the default. For `attr.option`, if the default isn't `None`, there is no way to express `None`.";

#[derive(Error, Debug)]
enum AttrError {
    #[error("Expected a package: `{0}` can only be specified in a build file.")]
    NotBuildFileContext(String),
    #[error("Expected a label, got the pattern `{0}`.")]
    RequiredLabel(String),
    #[error("Source file `{1}` does not exist as a member of package `{0}`.")]
    SourceFileMissing(Package, String),
    #[error("Expected file, but got a directory for path `{1}` in package `{0}`.")]
    SourceFileIsDirectory(Package, String),
    #[error(
        "Directory `{1}` of package `{0}` may not cover any subpackages, but includes subpackage `{2}`."
    )]
    SourceDirectoryIncludesSubPackage(Package, String, PackageRelativePathBuf),
    #[error(
        "`attr.option` `default` parameter must be `None` or absent, got `{0}`.\n{}",
        OPTION_NONE_EXPLANATION
    )]
    OptionDefaultNone(String),
}

/// An incomplete attr coercion context. Will be replaced with a real one later.
pub struct BuildAttrCoercionContext {
    /// Used to coerce targets
    cell_alias_resolver: CellAliasResolver,
    /// Used to resolve relative targets. This is present when a build file
    /// is being evaluated, however it is absent if an extension file is being
    /// evaluated. The latter case occurs when default values for attributes
    /// are coerced when a UDR is declared.
    enclosing_package: Option<(Package, PackageListing)>,
    /// Does this package (if present) have a package boundary exception on it.
    package_boundary_exception: bool,
    /// Allocator for `label_cache`.
    alloc: Bump,
    /// Label coercion cache. We use `RawTable` where because `HashMap` API
    /// requires either computing hash twice (for get, then for insert) or
    /// allocating a key to perform a query using `entry` API.
    /// Strings are owned by `alloc`, using bump allocator makes evaluation 0.5% faster.
    label_cache: RefCell<RawTable<(u64, *const str, ProvidersLabel)>>,
}

impl BuildAttrCoercionContext {
    fn new(
        cell_alias_resolver: CellAliasResolver,
        enclosing_package: Option<(Package, PackageListing)>,
        package_boundary_exception: bool,
    ) -> Self {
        Self {
            cell_alias_resolver,
            enclosing_package,
            package_boundary_exception,
            alloc: Bump::new(),
            label_cache: RefCell::new(RawTable::new()),
        }
    }

    pub fn new_no_package(cell_alias_resolver: CellAliasResolver) -> Self {
        Self::new(cell_alias_resolver, None, false)
    }

    pub fn new_with_package(
        cell_alias_resolver: CellAliasResolver,
        enclosing_package: (Package, PackageListing),
        package_boundary_exception: bool,
    ) -> Self {
        Self::new(
            cell_alias_resolver,
            Some(enclosing_package),
            package_boundary_exception,
        )
    }

    pub(crate) fn parse_pattern<P: PatternType>(
        &self,
        value: &str,
    ) -> anyhow::Result<ParsedPattern<P>> {
        ParsedPattern::parsed_opt_absolute(
            &self.cell_alias_resolver,
            self.enclosing_package.as_ref().map(|x| &x.0),
            value,
        )
    }

    fn coerce_label_no_cache(&self, value: &str) -> anyhow::Result<ProvidersLabel> {
        // TODO(nmj): Make this take an import path / package
        match self.parse_pattern::<ProvidersPattern>(value)? {
            ParsedPattern::Target(package, (target_name, providers_name)) => Ok(
                ProvidersLabel::new(TargetLabel::new(package, target_name), providers_name),
            ),
            _ => Err(AttrError::RequiredLabel(value.to_owned()).into()),
        }
    }

    fn require_enclosing_package(&self, msg: &str) -> anyhow::Result<&(Package, PackageListing)> {
        self.enclosing_package
            .as_ref()
            .ok_or_else(|| AttrError::NotBuildFileContext(msg.to_owned()).into())
    }
}

impl AttrCoercionContext for BuildAttrCoercionContext {
    fn coerce_label(&self, value: &str) -> anyhow::Result<ProvidersLabel> {
        fn compute_hash(s: &str) -> u64 {
            xxh3::hash64(s.as_bytes())
        }

        let hash = compute_hash(value);
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

    fn coerce_path(&self, value: &str, allow_directory: bool) -> anyhow::Result<CoercedPath> {
        let path = PackageRelativePathBuf::try_from(value.to_owned())?;
        let (package, listing) = self.require_enclosing_package(value)?;

        // TODO: Make the warnings below into errors
        if !listing.contains_file(&path) {
            if listing.contains_dir(&path) {
                if !allow_directory {
                    let e = AttrError::SourceFileIsDirectory(package.dupe(), value.to_owned());
                    soft_error!(e.into())?;
                } else if let Some(subpackage) = listing.subpackages_within(&path).next() {
                    let e = AttrError::SourceDirectoryIncludesSubPackage(
                        package.dupe(),
                        value.to_owned(),
                        subpackage.to_owned(),
                    );
                    if self.package_boundary_exception {
                        info!("{} (could be due to a package boundary violation)", e);
                    } else {
                        soft_error!(e.into())?;
                    }
                }
                let files = listing
                    .files_within(&path)
                    .map(|x| BuckPath::new(package.dupe(), x.to_owned()))
                    .collect();
                return Ok(CoercedPath::Directory(
                    BuckPath::new(package.dupe(), path),
                    files,
                ));
            } else {
                let e = AttrError::SourceFileMissing(package.dupe(), value.to_owned());
                if self.package_boundary_exception {
                    info!("{} (could be due to a package boundary violation)", e);
                } else {
                    soft_error!(e.into())?;
                }
            }
        }

        Ok(CoercedPath::File(BuckPath::new(package.dupe(), path)))
    }
}

/// Can we use `select` with this attribute?
#[derive(Copy, Clone, Dupe)]
pub enum AttrIsConfigurable {
    Yes,
    No,
}

/// Starlark compatible container for results from e.g. `attr.string()`
#[derive(Clone, Debug, Eq, PartialEq, Hash, ProvidesStaticType, NoSerialize)]
pub struct Attribute {
    /// The default value. If None, the value is not optional and must be provided by the user
    pub(crate) default: Option<Arc<CoercedAttr>>,
    /// Documentation for what the attribute actually means
    doc: String,
    /// The coercer to take this parameter's value from Starlark value -> an
    /// internal representation
    coercer: AttrType,
}

impl Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.coercer
            .fmt_with_default(f, self.default.as_ref().map(|x| x.to_string()).as_deref())
    }
}

starlark_simple_value!(Attribute);

/// Attribute which may be either a custom value supplied by the user, or missing/None to indicate use the default.
pub(crate) enum CoercedValue {
    Custom(CoercedAttr),
    Default,
}

impl Attribute {
    pub(crate) fn new_internal(
        default: Option<Arc<CoercedAttr>>,
        doc: String,
        coercer: AttrType,
    ) -> Self {
        Self {
            default,
            doc,
            coercer,
        }
    }

    /// Helper to create an attribute from attr.foo functions
    fn attr<'v>(
        eval: &mut Evaluator<'v, '_>,
        default: Option<Value<'v>>,
        doc: &str,
        coercer: AttrType,
    ) -> anyhow::Result<Self> {
        let default = match default {
            None => None,
            Some(x) => Some(Arc::new(
                coercer
                    .coerce(
                        AttrIsConfigurable::Yes,
                        &get_attr_coercion_context(eval)?,
                        x,
                    )
                    .map_err(|_| ValueError::IncorrectParameterType)?,
            )),
        };
        Ok(Self {
            default,
            doc: doc.to_owned(),
            coercer,
        })
    }

    /// An `attr` which is not allowed to have a default as a relative label.
    fn check_not_relative_label<'v>(default: Option<Value<'v>>, attr: &str) -> anyhow::Result<()> {
        if let Some(as_str) = default.and_then(|x| x.unpack_str()) {
            if ProvidersLabel::maybe_relative_label(as_str) {
                return Err(DepError::DepRelativeDefault {
                    invalid_label: as_str.to_owned(),
                    attr: attr.to_owned(),
                }
                .into());
            }
        }
        Ok(())
    }

    /// Attempt to coerce a value. If the value provided is `None`, and a default value is available,
    /// that default value is returned.
    pub(crate) fn coerce<'v>(
        &self,
        param_name: &str,
        configurable: AttrIsConfigurable,
        coercer_ctx: &dyn AttrCoercionContext,
        value: Option<Value<'v>>,
    ) -> anyhow::Result<CoercedValue> {
        match (&self.default, value) {
            (default, Some(value)) if !value.is_none() => self
                .coercer
                .coerce_with_default(
                    configurable,
                    coercer_ctx,
                    value,
                    default.as_ref().map(|x| &**x),
                )
                .map(CoercedValue::Custom)
                .with_context(|| format!("when coercing attribute `{}`", param_name)),
            (Some(_), _) => Ok(CoercedValue::Default),
            (None, _) => Err(RuleError::MissingMandatoryParameter(param_name.to_owned()).into()),
        }
    }

    pub fn docstring(&self) -> Option<DocString> {
        DocString::from_docstring(DocStringKind::Starlark, &self.doc)
    }

    pub fn starlark_type(&self) -> String {
        self.coercer.starlark_type()
    }
}

impl<'v> StarlarkValue<'v> for Attribute {
    starlark_type!("attribute");
}

/// Grab a new coercion context object based on the main build file that is being evaluated.
/// This is used because we do not have access to a specific shared instance via ctx.extra
/// when evaluating .bzl files
pub(crate) fn get_attr_coercion_context<'v>(
    eval: &Evaluator<'v, '_>,
) -> anyhow::Result<BuildAttrCoercionContext> {
    Ok(BuildAttrCoercionContext::new_no_package(
        BuildContext::from_context(eval)?
            .cell_info()
            .cell_alias_resolver()
            .dupe(),
    ))
}

fn attr_any<'v>(doc: &'v str) -> Attribute {
    let coercer = AttrType::any();

    Attribute {
        default: Some(Arc::new(AnyAttrType::empty_string())),
        doc: doc.to_owned(),
        coercer,
    }
}

#[derive(Debug, thiserror::Error)]
enum DepError {
    #[error(
        "relative labels ('{invalid_label}') are not permitted as default values for `{attr}` \
        attributes. Use a fully qualified one like //foo:bar"
    )]
    DepRelativeDefault { invalid_label: String, attr: String },
}

/// Common code to handle `providers` argument of dep-like attrs.
fn dep_like_attr_handle_providers_arg(
    providers: Vec<Value>,
) -> anyhow::Result<Vec<Arc<ProviderId>>> {
    providers.try_map(|v| match v.as_provider_callable() {
        Some(callable) => callable.require_id(),
        None => Err(ValueError::IncorrectParameterTypeNamed("providers".to_owned()).into()),
    })
}

#[starlark_module]
pub(crate) fn attr_module(registry: &mut GlobalsBuilder) {
    fn string<'v>(
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[allow(unused_variables)]
        #[starlark(require = named)]
        validate: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::attr(eval, default, doc, AttrType::string())
    }

    fn list<'v>(
        #[starlark(require = pos)] inner: &Attribute,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        let coercer = AttrType::list(inner.coercer.dupe());
        Attribute::attr(eval, default, doc, coercer)
    }

    fn exec_dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::check_not_relative_label(default, "attr.exec_dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let coercer = AttrType::exec_dep(required_providers);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn transition_dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        cfg: Value<'v>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::check_not_relative_label(default, "attr.transition_dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let transition_id = Transition::id_from_value(cfg)?;
        let coercer = AttrType::transition_dep(required_providers, transition_id);

        let coerced_default = match default {
            None => None,
            Some(default) => {
                match coercer.coerce(
                    AttrIsConfigurable::Yes,
                    &get_attr_coercion_context(eval)?,
                    default,
                ) {
                    Ok(coerced_default) => (Some(coerced_default)),
                    Err(_) => return Err(ValueError::IncorrectParameterType.into()),
                }
            }
        };

        Ok(Attribute {
            default: coerced_default.map(Arc::new),
            doc: doc.to_owned(),
            coercer,
        })
    }

    fn configured_dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::check_not_relative_label(default, "attr.configured_dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let coercer = AttrType::configured_dep(required_providers);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn split_transition_dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        cfg: Value<'v>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::check_not_relative_label(default, "attr.split_transition_dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let transition_id = Transition::id_from_value(cfg)?;
        let coercer = AttrType::split_transition_dep(required_providers, transition_id);

        let coerced_default = match default {
            None => None,
            Some(default) => {
                match coercer.coerce(
                    AttrIsConfigurable::Yes,
                    &get_attr_coercion_context(eval)?,
                    default,
                ) {
                    Ok(coerced_default) => (Some(coerced_default)),
                    Err(_) => return Err(ValueError::IncorrectParameterType.into()),
                }
            }
        };

        Ok(Attribute {
            default: coerced_default.map(Arc::new),
            doc: doc.to_owned(),
            coercer,
        })
    }

    fn dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::check_not_relative_label(default, "attr.dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let coercer = AttrType::dep(required_providers);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn any(#[starlark(require = named, default = "")] doc: &str) -> anyhow::Result<Attribute> {
        Ok(attr_any(doc))
    }

    fn bool<'v>(
        #[starlark(require = named, default = false)] default: Value<'v>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::attr(eval, Some(default), doc, AttrType::bool())
    }

    fn option<'v>(
        inner: &Attribute,
        #[starlark(require = named, default = NoneType)] default: Value<'v>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        let coercer = AttrType::option(inner.coercer.dupe());
        let attr = Attribute::attr(eval, Some(default), doc, coercer)?;

        if attr.default.as_ref().map_or(true, |x| x.may_return_none()) {
            Ok(attr)
        } else {
            Err(AttrError::OptionDefaultNone(default.to_string()).into())
        }
    }

    fn default_only(
        inner: &Attribute,
        #[starlark(require = named, default = "")] doc: &str,
    ) -> anyhow::Result<Attribute> {
        Ok(Attribute {
            default: inner.default.dupe(),
            doc: doc.to_owned(),
            coercer: AttrType::default_only(),
        })
    }

    fn label(
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::attr(eval, None, doc, AttrType::label())
    }

    fn dict<'v>(
        key: &Attribute,
        value: &Attribute,
        #[starlark(default = false)] sorted: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        let coercer = AttrType::dict(key.coercer.dupe(), value.coercer.dupe(), sorted);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn arg<'v>(
        #[allow(unused_variables)]
        #[starlark(default = false)]
        json: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::attr(eval, default, doc, AttrType::arg())
    }

    fn r#enum<'v>(
        #[starlark(require = pos)] variants: Vec<String>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        // Value seems to usually be a `[String]`, listing the possible values of the
        // enumeration. Unfortunately, for things like `exported_lang_preprocessor_flags`
        // it ends up being `Type` which doesn't match the data we see.
        Attribute::attr(eval, default, doc, AttrType::enumeration(variants)?)
    }

    fn configuration_label(
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::attr(eval, None, doc, AttrType::dep(Vec::new()))
    }

    fn regex<'v>(
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::attr(eval, default, doc, AttrType::string())
    }

    fn set<'v>(
        value_type: &Attribute,
        #[allow(unused_variables)]
        #[starlark(default = false)]
        sorted: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        let coercer = AttrType::list(value_type.coercer.dupe());
        Attribute::attr(eval, default, doc, coercer)
    }

    fn named_set<'v>(
        value_type: &Attribute,
        #[starlark(default = false)] sorted: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        let value_coercer = value_type.coercer.dupe();
        let coercer = AttrType::one_of(vec![
            AttrType::dict(AttrType::string(), value_coercer.dupe(), sorted),
            AttrType::list(value_coercer),
        ]);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn one_of<'v>(
        #[starlark(args)] args: Vec<&Attribute>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        let coercer = AttrType::one_of(args.into_map(|arg| arg.coercer.dupe()));
        Attribute::attr(eval, default, doc, coercer)
    }

    fn tuple<'v>(
        #[starlark(args)] args: Vec<&Attribute>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        let coercer = AttrType::tuple(args.into_map(|arg| arg.coercer.dupe()));
        Attribute::attr(eval, default, doc, coercer)
    }

    fn int<'v>(
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::attr(eval, default, doc, AttrType::int())
    }

    fn query(
        #[starlark(default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::attr(eval, None, doc, AttrType::query())
    }

    fn versioned(
        value_type: &Attribute,
        #[starlark(require = named, default = "")] doc: &str,
    ) -> anyhow::Result<Attribute> {
        // A versioned field looks like:
        // [ ({"key":"value1"}, arg), ({"key":"value2"}, arg) ]
        let element_type = AttrType::tuple(vec![
            AttrType::dict(AttrType::string(), AttrType::string(), false),
            value_type.coercer.dupe(),
        ]);
        let coercer = AttrType::list(element_type.dupe());

        Ok(Attribute {
            default: Some(Arc::new(AnyAttrType::empty_list(element_type))),
            doc: doc.to_owned(),
            coercer,
        })
    }

    fn source<'v>(
        #[starlark(default = false)] allow_directory: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Attribute> {
        Attribute::check_not_relative_label(default, "attr.source")?;
        Attribute::attr(eval, default, doc, AttrType::source(allow_directory))
    }
}

pub fn register_attr_module(registry: &mut GlobalsBuilder) {
    attr_module(registry)
}

pub mod testing {
    // utilities to create attributes for testing
    use std::sync::Arc;

    use crate::{
        attrs::{attr_type::AttrType, coerced_attr::CoercedAttr},
        interpreter::rule_defs::attr::Attribute,
    };

    pub(crate) trait AttributeExt {
        fn testing_new(default: Option<Arc<CoercedAttr>>, coercer: AttrType) -> Self;
    }

    impl AttributeExt for Attribute {
        fn testing_new(default: Option<Arc<CoercedAttr>>, coercer: AttrType) -> Attribute {
            Attribute {
                default,
                doc: String::new(),
                coercer,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::{
        buck_path::BuckPath,
        cells::paths::CellRelativePath,
        package::{Package, PackageRelativePathBuf},
        result::SharedResult,
    };
    use buck2_interpreter::package_listing::listing::{testing::PackageListingExt, PackageListing};
    use gazebo::prelude::*;
    use indoc::indoc;
    use starlark::values::Heap;

    use crate::{
        attrs::{attr_type::AttrType, AttrCoercionContext},
        interpreter::{
            rule_defs::attr::{AttrIsConfigurable, BuildAttrCoercionContext},
            testing::{cells, run_starlark_bzl_test, run_starlark_bzl_test_expecting_error},
        },
        nodes::hacks::value_to_string,
    };

    #[test]
    fn string_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen = attr.string(default="something", doc = "foo")
            def test():
                assert_eq('attr.string(default="something")', repr(attr.string(default="something", doc = "foo")))
                assert_eq('attr.string(default="something")', repr(frozen))
            "#
        ))
    }

    #[test]
    fn boolean_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen = attr.bool()
            def test():
                assert_eq('attr.bool(default=True)', repr(attr.bool(default=True, doc = "foo")))
                assert_eq('attr.bool(default=False)', repr(frozen))
            "#
        ))
    }

    #[test]
    fn test_attr_module_registered() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            def test():
                assert_eq(True, attr.string != None)
            "#
        ))
    }

    #[test]
    fn list_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen = attr.list(
                attr.string(default = "something", doc = "foo"),
                default=["1", "2"],
                doc = "foo",
            )
            def test():
                not_frozen = attr.list(
                    attr.string(default = "something", doc = "foo"),
                    default=[],
                    doc = "foo",
                )

                assert_eq('attr.list(attr.string(), default=[])', repr(not_frozen))
                assert_eq('attr.list(attr.string(), default=["1","2"])', repr(frozen))
            "#
        ))
    }

    #[test]
    fn enum_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen = attr.enum(["red", "green", "blue"])
            def test():
                not_frozen = attr.enum(["yes", "no"], default="no")
                assert_eq('attr.enum(["red","green","blue"])', repr(frozen))
                assert_eq('attr.enum(["yes","no"], default="no")', repr(not_frozen))
            "#
        ))
    }

    #[test]
    fn attr_coercer_coerces() -> anyhow::Result<()> {
        let heap = Heap::new();
        let some_cells = cells(None)?;
        let cell_alias_resolver = some_cells.0;
        let enclosing_package = (
            Package::new(
                cell_alias_resolver.resolve_self(),
                CellRelativePath::unchecked_new("foo"),
            ),
            PackageListing::testing_empty(),
        );
        let coercer_ctx = BuildAttrCoercionContext::new_with_package(
            cell_alias_resolver,
            enclosing_package,
            false,
        );
        let label_coercer = AttrType::dep(Vec::new());
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

        assert_eq!("root//foo:bar", value_to_string(&label_value1)?);
        assert_eq!("root//foo:bar[baz]", value_to_string(&label_value2)?);
        assert_eq!("root//foo:bar", value_to_string(&label_value3)?);
        assert_eq!("root//foo:bar[baz]", value_to_string(&label_value4)?);
        assert!(invalid_label_value1.is_err());
        assert!(invalid_label_value2.is_err());
        assert!(invalid_label_value3.is_err());

        let string_value1 =
            string_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("str"))?;
        assert_eq!("str", value_to_string(&string_value1)?);

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
        assert_eq!("red", value_to_string(&enum_valid1)?);
        assert_eq!("green", value_to_string(&enum_valid2)?);
        assert_eq!("red", value_to_string(&enum_valid3)?);
        assert!(enum_invalid1.is_err());
        assert!(enum_invalid2.is_err());

        Ok(())
    }

    #[test]
    fn dep_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen1 = attr.dep(default="root//foo:bar")
            frozen2 = attr.dep(default="//foo:bar")
            def test():
                assert_eq('attr.dep(default="root//foo:bar")', repr(attr.dep(default="//foo:bar")))
                assert_eq('attr.dep(default="root//foo:bar")', repr(frozen1))
                assert_eq('attr.dep(default="root//foo:bar")', repr(frozen2))
            "#
        ))?;

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                attr.dep(default="notatarget")
            "#
            ),
            "Type of parameter",
        );

        // Relative targets are disallowed; there is no build file for them to be relative to
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                attr.dep(default=":reltarget")
            "#
            ),
            "Use a fully qualified",
        );
        Ok(())
    }

    #[test]
    fn source_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen1 = attr.source(default="root//foo:bar")
            frozen2 = attr.source(default="//foo:bar")
            def test():
                assert_eq('attr.source(default="root//foo:bar")', repr(attr.source(default="root//foo:bar")))
                assert_eq('attr.source(default="root//foo:bar")', repr(frozen1))
                assert_eq('attr.source(default="root//foo:bar")', repr(frozen2))
            "#
        ))?;

        // Relative targets are disallowed; there is no build file for them to be relative to
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                attr.source(default=":reltarget")
            "#
            ),
            "Use a fully qualified",
        );
        Ok(())
    }

    #[test]
    fn coercing_src_to_path_works() -> anyhow::Result<()> {
        let cell_alias_resolver = cells(None).unwrap().0;
        let package = Package::new(
            cell_alias_resolver.resolve("")?,
            CellRelativePath::unchecked_new("foo/bar"),
        );
        let package_ctx = BuildAttrCoercionContext::new_with_package(
            cell_alias_resolver.dupe(),
            (
                package.dupe(),
                PackageListing::testing_files(&["baz/quz.cpp"]),
            ),
            false,
        );
        let no_package_ctx = BuildAttrCoercionContext::new_no_package(cell_alias_resolver);

        let err = no_package_ctx
            .coerce_path("baz/quz.cpp", false)
            .unwrap_err();
        assert!(err.to_string().contains("Expected a package"));

        let err = package_ctx
            .coerce_path("/invalid/absolute/path", false)
            .unwrap_err();
        assert!(format!("{:#}", err).contains("absolute path"), "{:?}", err);

        let err = package_ctx
            .coerce_path("../upward/traversal", false)
            .unwrap_err();
        assert!(err.to_string().contains("normalized path"));

        let expected = BuckPath::new(
            package,
            PackageRelativePathBuf::unchecked_new("baz/quz.cpp".to_owned()),
        );
        assert_eq!(
            &expected,
            package_ctx
                .coerce_path("baz/quz.cpp", false)
                .unwrap()
                .path()
        );
        Ok(())
    }
}
