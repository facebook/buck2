/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;

use allocative::Allocative;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::plugins::PluginKind;
use buck2_interpreter::plugins::PLUGIN_KIND_FROM_VALUE;
use derive_more::Display;
use dupe::Dupe;
use either::Either;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::typing::Ty;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueTypedComplex;

use crate::interpreter::build_context::BuildContext;

#[derive(Debug, derive_more::Display, Allocative)]
enum InnerStarlarkPluginKind {
    #[display("<plugin_kind <unbound>>")]
    Unbound(CellPath),
    #[display("<plugin_kind _0>")]
    Bound(PluginKind),
}

/// A kind of plugin, created via `plugins.kind()`.
///
/// These are used when declaring rules or attributes that make use of plugin deps.
#[derive(
    derive_more::Display,
    Debug,
    ProvidesStaticType,
    NoSerialize,
    Trace,
    Allocative
)]
#[display("{}", RefCell::borrow(_0))]
pub struct StarlarkPluginKind(RefCell<InnerStarlarkPluginKind>);

#[starlark_value(type = "PluginKind")]
impl<'v> StarlarkValue<'v> for StarlarkPluginKind {
    fn export_as(
        &self,
        variable_name: &str,
        _eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<()> {
        let mut inner = self.0.borrow_mut();
        let InnerStarlarkPluginKind::Unbound(cell_path) = &*inner else {
            // Was already exported
            return Ok(());
        };
        let cell_path = cell_path.clone();
        let kind = PluginKind::new(variable_name.to_owned(), cell_path);
        *inner = InnerStarlarkPluginKind::Bound(kind);
        Ok(())
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum PluginKindError {
    #[error("Plugin kind has not yet been assigned to a global")]
    NotBound,
    #[error("Expected a plugin kind, got a {0}")]
    NotAPluginKind(String),
}

impl StarlarkPluginKind {
    pub fn expect_bound(&self) -> buck2_error::Result<PluginKind> {
        match &*self.0.borrow() {
            InnerStarlarkPluginKind::Unbound(_) => Err(PluginKindError::NotBound.into()),
            InnerStarlarkPluginKind::Bound(kind) => Ok(kind.dupe()),
        }
    }
}

impl<'v> AllocValue<'v> for StarlarkPluginKind {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

#[derive(
    derive_more::Display,
    Debug,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
#[display("{_0}")]
pub struct FrozenStarlarkPluginKind(PluginKind);
starlark_simple_value!(FrozenStarlarkPluginKind);

#[starlark_value(type = "PluginKind")]
impl<'v> StarlarkValue<'v> for FrozenStarlarkPluginKind {
    type Canonical = StarlarkPluginKind;
}

impl Freeze for StarlarkPluginKind {
    type Frozen = FrozenStarlarkPluginKind;
    fn freeze(self, _: &Freezer) -> FreezeResult<Self::Frozen> {
        self.expect_bound()
            .map(FrozenStarlarkPluginKind)
            .map_err(|e| FreezeError::new(e.to_string()))
    }
}

fn plugin_kind_from_value_typed<'v>(
    v: ValueTypedComplex<'v, StarlarkPluginKind>,
) -> buck2_error::Result<PluginKind> {
    match v.unpack() {
        Either::Left(unfrozen) => unfrozen.expect_bound(),
        Either::Right(frozen) => Ok(frozen.0.dupe()),
    }
}

fn plugin_kind_from_value<'v>(v: Value<'v>) -> buck2_error::Result<PluginKind> {
    let Some(v) = ValueTypedComplex::new(v) else {
        return Err(PluginKindError::NotAPluginKind(v.to_repr()).into());
    };
    plugin_kind_from_value_typed(v)
}

pub(crate) struct PluginKindArg {
    pub(crate) plugin_kind: PluginKind,
}

impl StarlarkTypeRepr for PluginKindArg {
    type Canonical = <StarlarkPluginKind as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        <Self::Canonical as StarlarkTypeRepr>::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for PluginKindArg {
    type Error = starlark::Error;

    fn unpack_value_impl(value: Value<'v>) -> Result<Option<Self>, Self::Error> {
        let Some(v) = ValueTypedComplex::new(value) else {
            return Ok(None);
        };
        Ok(
            plugin_kind_from_value_typed(v)
                .map(|kind| Some(PluginKindArg { plugin_kind: kind }))?,
        )
    }
}

/// The value yielded by `plugins.ALL`
#[derive(Display, Debug, Allocative, ProvidesStaticType, NoSerialize)]
#[display("<all_plugins>")]
pub struct AllPlugins;
starlark_simple_value!(AllPlugins);

#[starlark_value(type = "all_plugins")]
impl<'v> StarlarkValue<'v> for AllPlugins {}

#[starlark_module]
fn register_plugins_methods(r: &mut GlobalsBuilder) {
    /// Create a new plugin kind.
    ///
    /// The value returned should always be immediately bound to a global, like `MyPluginKind =
    /// plugins.kind()`
    fn kind<'v>(eval: &mut Evaluator<'v, '_, '_>) -> starlark::Result<StarlarkPluginKind> {
        let cell_path = BuildContext::from_context(eval)?
            .starlark_path()
            .path()
            .into_owned();
        Ok(StarlarkPluginKind(RefCell::new(
            InnerStarlarkPluginKind::Unbound(cell_path),
        )))
    }

    /// A special value for use with `pulls_and_pushes_plugins`.
    ///
    /// This value can be passed to `pulls_and_pushes_plugins` on any `attr.dep()` to indicate that
    /// all plugin kinds from the dep should be pulled and pushed. This is useful for rules like
    /// `alias`.
    ///
    /// This value is not supported on `uses_plugins` at this time, and hence it is not useful on
    /// `pulls_plugins` either.
    const All: AllPlugins = AllPlugins;

    /// Type symbol for `PluginKind`.
    const PluginKind: StarlarkValueAsType<StarlarkPluginKind> = StarlarkValueAsType::new();
}

pub(crate) fn register_plugins(globals: &mut GlobalsBuilder) {
    globals.namespace("plugins", register_plugins_methods);
}

pub(crate) fn init_plugin_kind_from_value_impl() {
    PLUGIN_KIND_FROM_VALUE.init(plugin_kind_from_value)
}
