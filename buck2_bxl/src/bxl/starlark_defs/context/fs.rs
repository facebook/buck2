//! Provides some basic tracked filesystem access for bxl functions so that they can meaningfully
//! detect simple properties of artifacts, and source directories.

use std::borrow::Cow;
use std::convert::TryFrom;
use std::path::Path;

use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_core;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::project::ProjectRelativePath;
use derivative::Derivative;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs
)]
#[derivative(Debug)]
#[starlark_docs_attrs(directory = "bxl")]
#[display(fmt = "{:?}", self)]
pub struct BxlFilesystem<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    dice: &'v BxlSafeDiceComputations<'v>,
}

impl<'v> BxlFilesystem<'v> {
    pub(crate) fn new(dice: &'v BxlSafeDiceComputations<'v>) -> Self {
        Self { dice }
    }
}

impl<'v> StarlarkValue<'v> for BxlFilesystem<'v> {
    starlark_type!("fs");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(fs_operations)
    }
}

impl<'v> AllocValue<'v> for BxlFilesystem<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v BxlFilesystem<'v> {
    fn starlark_type_repr() -> String {
        BxlFilesystem::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v BxlFilesystem<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v BxlFilesystem<'v>> {
        x.downcast_ref()
    }
}

#[starlark_module]
fn fs_operations(builder: &mut MethodsBuilder) {
    /// check if a path exists on disk, taking advantage of Buck's cached filesystem
    fn exists<'v>(this: &BxlFilesystem<'v>, path: &str) -> anyhow::Result<bool> {
        let fs = this.dice.0.global_data().get_io_provider().fs().dupe();
        let rel = if Path::new(path).is_absolute() {
            fs.relativize(AbsPath::new(path)?)?
        } else {
            Cow::Borrowed(<&ProjectRelativePath>::try_from(path)?)
        };

        this.dice.via_dice(async move |ctx| {
            let path = ctx.get_cell_resolver().await?.get_cell_path(&rel)?;

            ctx.file_ops().try_exists(&path).await
        })
    }
}
