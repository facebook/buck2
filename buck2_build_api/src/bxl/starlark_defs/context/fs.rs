//! Provides some basic tracked filesystem access for bxl functions so that they can meaningfully
//! detect simple properties of artifacts, and source directories.

use std::{borrow::Cow, convert::TryFrom, path::Path};

use buck2_common::{
    dice::{cells::HasCellResolver, data::HasIoProvider, file_ops::HasFileOps},
    file_ops::FileOps,
};
use buck2_core::{
    self,
    fs::{paths::AbsPath, project::ProjectRelativePath},
};
use derivative::Derivative;
use derive_more::Display;
use gazebo::{any::AnyLifetime, prelude::*};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{
        AllocValue, Freeze, Freezer, Heap, NoSerialize, NoSimpleValue, StarlarkValue, Trace,
        UnpackValue, Value, ValueLike,
    },
};

use crate::bxl::starlark_defs::{context::starlark_async::BxlSafeDiceComputations, BxlError};

#[derive(AnyLifetime, Derivative, Display, Trace, NoSerialize)]
#[derivative(Debug)]
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

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(fs_operations)
    }
}

impl<'v> Freeze for BxlFilesystem<'v> {
    type Frozen = NoSimpleValue;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Err(BxlError::NoFreeze("BxlFilesystem").into())
    }
}

impl<'v> AllocValue<'v> for BxlFilesystem<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> UnpackValue<'v> for &'v BxlFilesystem<'v> {
    fn expected() -> String {
        BxlFilesystem::get_type_value_static().as_str().to_owned()
    }

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
            let path = ctx.get_cell_resolver().await.get_cell_path(&rel)?;

            ctx.file_ops().try_exists(&path).await
        })
    }
}
