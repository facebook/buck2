//! Provides some basic tracked filesystem access for bxl functions so that they can meaningfully
//! detect simple properties of artifacts, and source directories.

use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use derivative::Derivative;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
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
use crate::bxl::starlark_defs::file_expr::FileExpr;
use crate::bxl::starlark_defs::file_set::StarlarkReadDirSet;

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
    fn exists<'v>(this: &BxlFilesystem<'v>, expr: FileExpr<'v>) -> anyhow::Result<bool> {
        let path = expr.get(this.dice);

        match path {
            Ok(p) => this
                .dice
                .via_dice(async move |ctx| ctx.file_ops().try_exists(&p).await),
            Err(e) => Err(e),
        }
    }

    /// returns all the contents of the given 'FileExpr' that points to a directory.
    /// Errors if the given path is a file.  the optional `include_ignored` specifies
    /// whether to include the buckconfig's ignored files in the output.
    fn list<'v>(
        this: &BxlFilesystem<'v>,
        expr: FileExpr<'v>,
        #[starlark(require = named, default = false)] include_ignored: bool,
    ) -> anyhow::Result<StarlarkReadDirSet> {
        let path = expr.get(this.dice);

        match path {
            Ok(path) => this.dice.via_dice(async move |ctx| {
                let read_dir_output = ctx.file_ops().read_dir_with_ignores(&path).await;

                match read_dir_output {
                    Ok(read_dir_output) => Ok(StarlarkReadDirSet {
                        cell_path: path,
                        included: read_dir_output.included,
                        ignored: if include_ignored {
                            Some(read_dir_output.ignored)
                        } else {
                            None
                        },
                    }),
                    Err(e) => Err(e.into()),
                }
            }),
            Err(e) => Err(e),
        }
    }
}
