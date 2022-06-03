//! The output stream for bxl to print values to the console as their result
//!

use std::{cell::RefCell, sync::Arc};

use buck2_core::fs::project::ProjectFilesystem;
use derivative::Derivative;
use derive_more::Display;
use gazebo::{any::ProvidesStaticType, prelude::SliceExt};
use itertools::Itertools;
use starlark::{
    collections::SmallSet,
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{
        none::NoneType, AllocValue, Freeze, Freezer, Heap, NoSerialize, NoSimpleValue,
        StarlarkValue, Trace, UnpackValue, Value, ValueError, ValueLike,
    },
};

use crate::{
    actions::artifact::ArtifactFs,
    bxl::starlark_defs::{
        artifacts::{EnsuredArtifact, EnsuredArtifactGen},
        BxlError::NoFreeze,
    },
    interpreter::rule_defs::artifact::ValueAsArtifactLike,
};

#[derive(ProvidesStaticType, Derivative, Display, Trace, NoSerialize)]
#[display(fmt = "{:?}", self)]
#[derivative(Debug)]
pub struct OutputStream<'v> {
    has_print: RefCell<bool>,
    #[trace(unsafe_ignore)]
    artifacts_to_ensure: RefCell<Option<SmallSet<Value<'v>>>>,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    project_fs: Arc<ProjectFilesystem>,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    artifact_fs: ArtifactFs,
}

impl<'v> OutputStream<'v> {
    pub fn new(project_fs: Arc<ProjectFilesystem>, artifact_fs: ArtifactFs) -> Self {
        Self {
            has_print: RefCell::new(false),
            artifacts_to_ensure: RefCell::new(Some(Default::default())),
            project_fs,
            artifact_fs,
        }
    }

    pub fn has_print(&self) -> bool {
        *self.has_print.borrow()
    }

    pub fn take_artifacts(&self) -> SmallSet<Value<'v>> {
        self.artifacts_to_ensure.borrow_mut().take().unwrap()
    }
}

impl<'v> UnpackValue<'v> for &'v OutputStream<'v> {
    fn expected() -> String {
        OutputStream::get_type_value_static().as_str().to_owned()
    }

    fn unpack_value(x: Value<'v>) -> Option<&'v OutputStream> {
        x.downcast_ref()
    }
}

impl<'v> StarlarkValue<'v> for OutputStream<'v> {
    starlark_type!("bxl_output_stream");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_output_stream)
    }
}

impl<'v> Freeze for OutputStream<'v> {
    type Frozen = NoSimpleValue;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Err(NoFreeze("OutputStream").into())
    }
}

impl<'v> AllocValue<'v> for OutputStream<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

#[starlark_module]
fn register_output_stream(builder: &mut MethodsBuilder) {
    /// Outputs results to the console via stdout. These outputs are considered to be the results
    /// of a bxl script, which will be displayed to stdout by buck2 even when the script is cached.
    /// Accepts an optional separator that defaults to " ".
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`.
    fn print(
        this: &OutputStream,
        args: Vec<Value>,
        #[starlark(default = " ")] sep: &str,
    ) -> anyhow::Result<NoneType> {
        // TODO handle printing of EnsuredArtifacts separately
        println!(
            "{}",
            &args
                .try_map(|x| {
                    anyhow::Ok(
                        if let Some(ensured) = <&EnsuredArtifact>::unpack_value(*x) {
                            let resolved = this
                                .artifact_fs
                                .resolve_artifactlike(ensured.artifact.as_artifact().unwrap())?;

                            if ensured.abs {
                                format!("{}", this.project_fs.resolve(&resolved).display())
                            } else {
                                resolved.as_str().to_owned()
                            }
                        } else {
                            x.to_str()
                        },
                    )
                })?
                .into_iter()
                .join(sep)
        );
        *this.has_print.borrow_mut() = true;

        Ok(NoneType)
    }

    /// Marks the artifact as an artifact that should be available to the users at the end of
    /// the bxl invocation. Any artifacts that do not get registered via this call is not
    /// accessible by users at the end of bxl script.
    ///
    /// This function returns an `ensured_artifact` type that can be printed via `ctx.output.print()`
    /// to print its actual path on disk.
    fn ensure<'v>(
        this: &OutputStream,
        artifact: Value<'v>,
    ) -> anyhow::Result<EnsuredArtifactGen<Value<'v>>> {
        artifact.as_artifact().ok_or_else(|| {
            ValueError::IncorrectParameterTypeWithExpected(
                "artifact-like".to_owned(),
                artifact.get_type().to_owned(),
            )
        })?;

        this.artifacts_to_ensure
            .borrow_mut()
            .as_mut()
            .expect("should not have been taken")
            .insert_hashed(artifact.get_hashed()?);

        EnsuredArtifactGen::new(artifact)
    }
}
