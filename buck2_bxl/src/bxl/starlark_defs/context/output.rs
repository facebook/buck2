//! The output stream for bxl to print values to the console as their result
//!

use std::{cell::RefCell, io::Write, sync::Arc};

use anyhow::Context;
use buck2_build_api::{
    actions::artifact::ArtifactFs,
    interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike,
};
use buck2_core::fs::project::ProjectFilesystem;
use derivative::Derivative;
use derive_more::Display;
use gazebo::{any::ProvidesStaticType, prelude::SliceExt};
use itertools::Itertools;
use serde::{Serialize, Serializer};
use starlark::{
    collections::SmallSet,
    environment::{Methods, MethodsBuilder, MethodsStatic},
    starlark_module, starlark_type,
    values::{
        dict::Dict,
        list::{List, ListRef},
        none::NoneType,
        record::Record,
        structs::Struct,
        tuple::Tuple,
        AllocValue, Heap, NoSerialize, StarlarkValue, Trace, UnpackValue, Value, ValueError,
        ValueLike,
    },
};

use crate::bxl::starlark_defs::{
    artifacts::{EnsuredArtifact, EnsuredArtifactGen},
    context::build::StarlarkProvidersArtifactIterable,
};

#[derive(ProvidesStaticType, Derivative, Display, Trace, NoSerialize)]
#[display(fmt = "{:?}", self)]
#[derivative(Debug)]
pub struct OutputStream<'v> {
    #[derivative(Debug = "ignore")]
    #[trace(unsafe_ignore)]
    sink: RefCell<Box<dyn Write>>,
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
    pub fn new(
        project_fs: Arc<ProjectFilesystem>,
        artifact_fs: ArtifactFs,
        sink: RefCell<Box<dyn Write>>,
    ) -> Self {
        Self {
            sink,
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

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_output_stream)
    }
}

impl<'v> AllocValue<'v> for OutputStream<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
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
        #[starlark(args)] args: Vec<Value>,
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

    /// Outputs results to the console via stdout as a json.
    /// These outputs are considered to be the results of a bxl script, which will be displayed to
    /// stdout by buck2 even when the script is cached.
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`.
    fn print_json(this: &OutputStream, value: Value) -> anyhow::Result<NoneType> {
        /// A wrapper with a Serialize instance so we can pass down the necessary context.
        struct SerializeValue<'a, 'v> {
            value: Value<'v>,
            artifact_fs: &'a ArtifactFs,
            project_fs: &'a ProjectFilesystem,
        }

        impl<'a, 'v> SerializeValue<'a, 'v> {
            fn with_value(&self, x: Value<'v>) -> Self {
                Self {
                    value: x,
                    artifact_fs: self.artifact_fs,
                    project_fs: self.project_fs,
                }
            }
        }

        impl<'a, 'v> Serialize for SerializeValue<'a, 'v> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
            {
                if let Some(ensured) = <&EnsuredArtifact>::unpack_value(self.value) {
                    let resolved = self
                        .artifact_fs
                        .resolve_artifactlike(ensured.artifact.as_artifact().unwrap())
                        .map_err(|err| serde::ser::Error::custom(format!("{:#}", err)))?;

                    if ensured.abs {
                        serializer.serialize_str(&format!(
                            "{}",
                            self.project_fs.resolve(&resolved).display()
                        ))
                    } else {
                        serializer.serialize_str(resolved.as_str())
                    }
                } else if let Some(x) = List::from_value(self.value) {
                    serializer.collect_seq(x.iter().map(|v| self.with_value(v)))
                } else if let Some(x) = Tuple::from_value(self.value) {
                    serializer.collect_seq(x.iter().map(|v| self.with_value(v)))
                } else if let Some(x) = Dict::from_value(self.value) {
                    serializer.collect_map(
                        x.iter()
                            .map(|(k, v)| (self.with_value(k), self.with_value(v))),
                    )
                } else if let Some(x) = Struct::from_value(self.value) {
                    serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
                } else if let Some(x) = Record::from_value(self.value) {
                    serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
                } else {
                    self.value.serialize(serializer)
                }
            }
        }

        println!(
            "{}",
            serde_json::to_string_pretty(&SerializeValue {
                value,
                artifact_fs: &this.artifact_fs,
                project_fs: &this.project_fs,
            },)
            .context("When converting to JSON for `write_json`")?
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
        this: &OutputStream<'v>,
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

    /// Same as `ensure`, but for multiple.
    fn ensure_multiple<'v>(
        this: &OutputStream<'v>,
        artifacts: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<Vec<EnsuredArtifactGen<Value<'v>>>> {
        if artifacts.is_none() {
            Ok(vec![])
        } else if let Some(list) = <&ListRef>::unpack_value(artifacts) {
            list.content().try_map(|artifact| {
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

                EnsuredArtifactGen::new(*artifact)
            })
        } else if let Some(artifact_gen) =
            <&StarlarkProvidersArtifactIterable>::unpack_value(artifacts)
        {
            artifact_gen
                .iterate(heap)?
                .map(|artifact| try {
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

                    EnsuredArtifactGen::new(artifact)?
                })
                .collect::<anyhow::Result<_>>()
        } else {
            Err(anyhow::anyhow!(
                ValueError::IncorrectParameterTypeWithExpected(
                    "list of artifacts or bxl-built-artifacts-iterable".to_owned(),
                    artifacts.get_type().to_owned()
                )
            ))
        }
    }
}
