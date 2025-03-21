/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_data::ActionErrorLocation;
use buck2_data::ActionErrorLocations;
use buck2_data::ActionSubError;
use buck2_data::CommandExecution;
use derive_more::Display;
use display_container::fmt_container;
use gazebo::prelude::SliceClonedExt;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::typing::Ty;
use starlark::values::dict::DictType;
use starlark::values::list::UnpackList;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueOfUnchecked;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use crate::starlark::values::ValueLike;

pub(crate) type ActionSubErrorResult<'a> = UnpackList<&'a StarlarkActionSubError<'a>>;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
pub(crate) enum ActionErrorHandlerError {
    #[error("Error handler failed. Expected return type `{0}`, got value with type `{1}`")]
    TypeError(Ty, String),
}

#[derive(ProvidesStaticType, Trace, Allocative, Debug, Display, NoSerialize)]
#[display(
     "ActionErrorCtx(stderr: {}, stdout: {})",
     self.stderr,
     self.stdout
 )]
pub struct StarlarkActionErrorContext<'v> {
    stderr: String,
    stdout: String,
    output_artifacts: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>,
}

impl<'v> StarlarkActionErrorContext<'v> {
    pub(crate) fn new_from_command_execution(
        command: Option<&CommandExecution>,
        output_artifacts: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>,
    ) -> Self {
        let stderr = command.map_or(String::default(), |c| {
            c.details
                .as_ref()
                .map_or(String::default(), |c| c.stderr.clone())
        });
        let stdout = command.map_or(String::default(), |c| {
            c.details
                .as_ref()
                .map_or(String::default(), |c| c.stdout.clone())
        });

        StarlarkActionErrorContext {
            stderr,
            stdout,
            output_artifacts,
        }
    }
}

impl<'v> AllocValue<'v> for StarlarkActionErrorContext<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

#[starlark_value(type = "ActionErrorCtx", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkActionErrorContext<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(action_error_context_methods)
    }
}

/// Methods available on `ActionErrorCtx` to help categorize the action failure. These
/// categorizations should be finer grain, and most likely language specific.
#[starlark_module]
fn action_error_context_methods(builder: &mut MethodsBuilder) {
    /// The stderr of the failed action.
    #[starlark(attribute)]
    fn stderr<'v>(this: &'v StarlarkActionErrorContext) -> starlark::Result<&'v str> {
        Ok(&this.stderr)
    }

    /// The stdout of the failed action.
    #[starlark(attribute)]
    fn stdout<'v>(this: &'v StarlarkActionErrorContext) -> starlark::Result<&'v str> {
        Ok(&this.stdout)
    }

    // The output artifacts of the failed action.
    #[starlark(attribute)]
    fn output_artifacts<'v>(
        this: &'v StarlarkActionErrorContext<'v>,
    ) -> starlark::Result<ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>>
    {
        Ok(this.output_artifacts)
    }

    /// Create a new error location, specifying a file path and an optional line number.
    ///
    /// The file path should be either a project-relative path, or an absolute path.
    fn new_error_location<'v>(
        #[starlark(this)] _this: &'v StarlarkActionErrorContext,
        #[starlark(require = named)] file: String,
        #[starlark(require = named, default = NoneOr::None)] line: NoneOr<u64>,
    ) -> starlark::Result<StarlarkActionErrorLocation> {
        // @TODO(wendyy) - actually enforce/validate the path types.
        Ok(StarlarkActionErrorLocation {
            file,
            line: line.into_option(),
        })
    }

    /// Create a new sub error, specifying an error category name, optional message, and
    /// an optional list of error locations.
    ///
    /// The category should be finer grain error categorizations provided by the rule authors,
    /// and tend to be language specific. These should not be any kind of shared concepts
    /// among all errors for all languages/rules. For example, timeouts and infra errors
    /// should not go here - buck2 tries to categorize these types of errors automatically.
    /// An example of a finer grain error category may be the error code for rustc outputs.
    ///
    /// The message will be emitted to the build report, and to the stderr in the error diagnostics
    /// section.
    fn new_sub_error<'v>(
        #[starlark(this)] _this: &'v StarlarkActionErrorContext,
        #[starlark(require = named)] category: String,
        #[starlark(require = named, default = NoneOr::None)] message: NoneOr<String>,
        #[starlark(require = named, default = NoneOr::None)] locations: NoneOr<
            UnpackListOrTuple<&'v StarlarkActionErrorLocation>,
        >,
    ) -> starlark::Result<StarlarkActionSubError<'v>> {
        Ok(StarlarkActionSubError {
            category,
            message: message.into_option(),
            locations: locations.into_option(),
        })
    }
}

#[derive(
    ProvidesStaticType,
    Trace,
    Allocative,
    Debug,
    Display,
    NoSerialize,
    Clone,
    Default,
    Ord,
    PartialOrd,
    Eq,
    PartialEq
)]
#[display(
     "ActionErrorLocation(file={}, line={})",
     self.file,
     self.line.map_or("None".to_owned(), |l| l.to_string())
 )]
pub struct StarlarkActionErrorLocation {
    file: String,
    line: Option<u64>,
}

#[starlark_value(type = "ActionErrorLocation", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkActionErrorLocation {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(action_error_location_methods)
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.eq(other))
        } else {
            Ok(false)
        }
    }

    fn compare(&self, other: Value<'v>) -> starlark::Result<std::cmp::Ordering> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.cmp(other))
        } else {
            ValueError::unsupported_with(self, "compare", other)
        }
    }
}

impl<'v> AllocValue<'v> for StarlarkActionErrorLocation {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

/// Methods available on `StarlarkActionErrorLocation` to help with testing the error
/// handler implementation
#[starlark_module]
fn action_error_location_methods(builder: &mut MethodsBuilder) {
    /// The file of the error location. This is only needed for action error handler
    /// unit testing.
    #[starlark(attribute)]
    fn file<'v>(this: &'v StarlarkActionErrorLocation) -> starlark::Result<&'v str> {
        Ok(&this.file)
    }

    /// The line of the error location. This is only needed for action error handler
    /// unit testing.
    #[starlark(attribute)]
    fn line<'v>(this: &'v StarlarkActionErrorLocation) -> starlark::Result<NoneOr<u64>> {
        Ok(NoneOr::from_option(this.line))
    }
}

#[derive(
    ProvidesStaticType,
    Trace,
    Allocative,
    Debug,
    NoSerialize,
    Clone,
    Ord,
    PartialOrd,
    Eq,
    PartialEq
)]
pub(crate) struct StarlarkActionSubError<'v> {
    category: String,
    message: Option<String>,
    #[allocative(skip)]
    #[trace(unsafe_ignore)]
    locations: Option<UnpackListOrTuple<&'v StarlarkActionErrorLocation>>,
}

impl<'v> Display for StarlarkActionSubError<'v> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = format!(
            "ActionSubError(category={}, message={}, locations=[",
            self.category,
            self.message.clone().unwrap_or_default()
        );
        fmt_container(
            f,
            &prefix,
            "])",
            self.locations
                .as_ref()
                .map_or(Vec::new(), |l| l.items.iter().collect()),
        )
    }
}

impl<'v> AllocValue<'v> for StarlarkActionSubError<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

#[starlark_value(type = "ActionSubError", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkActionSubError<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(action_sub_error_methods)
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.eq(other))
        } else {
            Ok(false)
        }
    }

    fn compare(&self, other: Value<'v>) -> starlark::Result<std::cmp::Ordering> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.cmp(other))
        } else {
            ValueError::unsupported_with(self, "compare", other)
        }
    }
}

/// Methods available on `StarlarkActionSubError` to help with testing the error
/// handler implementation
#[starlark_module]
fn action_sub_error_methods(builder: &mut MethodsBuilder) {
    /// The category name of this sub error. This function is only needed for action
    /// error handler unit testing.
    #[starlark(attribute)]
    fn category<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<&'v str> {
        Ok(&this.category)
    }

    /// The optional message associated with this sub error.  This function is only
    /// needed for action error handler unit testing.
    #[starlark(attribute)]
    fn message<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<NoneOr<&'v str>> {
        Ok(match &this.message {
            Some(message) => NoneOr::Other(message.as_str()),
            None => NoneOr::None,
        })
    }

    /// Any locations associated with this sub error.  This function is only needed
    /// for action error handler unit testing.
    #[starlark(attribute)]
    fn locations<'v>(
        this: &'v StarlarkActionSubError,
    ) -> starlark::Result<NoneOr<Vec<StarlarkActionErrorLocation>>> {
        match &this.locations {
            None => Ok(NoneOr::None),
            Some(locations) => Ok(NoneOr::Other(locations.items.cloned())),
        }
    }
}

impl<'v> StarlarkActionSubError<'v> {
    pub(crate) fn to_proto(&self) -> ActionSubError {
        ActionSubError {
            category: self.category.clone(),
            message: self.message.clone(),
            locations: self
                .locations
                .clone()
                .map(|locations| ActionErrorLocations {
                    locations: locations
                        .items
                        .iter()
                        .map(|l| ActionErrorLocation {
                            file: l.file.clone(),
                            line: l.line,
                        })
                        .collect(),
                }),
        }
    }
}

#[starlark_module]
pub(crate) fn register_action_error_types(globals: &mut GlobalsBuilder) {
    const ActionSubError: StarlarkValueAsType<StarlarkActionSubError> = StarlarkValueAsType::new();
    const ActionErrorCtx: StarlarkValueAsType<StarlarkActionErrorContext> =
        StarlarkValueAsType::new();
    const ActionErrorLocation: StarlarkValueAsType<StarlarkActionErrorLocation> =
        StarlarkValueAsType::new();
}

/// Global methods for testing starlark action error handler.
#[starlark_module]
pub(crate) fn register_action_error_handler_for_testing(builder: &mut GlobalsBuilder) {
    /// Global function to create a new `ActionErrorContext` for testing a starlark action error
    /// handler via `bxl_test`.
    fn new_test_action_error_ctx<'v>(
        #[starlark(require=named, default = "")] stderr: &str,
        #[starlark(require=named, default = "")] stdout: &str,
    ) -> starlark::Result<StarlarkActionErrorContext<'v>> {
        Ok(StarlarkActionErrorContext {
            stderr: stderr.to_owned(),
            stdout: stdout.to_owned(),
            output_artifacts: ValueOfUnchecked::new(starlark::values::Value::new_none()),
        })
    }
}
