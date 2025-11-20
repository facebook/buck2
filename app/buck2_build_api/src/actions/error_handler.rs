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

use allocative::Allocative;
use buck2_data::ActionSubError;
use buck2_data::CommandExecution;
use console::strip_ansi_codes;
use derive_more::Display;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::typing::Ty;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueOfUnchecked;
use starlark::values::dict::DictType;
use starlark::values::list::UnpackList;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use crate::starlark::values::ValueLike;

pub(crate) type ActionSubErrorResult<'a> = UnpackList<&'a StarlarkActionSubError>;

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
            c.details.as_ref().map_or(String::default(), |c| {
                strip_ansi_codes(&c.cmd_stderr).to_string()
            })
        });
        let stdout = command.map_or(String::default(), |c| {
            c.details.as_ref().map_or(String::default(), |c| {
                strip_ansi_codes(&c.cmd_stdout).to_string()
            })
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
    /// Retrieve the stderr of the failed action.
    /// Can use string/regex matching to identify the error in order to categorize it.
    #[starlark(attribute)]
    fn stderr<'v>(this: &'v StarlarkActionErrorContext) -> starlark::Result<&'v str> {
        Ok(&this.stderr)
    }

    /// Retrieve the stdout of the failed action.
    /// Can use string/regex matching to identify the patterns in order to categorize it.
    #[starlark(attribute)]
    fn stdout<'v>(this: &'v StarlarkActionErrorContext) -> starlark::Result<&'v str> {
        Ok(&this.stdout)
    }

    /// Allows the output artifacts to be retrieve if [`outputs_for_error_handler`](https://buck2.build/docs/api/build/AnalysisActions/#analysisactionsrun)
    /// is set and the output artifact exists. This is useful for languages with structured error output, making the error retrieval process simpler.
    ///
    /// This is also the recommended way to retrieve file path and line number, as reliably extracting that information
    /// from stdout/stderr can be challenging
    #[starlark(attribute)]
    fn output_artifacts<'v>(
        this: &'v StarlarkActionErrorContext<'v>,
    ) -> starlark::Result<ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>>
    {
        Ok(this.output_artifacts)
    }

    /// Create a new sub error, specifying an error category name, optional message, and
    /// optional location information.
    ///
    /// The category should be finer grain error categorizations provided by the rule authors,
    /// and tend to be language specific. These should not be any kind of shared concepts
    /// among all errors for all languages/rules. For example, timeouts and infra errors
    /// should not go here - buck2 tries to categorize these types of errors automatically.
    /// An example of a finer grain error category may be the error code for rustc outputs.
    ///
    /// 'category': Required, useful for providing a more granular error category for action errors.
    /// 'message': Optional, provide users with additional context about the error to help with debugging/understandability/resolution, etc.
    /// 'file': Optional, file path where the error occurred.
    /// 'lnum': Optional, line number where the error occurred.
    /// 'end_lnum': Optional, end line number for multi-line error spans.
    /// 'col': Optional, column number where the error occurred.
    /// 'end_col': Optional, end column number for error ranges.
    /// 'error_type': Optional, type of error (e.g., error, warning, info).
    /// 'error_number': Optional, numeric error code.
    /// 'show_in_stderr': Optional, whether to show this error in stderr (default: false).
    ///
    /// The message will be emitted to the build report, and to the stderr in the error diagnostics
    /// section.
    fn new_sub_error<'v>(
        #[starlark(this)] _this: &'v StarlarkActionErrorContext,
        #[starlark(require = named)] category: String,
        #[starlark(require = named, default = NoneOr::None)] message: NoneOr<String>,
        #[starlark(require = named, default = NoneOr::None)] file: NoneOr<String>,
        #[starlark(require = named, default = NoneOr::None)] lnum: NoneOr<u64>,
        #[starlark(require = named, default = NoneOr::None)] end_lnum: NoneOr<u64>,
        #[starlark(require = named, default = NoneOr::None)] col: NoneOr<u64>,
        #[starlark(require = named, default = NoneOr::None)] end_col: NoneOr<u64>,
        #[starlark(require = named, default = NoneOr::None)] error_type: NoneOr<String>,
        #[starlark(require = named, default = NoneOr::None)] error_number: NoneOr<u64>,
        #[starlark(require = named, default = false)] show_in_stderr: bool,
    ) -> starlark::Result<StarlarkActionSubError> {
        Ok(StarlarkActionSubError {
            category: RefCell::new(category),
            message: message.into_option(),
            file: file.into_option(),
            lnum: lnum.into_option(),
            end_lnum: end_lnum.into_option(),
            col: col.into_option(),
            end_col: end_col.into_option(),
            error_type: error_type.into_option(),
            error_number: error_number.into_option(),
            show_in_stderr: RefCell::new(show_in_stderr),
        })
    }

    /// Parse error text using vim errorformat patterns to create structured error information.
    /// This method leverages vim's proven errorformat system to extract file paths, line numbers,
    /// and error messages from compiler/tool output, automatically creating ActionSubError objects.
    ///
    /// For errorformat pattern syntax, see: https://neovim.io/doc/user/quickfix.html#errorformat
    ///
    /// Multiple patterns can be provided and will be tried in order until one matches.
    /// This is useful for tools that may output errors in different formats.
    ///
    /// Args:
    /// - `category`: Base category name for the generated sub-errors (e.g., "rust", "gcc")
    /// - `error`: The error text to parse (typically stderr or stdout from the failed action)
    /// - `errorformats`: List of vim errorformat pattern strings to try matching against
    ///
    /// Returns a list of ActionSubError objects with structured error information including
    /// file locations when successfully parsed from the error text.
    fn parse_with_errorformat<'v>(
        #[starlark(this)] _this: &'v StarlarkActionErrorContext,
        #[starlark(require = named)] category: String,
        #[starlark(require = named)] error: String,
        #[starlark(require = named)] errorformats: UnpackListOrTuple<String>,
        _heap: &'v Heap,
    ) -> starlark::Result<Vec<StarlarkActionSubError>> {
        let error_lines = buck2_errorformat::split_lines(&error);
        let error_entries = buck2_errorformat::parse_error_format(errorformats.items, error_lines)
            .map_err(buck2_error::Error::from)?;
        let res = error_entries
            .into_iter()
            .map(|e| StarlarkActionSubError::from_errorformat_entry(e, category.clone()))
            .collect();
        Ok(res)
    }
}

#[derive(ProvidesStaticType, Trace, Allocative, Debug, NoSerialize)]
pub(crate) struct StarlarkActionSubError {
    category: RefCell<String>,
    message: Option<String>,

    // file path for the error location
    file: Option<String>,
    // Line number
    lnum: Option<u64>,
    // End line (for multi-line spans)
    end_lnum: Option<u64>,
    //  Column number
    col: Option<u64>,
    // End column (for ranges)
    end_col: Option<u64>,
    // Type of error (error, warning, info, etc.)
    error_type: Option<String>,
    // Numeric error code (e.g., 404, 500)
    error_number: Option<u64>,
    // Whether to show this error in stderr
    show_in_stderr: RefCell<bool>,
}

impl Display for StarlarkActionSubError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ActionSubError(category={}", self.category.borrow())?;

        if let Some(ref message) = self.message {
            write!(f, ", message={}", message)?;
        }

        if let Some(ref file) = self.file {
            write!(f, ", file={}", file)?;
        }

        if let Some(lnum) = self.lnum {
            write!(f, ", lnum={}", lnum)?;
        }

        if let Some(end_lnum) = self.end_lnum {
            write!(f, ", end_lnum={}", end_lnum)?;
        }

        if let Some(col) = self.col {
            write!(f, ", col={}", col)?;
        }

        if let Some(end_col) = self.end_col {
            write!(f, ", end_col={}", end_col)?;
        }

        if let Some(ref error_type) = self.error_type {
            write!(f, ", error_type={}", error_type)?;
        }

        if let Some(error_number) = self.error_number {
            write!(f, ", error_number={}", error_number)?;
        }

        write!(f, ", show_in_stderr={}", self.show_in_stderr.borrow())?;

        write!(f, ")")
    }
}

impl PartialEq for StarlarkActionSubError {
    fn eq(&self, other: &Self) -> bool {
        *self.category.borrow() == *other.category.borrow()
            && self.message == other.message
            && self.file == other.file
            && self.lnum == other.lnum
            && self.end_lnum == other.end_lnum
            && self.col == other.col
            && self.end_col == other.end_col
            && self.error_type == other.error_type
            && self.error_number == other.error_number
            && *self.show_in_stderr.borrow() == *other.show_in_stderr.borrow()
    }
}

impl Eq for StarlarkActionSubError {}

impl StarlarkActionSubError {
    pub(crate) fn from_errorformat_entry(
        entry: buck2_errorformat::Entry,
        category: String,
    ) -> Self {
        StarlarkActionSubError {
            category: RefCell::new(category),
            message: entry.message,
            file: entry.filename,
            lnum: entry.lnum.map(|x| x as u64),
            end_lnum: entry.end_lnum.map(|x| x as u64),
            col: entry.col.map(|x| x as u64),
            end_col: entry.end_col.map(|x| x as u64),
            error_type: entry.error_type,
            error_number: entry.error_number.map(|x| x as u64),
            show_in_stderr: RefCell::new(false),
        }
    }
}

impl<'v> AllocValue<'v> for StarlarkActionSubError {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

#[starlark_value(type = "ActionSubError", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkActionSubError {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(action_sub_error_methods)
    }

    fn set_attr(&self, attribute: &str, new_value: Value<'v>) -> starlark::Result<()> {
        match attribute {
            "category" => {
                let new_category = new_value.unpack_str_err()?;
                *self.category.borrow_mut() = new_category.to_owned();
                Ok(())
            }
            "show_in_stderr" => {
                let new_show_in_stderr = new_value
                    .unpack_bool()
                    .expect("show_in_stderr expected bool");
                *self.show_in_stderr.borrow_mut() = new_show_in_stderr;
                Ok(())
            }
            _ => ValueError::unsupported(self, &format!(".{attribute}=")),
        }
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.eq(other))
        } else {
            Ok(false)
        }
    }
}

/// Methods available on `StarlarkActionSubError` to help with testing the error
/// handler implementation
#[starlark_module]
fn action_sub_error_methods(builder: &mut MethodsBuilder) {
    /// A more granular category for the action error.
    #[starlark(attribute)]
    fn category<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<String> {
        Ok(this.category.borrow().clone())
    }

    /// An optional message to be displayed with the error, used to provide additoinal context
    #[starlark(attribute)]
    fn message<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<NoneOr<&'v str>> {
        Ok(match &this.message {
            Some(message) => NoneOr::Other(message.as_str()),
            None => NoneOr::None,
        })
    }

    /// File path for the error location.
    #[starlark(attribute)]
    fn file<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<NoneOr<&'v str>> {
        Ok(match &this.file {
            Some(file) => NoneOr::Other(file.as_str()),
            None => NoneOr::None,
        })
    }

    /// Line number for the error location.
    #[starlark(attribute)]
    fn lnum<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<NoneOr<u64>> {
        Ok(NoneOr::from_option(this.lnum))
    }

    /// End line number for multi-line error spans.
    #[starlark(attribute)]
    fn end_lnum<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<NoneOr<u64>> {
        Ok(NoneOr::from_option(this.end_lnum))
    }

    /// Column number for the error location.
    #[starlark(attribute)]
    fn col<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<NoneOr<u64>> {
        Ok(NoneOr::from_option(this.col))
    }

    /// End column number for error ranges.
    #[starlark(attribute)]
    fn end_col<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<NoneOr<u64>> {
        Ok(NoneOr::from_option(this.end_col))
    }

    /// Type of error (e.g., error, warning, info).
    #[starlark(attribute)]
    fn error_type<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<NoneOr<&'v str>> {
        Ok(match &this.error_type {
            Some(error_type) => NoneOr::Other(error_type.as_str()),
            None => NoneOr::None,
        })
    }

    /// Numeric error code (e.g., 404, 500).
    #[starlark(attribute)]
    fn error_number<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<NoneOr<u64>> {
        Ok(NoneOr::from_option(this.error_number))
    }

    /// Whether to show this error in stderr.
    #[starlark(attribute)]
    fn show_in_stderr<'v>(this: &'v StarlarkActionSubError) -> starlark::Result<bool> {
        Ok(*this.show_in_stderr.borrow())
    }
}

impl StarlarkActionSubError {
    pub(crate) fn to_proto(&self) -> ActionSubError {
        ActionSubError {
            category: self.category.borrow().clone(),
            message: self.message.clone(),
            file: self.file.clone(),
            lnum: self.lnum,
            end_lnum: self.end_lnum,
            col: self.col,
            end_col: self.end_col,
            error_type: self.error_type.clone(),
            error_number: self.error_number,
            show_in_stderr: *self.show_in_stderr.borrow(),
        }
    }
}

#[starlark_module]
pub(crate) fn register_action_error_types(globals: &mut GlobalsBuilder) {
    const ActionSubError: StarlarkValueAsType<StarlarkActionSubError> = StarlarkValueAsType::new();
    const ActionErrorCtx: StarlarkValueAsType<StarlarkActionErrorContext> =
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
