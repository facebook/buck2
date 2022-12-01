/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::cell::RefMut;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use buck2_core::fs::paths::RelativePathBuf;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::cell::ARef;
use gazebo::coerce::coerce;
use gazebo::coerce::Coerce;
use gazebo::display::display_chain;
use gazebo::display::display_container;
use gazebo::display::display_pair;
use gazebo::prelude::*;
use indexmap::IndexSet;
use regex::Regex;
use serde::Serialize;
use serde::Serializer;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_type;
use starlark::values::docs::StarlarkDocs;
use starlark::values::list::List;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use static_assertions::assert_eq_size;

use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::cmd_args::options::CommandLineOptions;
use crate::interpreter::rule_defs::cmd_args::options::QuoteStyle;
use crate::interpreter::rule_defs::cmd_args::options::RelativeOrigin;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::traits::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::traits::WriteToFileMacroVisitor;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::util::commas;

/// A tiny wrapper around `Value`/`FrozenValue` that proxies `CommandLineArgLike` calls.
///
/// This should be unnecessary, however I'm not smart enough to figure out how to get
/// things to live long enough, in `ValueAsCommandLineArgLike`, so I'm moving on with my life
/// for now. All values contained in here are guaranteed to implement `CommandLineArgLike`.
#[derive(Debug, Clone, Copy, Dupe, Trace, Display, Serialize, Allocative)]
#[serde(bound = "V: Display", transparent)]
struct CommandLineArgGen<V>(#[serde(serialize_with = "serialize_as_display")] V);

fn serialize_as_display<V: Display, S>(v: &V, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    s.collect_str(v)
}

impl<'v> CommandLineArgGen<Value<'v>> {
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<CommandLineArgGen<FrozenValue>> {
        Ok(CommandLineArgGen(self.0.freeze(freezer)?))
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgGen<V> {
    fn try_from_value(value: V) -> anyhow::Result<Self> {
        value.to_value().as_command_line_err()?;
        Ok(Self(value))
    }

    fn visit_inner<R>(&self, f: impl FnOnce(&dyn CommandLineArgLike) -> R) -> R {
        f(self.0.to_value().as_command_line().unwrap())
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for CommandLineArgGen<V> {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineBuilderContext,
    ) -> anyhow::Result<()> {
        self.visit_inner(|x| x.add_to_command_line(cli, context))
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        self.visit_inner(|x| x.visit_artifacts(visitor))
    }

    fn contains_arg_attr(&self) -> bool {
        self.visit_inner(|x| x.contains_arg_attr())
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        self.visit_inner(|x| x.visit_write_to_file_macros(visitor))
    }
}

/// Starlark object returned by `cmd_args()`
/// A container for all of the args and nested command lines that a users adds to `ctx.args()`
///
/// This allows more efficient iterative argument building, including O(1) insertion of
/// `CommandLine` / `FrozenCommandLine` args.
///
/// When frozen, a `FrozenCommandLine` is created.
///
/// `items` contains strings, artifacts, command line args (frozen and not), but does not
///         contain any builders.
#[derive(
    Debug,
    Default_,
    Clone,
    Trace,
    ProvidesStaticType,
    Serialize,
    Allocative
)]
#[serde(bound = "V: Display")]
#[repr(C)]
pub struct StarlarkCommandLineDataGen<'v, V: ValueLike<'v>> {
    items: Vec<CommandLineArgGen<V>>,
    hidden: Vec<CommandLineArgGen<V>>,
    options: Option<Box<CommandLineOptions<'v, V>>>,
}

// These types show up a lot in the frozen heaps, so make sure they don't regress
assert_eq_size!(StarlarkCommandLineDataGen<'static, FrozenValue>, [usize; 7]);
assert_eq_size!(CommandLineOptions<'static, FrozenValue>, [usize; 11]);

impl<'v, V: ValueLike<'v>> Display for StarlarkCommandLineDataGen<'v, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_container(
            f,
            "cmd_args(",
            ")",
            display_chain(
                &self.items,
                display_chain(
                    Some(&self.hidden).filter(|x| !x.is_empty()).map(|hidden| {
                        struct Wrapper<'a, V>(&'a Vec<CommandLineArgGen<V>>);
                        impl<'a, V: Display> Display for Wrapper<'a, V> {
                            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                                display_container(f, "[", "]", self.0.iter())
                            }
                        }
                        display_pair("hidden", "=", Wrapper(hidden))
                    }),
                    self.options
                        .iter()
                        .map(|options| display_pair("options", "=", options)),
                ),
            ),
        )
    }
}

impl<'v, V: ValueLike<'v>> StarlarkCommandLineDataGen<'v, V> {
    fn ignore_artifacts(&self) -> bool {
        self.options
            .as_ref()
            .map(|o| o.ignore_artifacts)
            .unwrap_or_default()
    }

    fn options_mut(&mut self) -> &mut CommandLineOptions<'v, V> {
        if self.options.is_none() {
            self.options = Some(box Default::default());
        }
        self.options.as_mut().unwrap()
    }

    fn is_concat(&self) -> bool {
        if let Some(x) = &self.options {
            x.delimiter.is_some()
        } else {
            false
        }
    }
}

impl<'v> StarlarkCommandLine<'v> {
    pub(crate) fn is_concat(&self) -> bool {
        self.0.borrow().is_concat()
    }
}

impl FrozenStarlarkCommandLine {
    pub(crate) fn is_concat(&self) -> bool {
        self.0.is_concat()
    }
}

unsafe impl<'v> Coerce<StarlarkCommandLineDataGen<'v, Value<'v>>>
    for StarlarkCommandLineDataGen<'static, FrozenValue>
{
}

#[derive(
    Debug,
    Default,
    Clone,
    Trace,
    ProvidesStaticType,
    Serialize,
    StarlarkDocs,
    Allocative
)]
#[serde(bound = "V : Serialize", transparent)]
pub struct StarlarkCommandLineGen<V>(V);

pub type StarlarkCommandLine<'v> =
    StarlarkCommandLineGen<RefCell<StarlarkCommandLineDataGen<'v, Value<'v>>>>;
pub type FrozenStarlarkCommandLine =
    StarlarkCommandLineGen<StarlarkCommandLineDataGen<'static, FrozenValue>>;

starlark_complex_values!(StarlarkCommandLine);

impl<'v> Display for StarlarkCommandLine<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.try_borrow() {
            Ok(x) => Display::fmt(&x, f),
            Err(_) => write!(f, "<cmd_args borrowed>"),
        }
    }
}

impl Display for FrozenStarlarkCommandLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<'v, V: ValueLike<'v>> StarlarkCommandLineDataGen<'v, V> {
    fn relative_to_path<C>(&self, ctx: &C) -> anyhow::Result<Option<RelativePathBuf>>
    where
        C: CommandLineBuilderContext + ?Sized,
    {
        match &self.options {
            None => Ok(None),
            Some(options) => options.relative_to_path(ctx),
        }
    }
}

impl<'v> StarlarkValue<'v> for StarlarkCommandLine<'v> {
    starlark_type!("cmd_args");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(command_line_builder_methods)
    }
}

impl<'v> StarlarkValue<'v> for FrozenStarlarkCommandLine {
    starlark_type!("cmd_args");

    fn get_methods() -> Option<&'static Methods> {
        // We return the same methods for frozen command lines, even though some of them fail,
        // so the methods remain consistent during freezing
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(command_line_builder_methods)
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for StarlarkCommandLineDataGen<'v, V> {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineBuilderContext,
    ) -> anyhow::Result<()> {
        match &self.options {
            None => {
                for item in &self.items {
                    item.add_to_command_line(cli, context)?;
                }
                Ok(())
            }
            Some(options) => options.wrap_builder(cli, context, |cli, context| {
                for item in &self.items {
                    item.add_to_command_line(cli, context)?;
                }
                Ok(())
            }),
        }
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        if !self.ignore_artifacts() {
            for item in &self.items {
                item.visit_artifacts(visitor)?;
            }
            for item in &self.hidden {
                item.visit_artifacts(visitor)?;
            }
        }
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        self.items.iter().any(|x| x.contains_arg_attr())
            || self.hidden.iter().any(|x| x.contains_arg_attr())
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        visitor.set_current_relative_to_path(&|ctx| self.relative_to_path(ctx))?;

        for item in &self.items {
            item.visit_write_to_file_macros(visitor)?;
        }
        for item in &self.hidden {
            item.visit_write_to_file_macros(visitor)?;
        }
        Ok(())
    }
}

impl<'v> CommandLineArgLike for StarlarkCommandLine<'v> {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineBuilderContext,
    ) -> anyhow::Result<()> {
        self.0.borrow().add_to_command_line(cli, context)
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        self.0.borrow().visit_artifacts(visitor)
    }

    fn contains_arg_attr(&self) -> bool {
        self.0.borrow().contains_arg_attr()
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        self.0.borrow().visit_write_to_file_macros(visitor)
    }
}

impl CommandLineArgLike for FrozenStarlarkCommandLine {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineBuilderContext,
    ) -> anyhow::Result<()> {
        self.0.add_to_command_line(cli, context)
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        self.0.visit_artifacts(visitor)
    }

    fn contains_arg_attr(&self) -> bool {
        self.0.contains_arg_attr()
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        self.0.visit_write_to_file_macros(visitor)
    }
}

impl<'v> Freeze for StarlarkCommandLine<'v> {
    type Frozen = StarlarkCommandLineGen<StarlarkCommandLineDataGen<'static, FrozenValue>>;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let StarlarkCommandLineDataGen {
            items,
            hidden,
            options,
        } = self.0.into_inner();

        let items = items.into_try_map(|x| x.freeze(freezer))?;
        let hidden = hidden.into_try_map(|x| x.freeze(freezer))?;
        let options = options.into_try_map(|options| options.freeze(freezer))?;

        Ok(StarlarkCommandLineGen(StarlarkCommandLineDataGen {
            items,
            hidden,
            options,
        }))
    }
}

impl<'v> StarlarkCommandLine<'v> {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn try_from_value(value: Value<'v>) -> anyhow::Result<Self> {
        let mut builder = Self::new();
        builder.0.get_mut().add_value(value)?;
        Ok(builder)
    }

    pub(crate) fn try_from_values_with_options(
        value: &[Value<'v>],
        delimiter: Option<StringValue<'v>>,
        format: Option<StringValue<'v>>,
        prepend: Option<StringValue<'v>>,
        quote: Option<QuoteStyle>,
    ) -> anyhow::Result<Self> {
        let mut builder = StarlarkCommandLineDataGen::default();
        if delimiter.is_some() || format.is_some() || prepend.is_some() || quote.is_some() {
            let opts = builder.options_mut();
            opts.delimiter = delimiter;
            opts.format = format;
            opts.prepend = prepend;
            opts.quote = quote;
        }
        for v in value {
            builder.add_value(*v)?;
        }
        Ok(Self(RefCell::new(builder)))
    }
}

impl<'v> StarlarkCommandLineDataGen<'v, Value<'v>> {
    fn add_value(&mut self, value: Value<'v>) -> anyhow::Result<()> {
        if let Some(values) = List::from_value(value) {
            self.add_values(values.content())?;
        } else {
            self.items.push(CommandLineArgGen::try_from_value(value)?);
        }
        Ok(())
    }

    /// Check the types of a list of values, and modify `data` accordingly
    ///
    /// The values must be one of: CommandLineArgLike or a list thereof.
    fn add_values(&mut self, values: &[Value<'v>]) -> anyhow::Result<()> {
        self.items.reserve(values.len());
        for value in values {
            self.add_value(*value)?
        }
        Ok(())
    }

    /// Add values to the artifact that don't show up on the command line, but do for dependency
    fn add_hidden(&mut self, values: &[Value<'v>]) -> anyhow::Result<()> {
        for value in values {
            if let Some(values) = List::from_value(*value) {
                self.add_hidden(values.content())?;
            } else {
                self.hidden.push(CommandLineArgGen::try_from_value(*value)?);
            }
        }
        Ok(())
    }
}

fn cmd_args_mut<'v>(
    x: Value<'v>,
) -> anyhow::Result<RefMut<'v, StarlarkCommandLineDataGen<'v, Value<'v>>>> {
    if let Some(v) = x.downcast_ref::<StarlarkCommandLine>() {
        Ok(v.0.borrow_mut())
    } else {
        Err(ValueError::CannotMutateImmutableValue.into())
    }
}

fn cmd_args<'v>(x: Value<'v>) -> ARef<'v, StarlarkCommandLineDataGen<Value<'v>>> {
    if let Some(x) = x.downcast_ref::<StarlarkCommandLine>() {
        ARef::new_ref(x.0.borrow())
    } else if let Some(x) = x.downcast_ref::<FrozenStarlarkCommandLine>() {
        ARef::new_ptr(coerce(&x.0))
    } else {
        unreachable!("This parameter must always be a type of command args")
    }
}

#[starlark_module]
fn command_line_builder_methods(builder: &mut MethodsBuilder) {
    fn add<'v>(
        this: Value<'v>,
        #[starlark(args)] args: Vec<Value<'v>>,
    ) -> anyhow::Result<Value<'v>> {
        cmd_args_mut(this)?.add_values(&args)?;
        Ok(this)
    }

    fn hidden<'v>(
        this: Value<'v>,
        #[starlark(args)] args: Vec<Value<'v>>,
    ) -> anyhow::Result<Value<'v>> {
        cmd_args_mut(this)?.add_hidden(&args)?;
        Ok(this)
    }

    fn ignore_artifacts<'v>(this: Value<'v>) -> anyhow::Result<Value<'v>> {
        cmd_args_mut(this)?.options_mut().ignore_artifacts = true;
        Ok(this)
    }

    fn relative_to<'v>(
        this: Value<'v>,
        directory: Value<'v>,
        #[starlark(default = 0i32)] parent: i32,
    ) -> anyhow::Result<Value<'v>> {
        if RelativeOrigin::from_value(directory).is_none() {
            return Err(ValueError::IncorrectParameterTypeNamed("directory".to_owned()).into());
        }
        if parent < 0 {
            return Err(ValueError::IncorrectParameterTypeNamed("parent".to_owned()).into());
        }
        cmd_args_mut(this)?.options_mut().relative_to = Some((directory, parent as usize));
        Ok(this)
    }

    fn absolute_prefix<'v>(this: Value<'v>, prefix: StringValue<'v>) -> anyhow::Result<Value<'v>> {
        cmd_args_mut(this)?.options_mut().absolute_prefix = Some(prefix);
        Ok(this)
    }

    fn absolute_suffix<'v>(this: Value<'v>, suffix: StringValue<'v>) -> anyhow::Result<Value<'v>> {
        cmd_args_mut(this)?.options_mut().absolute_suffix = Some(suffix);
        Ok(this)
    }

    /// For all the artifacts listed in this `cmd_args`, use their parent directory.
    ///
    /// Typically used when the file name is passed one way, and the directory another,
    /// e.g. `cmd_args(artifact, format="-L{}").parent()`.
    fn parent<'v>(
        this: Value<'v>,
        #[starlark(require = pos, default = 1i32)] count: i32,
    ) -> anyhow::Result<Value<'v>> {
        if count < 0 {
            return Err(ValueError::IncorrectParameterTypeNamed("count".to_owned()).into());
        }
        cmd_args_mut(this)?.options_mut().parent += count as usize;
        Ok(this)
    }

    fn replace_regex<'v>(
        this: Value<'v>,
        pattern: StringValue<'v>,
        replacement: StringValue<'v>,
    ) -> anyhow::Result<Value<'v>> {
        let mut cmd_args = cmd_args_mut(this)?;
        let options = cmd_args.options_mut();
        // Validate that regex is valid
        Regex::new(pattern.as_str())?;
        if let Some(replacements) = &mut options.replacements {
            replacements.push((pattern, replacement));
        } else {
            options.replacements = Some(box vec![(pattern, replacement)]);
        }
        Ok(this)
    }

    fn copy<'v>(this: Value<'v>) -> anyhow::Result<StarlarkCommandLine<'v>> {
        Ok(StarlarkCommandLineGen(RefCell::new(cmd_args(this).clone())))
    }

    /// Collect all the inputs (including hidden) referenced by this command line.
    #[starlark(attribute)]
    fn inputs<'v>(this: Value<'v>) -> anyhow::Result<StarlarkCommandLineInputs> {
        let mut visitor = SimpleCommandLineArtifactVisitor::new();
        cmd_args(this).visit_artifacts(&mut visitor)?;
        Ok(StarlarkCommandLineInputs {
            inputs: visitor.inputs,
        })
    }

    /// Collect all the outputs (including hidden) referenced by this command line.
    #[starlark(attribute)]
    fn outputs<'v>(this: Value<'v>) -> anyhow::Result<Vec<StarlarkOutputArtifact>> {
        let mut visitor = SimpleCommandLineArtifactVisitor::new();
        cmd_args(this).visit_artifacts(&mut visitor)?;
        Ok(visitor
            .outputs
            .into_iter()
            .map(StarlarkOutputArtifact::new)
            .collect())
    }
}

/// A wrapper for a [StarlarkCommandLine]'s inputs. This is an opaque type that only allows
/// debug-printing and querying the length to tell if any inputs exist.
#[derive(Debug, PartialEq, ProvidesStaticType, NoSerialize, Allocative)]
pub struct StarlarkCommandLineInputs {
    pub(crate) inputs: IndexSet<ArtifactGroup>,
}

starlark_simple_value!(StarlarkCommandLineInputs);

impl Display for StarlarkCommandLineInputs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut comma = commas();
        write!(f, "command_line_inputs(")?;
        for v in self.inputs.iter() {
            comma(f)?;
            Display::fmt(v, f)?;
        }
        write!(f, ")")
    }
}

impl<'v> StarlarkValue<'v> for StarlarkCommandLineInputs {
    starlark_type!("command_line_inputs");

    fn length(&self) -> anyhow::Result<i32> {
        Ok(self.inputs.len().try_into()?)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.inputs == other.inputs)
        } else {
            Ok(false)
        }
    }
}
