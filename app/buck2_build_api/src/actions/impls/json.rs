/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::sink;
use std::io::Write;
use std::sync::Arc;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use dupe::Dupe;
use either::Either;
use serde::Serialize;
use serde::Serializer;
use starlark::values::dict::DictRef;
use starlark::values::enumeration::EnumValue;
use starlark::values::list::ListRef;
use starlark::values::none::NoneType;
use starlark::values::record::Record;
use starlark::values::structs::StructRef;
use starlark::values::tuple::TupleRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueTypedComplex;

use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact_tagging::TaggedValue;
use crate::interpreter::rule_defs::cmd_args::value::CommandLineArg;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use crate::interpreter::rule_defs::cmd_args::FrozenStarlarkCmdArgs;
use crate::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;
use crate::interpreter::rule_defs::provider::ValueAsProviderLike;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetJsonProjection;

/// A wrapper with a Serialize instance so we can pass down the necessary context.
pub struct SerializeValue<'a, 'v> {
    pub value: JsonUnpack<'v>,
    pub fs: Option<&'a ExecutorFs<'a>>,
    pub absolute: bool,
}

struct Buck2ErrorResultOfSerializedValue<'a, 'v> {
    result: buck2_error::Result<SerializeValue<'a, 'v>>,
}

impl<'a, 'v> Serialize for Buck2ErrorResultOfSerializedValue<'a, 'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match &self.result {
            Ok(v) => v.serialize(serializer),
            Err(e) => Err(serde::ser::Error::custom(format!("{:#}", e))),
        }
    }
}

impl<'a, 'v> SerializeValue<'a, 'v> {
    fn with_value(&self, x: Value<'v>) -> Buck2ErrorResultOfSerializedValue<'a, 'v> {
        Buck2ErrorResultOfSerializedValue {
            result: JsonUnpack::unpack_value_err(x)
                .map_err(buck2_error::Error::from)
                .map(|value| SerializeValue {
                    value,
                    fs: self.fs,
                    absolute: self.absolute,
                }),
        }
    }
}

fn err<R, E: serde::ser::Error>(res: buck2_error::Result<R>) -> Result<R, E> {
    match res {
        Ok(v) => Ok(v),
        Err(e) => Err(serde::ser::Error::custom(format!("{:#}", e))),
    }
}

fn with_command_line_context<F, T>(fs: &ExecutorFs<'_>, absolute: bool, f: F) -> T
where
    F: FnOnce(&mut dyn CommandLineContext) -> T,
{
    let mut ctx = DefaultCommandLineContext::new(fs);
    let mut abs;
    let ctx = if absolute {
        abs = AbsCommandLineContext::wrap(ctx);
        &mut abs as _
    } else {
        &mut ctx as _
    };

    f(ctx)
}

/// Grab the value as an artifact, if you can.
/// We want to deal with both normal artifacts, and .as_output() artifacts,
/// since otherwise the .as_output ones will fall through as a cmd_args
/// and end up getting wrapped in a list below.
#[derive(UnpackValue, StarlarkTypeRepr)]
pub enum JsonArtifact<'v> {
    ValueAsArtifactLike(ValueAsArtifactLike<'v>),
    StarlarkOutputArtifact(ValueTypedComplex<'v, StarlarkOutputArtifact<'v>>),
}

impl<'v> JsonArtifact<'v> {
    fn artifact(&self) -> buck2_error::Result<Artifact> {
        match self {
            JsonArtifact::ValueAsArtifactLike(x) => Ok(x.0.get_bound_artifact()?.dupe()),
            JsonArtifact::StarlarkOutputArtifact(x) => match x.unpack() {
                Either::Left(x) => Ok((*x.inner()?).get_bound_artifact()?.dupe()),
                Either::Right(x) => Ok(x.inner()?.artifact()),
            },
        }
    }
}

/// Partially unpack the value into JSON writable with `ctx.actions.write_json`.
/// This does not help typechecker much (because it only validates top-level types),
/// but it provides better documentation.
#[derive(UnpackValue, StarlarkTypeRepr)]
pub enum JsonUnpack<'v> {
    None(NoneType),
    String(&'v str),
    Number(i64),
    Bool(bool),
    List(&'v ListRef<'v>),
    Tuple(&'v TupleRef<'v>),
    Dict(DictRef<'v>),
    Struct(StructRef<'v>),
    Record(&'v Record<'v>),
    Enum(&'v EnumValue<'v>),
    TransitiveSetJsonProjection(&'v TransitiveSetJsonProjection<'v>),
    TargetLabel(&'v StarlarkTargetLabel),
    ConfiguredProvidersLabel(&'v StarlarkConfiguredProvidersLabel),
    Artifact(JsonArtifact<'v>),
    CommandLine(CommandLineArg<'v>),
    Provider(ValueAsProviderLike<'v>),
    TaggedValue(&'v TaggedValue<'v>),
}

impl<'a, 'v> Serialize for SerializeValue<'a, 'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match &self.value {
            JsonUnpack::None(_) => serializer.serialize_none(),
            JsonUnpack::String(x) => serializer.serialize_str(x),
            JsonUnpack::Number(x) => serializer.serialize_i64(*x),
            JsonUnpack::Bool(x) => serializer.serialize_bool(*x),
            JsonUnpack::List(x) => serializer.collect_seq(x.iter().map(|v| self.with_value(v))),
            JsonUnpack::Tuple(x) => serializer.collect_seq(x.iter().map(|v| self.with_value(v))),
            JsonUnpack::Dict(x) => serializer.collect_map(
                x.iter()
                    .map(|(k, v)| (self.with_value(k), self.with_value(v))),
            ),
            JsonUnpack::Struct(x) => {
                serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
            }
            JsonUnpack::Record(x) => {
                serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
            }
            JsonUnpack::Enum(x) => x.serialize(serializer),
            JsonUnpack::TransitiveSetJsonProjection(x) => {
                serializer.collect_seq(err(x.iter_values())?.map(|v| self.with_value(v)))
            }
            JsonUnpack::TargetLabel(x) => {
                // Users could do this with `str(ctx.label.raw_target())`, but in some benchmarks that causes
                // a lot of additional memory to be retained for all those strings
                x.serialize(serializer)
            }
            JsonUnpack::ConfiguredProvidersLabel(x) => {
                // Users could do this with `str(ctx.label)`, but a bit wasteful
                x.serialize(serializer)
            }
            JsonUnpack::Artifact(x) => {
                match self.fs {
                    None => {
                        // Invariant: If fs == None, then the writer = sink().
                        // Therefore, in the None branch, we only care about getting Serde errors,
                        // so pass something of the right type, but don't worry about the value.
                        serializer.serialize_str("")
                    }
                    Some(fs) => {
                        let path = err(err(x.artifact())?.resolve_path(fs.fs()))?;
                        let path = with_command_line_context(fs, self.absolute, |ctx| {
                            err(ctx.resolve_project_path(path)).map(|loc| loc.into_string())
                        })?;
                        serializer.serialize_str(&path)
                    }
                }
            }
            JsonUnpack::CommandLine(x) => {
                let singleton = is_singleton_cmdargs(*x);
                match self.fs {
                    None => {
                        // See a few lines up for fs == None details.
                        if singleton {
                            serializer.serialize_str("")
                        } else {
                            serializer.collect_seq(&[""])
                        }
                    }
                    Some(fs) => {
                        // WriteJsonCommandLineArgGen assumes that any args/write-to-file macros are
                        // rejected here and needs to be updated if that changes.
                        let mut items = Vec::<String>::new();

                        with_command_line_context(fs, self.absolute, |ctx| {
                            err(x.as_command_line_arg().add_to_command_line(&mut items, ctx))
                        })?;

                        // We change the type, based on the value - singleton = String, otherwise list.
                        // That's a little annoying (type based on value), but otherwise there would be
                        // no way to produce a cmd_args as a single string.
                        if singleton {
                            serializer.serialize_str(&items.concat())
                        } else {
                            serializer.collect_seq(items)
                        }
                    }
                }
            }
            JsonUnpack::Provider(x) => {
                serializer.collect_map(x.0.items().iter().map(|(k, v)| (k, self.with_value(*v))))
            }
            JsonUnpack::TaggedValue(x) => self.with_value(*x.value()).serialize(serializer),
        }
    }
}

fn is_singleton_cmdargs(x: CommandLineArg) -> bool {
    if let Some(x) = x.to_value().downcast_ref::<StarlarkCmdArgs>() {
        x.is_concat()
    } else if let Some(x) = x.to_value().downcast_ref::<FrozenStarlarkCmdArgs>() {
        x.is_concat()
    } else {
        false
    }
}

pub fn validate_json(x: JsonUnpack) -> buck2_error::Result<()> {
    write_json(x, None, &mut sink(), false, false)
}

pub fn write_json(
    value: JsonUnpack,
    fs: Option<&ExecutorFs>,
    mut writer: &mut dyn Write,
    pretty: bool,
    absolute: bool,
) -> buck2_error::Result<()> {
    let value = SerializeValue {
        value,
        fs,
        absolute,
    };
    (|| {
        if pretty {
            serde_json::to_writer_pretty(&mut writer, &value)?;
            // serde_json does not add a trailing line, but we add it because
            // "pretty" implies that this JSON is for people to read.
            writer.write_all(b"\n")?;
        } else {
            serde_json::to_writer(&mut writer, &value)?;
        }
        buck2_error::Ok(())
    })()
    .buck_error_context("Error converting to JSON for `write_json`")
}

pub fn visit_json_artifacts(
    v: Value,
    visitor: &mut dyn CommandLineArtifactVisitor,
) -> buck2_error::Result<()> {
    match JsonUnpack::unpack_value_err(v)? {
        JsonUnpack::None(_)
        | JsonUnpack::String(_)
        | JsonUnpack::Number(_)
        | JsonUnpack::Bool(_)
        | JsonUnpack::TargetLabel(_)
        | JsonUnpack::Enum(_)
        | JsonUnpack::ConfiguredProvidersLabel(_) => {}

        JsonUnpack::List(x) => {
            for x in x.iter() {
                visit_json_artifacts(x, visitor)?;
            }
        }
        JsonUnpack::Tuple(x) => {
            for x in x.iter() {
                visit_json_artifacts(x, visitor)?;
            }
        }
        JsonUnpack::Dict(x) => {
            for (k, v) in x.iter() {
                visit_json_artifacts(k, visitor)?;
                visit_json_artifacts(v, visitor)?;
            }
        }
        JsonUnpack::Struct(x) => {
            for (_k, v) in x.iter() {
                visit_json_artifacts(v, visitor)?;
            }
        }
        JsonUnpack::Record(x) => {
            for (_k, v) in x.iter() {
                visit_json_artifacts(v, visitor)?;
            }
        }
        JsonUnpack::TransitiveSetJsonProjection(x) => visitor.visit_input(
            ArtifactGroup::TransitiveSetProjection(Arc::new(x.to_projection_key()?)),
            None,
        ),
        JsonUnpack::Artifact(_x) => {
            // The _x function requires that the artifact is already bound, but we may need to visit artifacts
            // before that happens. Treating it like an opaque command_line works as we want for any artifact
            // type.
            ValueAsCommandLineLike::unpack_value_err(v)?
                .0
                .visit_artifacts(visitor)?;
        }
        JsonUnpack::CommandLine(x) => x.as_command_line_arg().visit_artifacts(visitor)?,
        JsonUnpack::Provider(x) => {
            for (_, v) in x.0.items() {
                visit_json_artifacts(v, visitor)?;
            }
        }
        JsonUnpack::TaggedValue(v) => {
            let mut visitor = v.wrap_visitor(visitor);
            visit_json_artifacts(*v.value(), &mut visitor)?;
        }
    }
    Ok(())
}
