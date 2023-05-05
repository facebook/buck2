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

use anyhow::Context;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use starlark::values::dict::DictRef;
use starlark::values::enumeration::EnumValue;
use starlark::values::list::ListRef;
use starlark::values::record::Record;
use starlark::values::structs::StructRef;
use starlark::values::tuple::TupleRef;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::actions::artifact::artifact_type::Artifact;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::FrozenStarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;
use crate::interpreter::rule_defs::artifact_tagging::TaggedValue;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use crate::interpreter::rule_defs::cmd_args::FrozenStarlarkCommandLine;
use crate::interpreter::rule_defs::cmd_args::StarlarkCommandLine;
use crate::interpreter::rule_defs::provider::ProviderLike;
use crate::interpreter::rule_defs::provider::ValueAsProviderLike;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetJsonProjection;

/// A wrapper with a Serialize instance so we can pass down the necessary context.
struct SerializeValue<'a, 'v> {
    value: Value<'v>,
    fs: Option<&'a ExecutorFs<'a>>,
}

impl<'a, 'v> SerializeValue<'a, 'v> {
    fn with_value(&self, x: Value<'v>) -> Self {
        Self {
            value: x,
            fs: self.fs,
        }
    }
}

fn err<R, E: serde::ser::Error>(res: anyhow::Result<R>) -> Result<R, E> {
    match res {
        Ok(v) => Ok(v),
        Err(e) => Err(serde::ser::Error::custom(format!("{:#}", e))),
    }
}

/// Grab the value as an artifact, if you can.
/// We want to deal with both normal artifacts, and .as_output() artifacts,
/// since otherwise the .as_output ones will fall through as a cmd_args
/// and end up getting wrapped in a list below.
fn get_artifact<'v>(x: Value<'v>) -> Option<Box<dyn FnOnce() -> anyhow::Result<Artifact> + 'v>> {
    if let Some(x) = x.as_artifact() {
        Some(Box::new(|| Ok(x.get_bound_artifact()?.dupe())))
    } else if let Some(x) = x.downcast_ref::<StarlarkOutputArtifact>() {
        Some(Box::new(|| Ok(((*x.inner()).get_bound_artifact())?.dupe())))
    } else if let Some(x) = x.downcast_ref::<FrozenStarlarkOutputArtifact>() {
        Some(Box::new(|| Ok(x.inner().artifact())))
    } else {
        None
    }
}

enum JsonUnpack<'v> {
    None,
    String(&'v str),
    Number(i32),
    Bool(bool),
    List(&'v ListRef<'v>),
    Tuple(&'v TupleRef<'v>),
    Dict(DictRef<'v>),
    Struct(StructRef<'v>),
    Record(&'v Record<'v>),
    Enum(&'v EnumValue<'v>),
    TransitiveSetJsonProjection(&'v TransitiveSetJsonProjection<'v>),
    TargetLabel(&'v StarlarkTargetLabel),
    Label(&'v Label),
    Artifact(Box<dyn FnOnce() -> anyhow::Result<Artifact> + 'v>),
    CommandLine(&'v dyn CommandLineArgLike),
    Provider(&'v dyn ProviderLike<'v>),
    TaggedValue(&'v TaggedValue<'v>),
    Unsupported,
}

fn unpack<'v>(value: Value<'v>) -> JsonUnpack<'v> {
    if value.is_none() {
        JsonUnpack::None
    } else if let Some(x) = value.unpack_str() {
        JsonUnpack::String(x)
    } else if let Some(x) = value.unpack_int() {
        JsonUnpack::Number(x)
    } else if let Some(x) = value.unpack_bool() {
        JsonUnpack::Bool(x)
    } else if let Some(x) = ListRef::from_value(value) {
        JsonUnpack::List(x)
    } else if let Some(x) = TupleRef::from_value(value) {
        JsonUnpack::Tuple(x)
    } else if let Some(x) = DictRef::from_value(value) {
        JsonUnpack::Dict(x)
    } else if let Some(x) = StructRef::from_value(value) {
        JsonUnpack::Struct(x)
    } else if let Some(x) = Record::from_value(value) {
        JsonUnpack::Record(x)
    } else if let Some(x) = EnumValue::from_value(value) {
        JsonUnpack::Enum(x)
    } else if let Some(x) = TransitiveSetJsonProjection::from_value(value) {
        JsonUnpack::TransitiveSetJsonProjection(x)
    } else if let Some(x) = StarlarkTargetLabel::from_value(value) {
        JsonUnpack::TargetLabel(x)
    } else if let Some(x) = Label::from_value(value) {
        JsonUnpack::Label(x)
    } else if let Some(x) = get_artifact(value) {
        JsonUnpack::Artifact(x)
    } else if let Some(x) = value.as_command_line() {
        JsonUnpack::CommandLine(x)
    } else if let Some(x) = value.as_provider() {
        JsonUnpack::Provider(x)
    } else if let Some(x) = TaggedValue::from_value(value) {
        JsonUnpack::TaggedValue(x)
    } else {
        JsonUnpack::Unsupported
    }
}

impl<'a, 'v> Serialize for SerializeValue<'a, 'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match unpack(self.value) {
            JsonUnpack::None => serializer.serialize_none(),
            JsonUnpack::String(x) => serializer.serialize_str(x),
            JsonUnpack::Number(x) => serializer.serialize_i32(x),
            JsonUnpack::Bool(x) => serializer.serialize_bool(x),
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
            JsonUnpack::Label(x) => {
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
                        serializer.serialize_str(err(err(x())?.resolve_path(fs.fs()))?.as_str())
                    }
                }
            }
            JsonUnpack::CommandLine(x) => {
                let singleton = is_singleton_cmdargs(self.value);
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
                        let mut ctx = DefaultCommandLineContext::new(fs);
                        err(x.add_to_command_line(&mut items, &mut ctx))?;
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
                serializer.collect_map(x.items().iter().map(|(k, v)| (k, self.with_value(*v))))
            }
            JsonUnpack::TaggedValue(x) => self.with_value(*x.value()).serialize(serializer),
            JsonUnpack::Unsupported => Err(serde::ser::Error::custom(format!(
                "Type `{}` is not supported by `write_json`",
                self.value.get_type()
            ))),
        }
    }
}

fn is_singleton_cmdargs(x: Value) -> bool {
    if let Some(x) = x.downcast_ref::<StarlarkCommandLine>() {
        x.is_concat()
    } else if let Some(x) = x.downcast_ref::<FrozenStarlarkCommandLine>() {
        x.is_concat()
    } else {
        false
    }
}

pub fn validate_json(x: Value) -> anyhow::Result<()> {
    write_json(x, None, &mut sink())
}

pub fn write_json(x: Value, fs: Option<&ExecutorFs>, writer: impl Write) -> anyhow::Result<()> {
    serde_json::ser::to_writer(writer, &SerializeValue { value: x, fs })
        .context("Error converting to JSON for `write_json`")
}

pub fn visit_json_artifacts(
    v: Value,
    visitor: &mut dyn CommandLineArtifactVisitor,
) -> anyhow::Result<()> {
    match unpack(v) {
        JsonUnpack::None
        | JsonUnpack::String(_)
        | JsonUnpack::Number(_)
        | JsonUnpack::Bool(_)
        | JsonUnpack::TargetLabel(_)
        | JsonUnpack::Enum(_)
        | JsonUnpack::Label(_) => {}

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
            ArtifactGroup::TransitiveSetProjection(x.to_projection_key()?),
            None,
        ),
        JsonUnpack::Artifact(_x) => {
            // The _x function requires that the artifact is already bound, but we may need to visit artifacts
            // before that happens. Treating it like an opaque command_line works as we want for any artifact
            // type.
            v.as_command_line_err()?.visit_artifacts(visitor)?;
        }
        JsonUnpack::CommandLine(x) => x.visit_artifacts(visitor)?,
        JsonUnpack::Unsupported => {
            return Err(anyhow::anyhow!(
                "Type `{}` is not supported by `write_json` (this should be unreachable)",
                v.get_type()
            ));
        }
        JsonUnpack::Provider(x) => {
            for (_, v) in x.items() {
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

#[cfg(test)]
mod test {
    use buck2_interpreter_for_build::interpreter::testing::Tester;
    use indoc::indoc;
    use starlark::environment::GlobalsBuilder;

    use super::*;
    use crate::actions::artifact::artifact_type::OutputArtifact;
    use crate::actions::impls::json::visit_json_artifacts;
    use crate::interpreter::rule_defs::artifact::testing::artifactory;
    use crate::interpreter::rule_defs::artifact_tagging::testing::artifact_tag_factory;
    use crate::interpreter::rule_defs::artifact_tagging::ArtifactTag;

    #[test]
    fn test_tagging() -> anyhow::Result<()> {
        struct AssertVisitor {
            tag: ArtifactTag,
            artifact: Artifact,
        }

        impl CommandLineArtifactVisitor for AssertVisitor {
            fn visit_input(&mut self, input: ArtifactGroup, tag: Option<&ArtifactTag>) {
                assert_eq!(tag, Some(&self.tag));
                assert_eq!(input, ArtifactGroup::Artifact(self.artifact.dupe()));
            }

            fn visit_output(&mut self, _artifact: OutputArtifact, _tag: Option<&ArtifactTag>) {}
        }

        #[starlark_module]
        fn assertions(builder: &mut GlobalsBuilder) {
            fn check_artifact_is_tagged<'v>(
                tagged: Value<'v>,
                tag: Value<'v>,
                artifact: Value<'v>,
            ) -> anyhow::Result<Value<'v>> {
                let tag = ArtifactTag::from_value(tag).context("Invalid tag")?.dupe();

                let artifact = artifact
                    .as_artifact()
                    .context("Not an artifact")?
                    .get_bound_artifact()
                    .context("Not a bound artifact")?
                    .dupe();

                visit_json_artifacts(tagged, &mut AssertVisitor { tag, artifact })?;
                Ok(Value::new_none())
            }

            fn check_passthrough<'v>(
                tagged: Value<'v>,
                value: Value<'v>,
            ) -> anyhow::Result<Value<'v>> {
                let json1 = serde_json::to_string(&SerializeValue {
                    value: tagged,
                    fs: None,
                })?;

                let json2 = serde_json::to_string(&SerializeValue { value, fs: None })?;

                assert_eq!(json1, json2);

                Ok(Value::new_none())
            }
        }

        let mut tester = Tester::new()?;
        tester.additional_globals(artifact_tag_factory);
        tester.additional_globals(artifactory);
        tester.additional_globals(assertions);

        tester.run_starlark_bzl_test(indoc!(
            r#"
        def test():
            t1 = make_tag()
            a1 = source_artifact("foo", "bar")
            v1 = {"foo": "bar"}

            check_artifact_is_tagged(t1.tag_artifacts(a1), t1, a1)
            check_passthrough(t1.tag_artifacts(v1), v1)
        "#
        ))?;

        Ok(())
    }
}
