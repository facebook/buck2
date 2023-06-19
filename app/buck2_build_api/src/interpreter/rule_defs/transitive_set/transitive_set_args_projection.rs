/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::iter;

use allocative::Allocative;
use anyhow::Context as _;
use display_container::display_pair;
use display_container::fmt_container;
use display_container::iter_display_chain;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::list::ListRef;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOf;

use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::TransitiveSetProjectionKey;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetOrdering;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetProjectionTraversal;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;

/// TransitiveSetArgsProjection is the starlark value returned from the starlark method `transitive_set.project_as_args()`
///
/// The projected values are all stored on the TransitiveSet itself and this value will reference back to that. The main
/// point of this object is to provide the implementation of CommandLineArgLike so that the args projection
/// can be used in places that accept command lines.
#[derive(Debug, Clone, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[derive(NoSerialize)] // TODO we should probably have a serialization for transitive set
#[repr(C)]
pub struct TransitiveSetArgsProjectionGen<V> {
    pub(super) transitive_set: V,

    /// The index of the projection. Once transitive sets are defined, their projections never
    /// change, so we can afford to just store the index here.
    pub projection: usize,

    /// The ordering to use when traversing the projection.
    pub ordering: TransitiveSetOrdering,
}

impl<'v, V: ValueLike<'v>> Display for TransitiveSetArgsProjectionGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let projection_name = self.projection_name().unwrap_or("<invalid projection>");
        fmt_container(
            f,
            "TransitiveSetProjection(",
            ")",
            iter_display_chain(
                iter::once(projection_name),
                iter::once(display_pair("transitive_set", "=", &self.transitive_set)),
            ),
        )
    }
}

impl<'v, V: ValueLike<'v>> TransitiveSetArgsProjectionGen<V> {
    fn projection_name(&self) -> anyhow::Result<&'v str> {
        TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?
            .projection_name(self.projection)
    }
}

impl<'v, V: ValueLike<'v>> TransitiveSetArgsProjectionGen<V> {
    /// For args projections, we allow either a CommandLineArgLike or a list of them (just like `cmd_args.add()`).
    /// This function allows us to treat those two as the same.
    /// TODO(cjhopman): It may be better to wrap the list case in a new CommandLineArgLike impl when returned from
    /// the projection. Then we'd only have to verify the contents type once and it might be a bit simpler to use.
    pub(super) fn as_command_line(v: V) -> anyhow::Result<impl CommandLineArgLike + 'v> {
        enum Impl<'v> {
            Item(&'v dyn CommandLineArgLike),
            List(&'v [Value<'v>]),
        }
        impl<'v> CommandLineArgLike for Impl<'v> {
            fn add_to_command_line(
                &self,
                cli: &mut dyn CommandLineBuilder,
                context: &mut dyn CommandLineContext,
            ) -> anyhow::Result<()> {
                match self {
                    Impl::Item(v) => v.add_to_command_line(cli, context),
                    Impl::List(items) => {
                        for v in *items {
                            v.as_command_line()
                                .unwrap()
                                .add_to_command_line(cli, context)?;
                        }
                        Ok(())
                    }
                }
            }

            fn contains_arg_attr(&self) -> bool {
                match self {
                    Impl::Item(v) => v.contains_arg_attr(),
                    Impl::List(items) => {
                        for v in *items {
                            if v.as_command_line().unwrap().contains_arg_attr() {
                                return true;
                            }
                        }
                        false
                    }
                }
            }

            fn visit_write_to_file_macros(
                &self,
                visitor: &mut dyn WriteToFileMacroVisitor,
            ) -> anyhow::Result<()> {
                match self {
                    Impl::Item(v) => v.visit_write_to_file_macros(visitor),
                    Impl::List(items) => {
                        for v in *items {
                            v.as_command_line()
                                .unwrap()
                                .visit_write_to_file_macros(visitor)?;
                        }
                        Ok(())
                    }
                }
            }

            fn visit_artifacts(
                &self,
                visitor: &mut dyn CommandLineArtifactVisitor,
            ) -> anyhow::Result<()> {
                match self {
                    Impl::Item(v) => v.visit_artifacts(visitor),
                    Impl::List(items) => {
                        for v in *items {
                            v.as_command_line().unwrap().visit_artifacts(visitor)?;
                        }
                        Ok(())
                    }
                }
            }
        }

        let value = v.to_value();
        if let Some(values) = ListRef::from_value(value) {
            for v in values.content() {
                v.as_command_line_err()?;
            }
            Ok(Impl::List(values.content()))
        } else {
            Ok(Impl::Item(value.as_command_line_err()?))
        }
    }
}

starlark_complex_value!(pub TransitiveSetArgsProjection);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TransitiveSetArgsProjectionGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    starlark_type!("transitive_set_args_projection");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(transitive_set_args_projection_methods)
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for TransitiveSetArgsProjectionGen<V> {
    fn add_to_command_line(
        &self,
        builder: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        let set = TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?;

        for node in set.iter(self.ordering).values() {
            let projection = node
                .projections
                .get(self.projection)
                .context("Invalid projection id")?;

            TransitiveSetArgsProjection::as_command_line(*projection)?
                .add_to_command_line(builder, context)?;
        }

        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        let set = TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?;

        visitor.visit_input(
            ArtifactGroup::TransitiveSetProjection(TransitiveSetProjectionKey {
                key: set.key().dupe(),
                projection: self.projection,
            }),
            None,
        );

        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        // TODO(cjhopman): This seems wrong, there's no enforcement that the projected values don't have arg attrs (and
        // in fact several of our common uses of in fact do have arg attrs). There doesn't seem to be any reason to
        // not allow them, either.
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        // TODO(cjhopman): This seems wrong, there's no verification that the projected
        // values don't have write_to_file_macros in them.
        Ok(())
    }
}

#[starlark_module]
fn transitive_set_args_projection_methods(builder: &mut MethodsBuilder) {
    fn traverse<'v>(
        this: ValueOf<'v, &'v TransitiveSetArgsProjection<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        Ok(heap.alloc(TransitiveSetProjectionTraversal {
            transitive_set: this.typed.transitive_set,
            projection: this.typed.projection,
            ordering: this.typed.ordering,
        }))
    }

    #[starlark(attribute)]
    fn projection_name<'v>(
        this: ValueOf<'v, &'v TransitiveSetArgsProjection<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<StringValue<'v>> {
        Ok(heap.alloc_str(this.typed.projection_name()?))
    }

    #[starlark(attribute)]
    fn transitive_set<'v>(
        this: ValueOf<'v, &'v TransitiveSetArgsProjection<'v>>,
    ) -> anyhow::Result<Value<'v>> {
        Ok(this.typed.transitive_set)
    }
}
