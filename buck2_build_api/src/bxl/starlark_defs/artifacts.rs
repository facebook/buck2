//! bxl additional artifact types

use std::fmt::{Display, Formatter};

use gazebo::{
    any::{AnyLifetime, ProvidesStaticType},
    coerce::Coerce,
};
use serde::{Serialize, Serializer};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{Freeze, Heap, StarlarkValue, Trace, Value, ValueLike, ValueOf},
};

use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;

/// An artifact that will be materialized to buck-out at the end of the bxl invocation.
/// These artifacts can be printed to bxl's results. Doing so will print the path of the artifact
/// rather than the standard representation.
#[derive(Clone, Debug, Coerce, Trace, Freeze, AnyLifetime)]
#[repr(C)]
pub struct EnsuredArtifactGen<V> {
    pub artifact: V,
    pub abs: bool,
}

impl<'v, V: ValueLike<'v>> EnsuredArtifactGen<V> {
    pub fn new(artifact: V) -> anyhow::Result<Self> {
        if artifact.as_artifact().is_none() {
            Err(anyhow::anyhow!("must be artifact like"))
        } else {
            Ok(Self {
                artifact,
                abs: false,
            })
        }
    }
}

starlark_complex_value!(pub EnsuredArtifact);

impl<'v, V: ValueLike<'v>> Display for EnsuredArtifactGen<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // this can't be converted to string in starlark and used as path arbitrarily.
        // this can only be printed as path via `ctx.output.print()`. Anywhere that wants to use
        // it as a path to commands should use the normal starlark artifacts to construct their
        // command
        write!(f, "<ensured {}>", self.artifact)
    }
}

impl<'v, V: ValueLike<'v>> Serialize for EnsuredArtifactGen<V> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_str(self)
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for EnsuredArtifactGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("ensured_artifact");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(artifact_methods)
    }
}

#[starlark_module]
fn artifact_methods(builder: &mut MethodsBuilder) {
    /// converts this artifact to be printed by its absolute path
    fn abs_path<'v>(
        this: ValueOf<'v, &'v EnsuredArtifact>,
        heap: &Heap,
    ) -> anyhow::Result<Value<'v>> {
        if this.typed.abs {
            Ok(this.value)
        } else {
            Ok(heap.alloc(EnsuredArtifactGen {
                artifact: this.typed.artifact,
                abs: true,
            }))
        }
    }

    /// converts this artifact to be printed by its path relative to the project root
    fn rel_path<'v>(
        this: ValueOf<'v, &'v EnsuredArtifact>,
        heap: &Heap,
    ) -> anyhow::Result<Value<'v>> {
        if !this.typed.abs {
            Ok(this.value)
        } else {
            Ok(heap.alloc(EnsuredArtifactGen {
                artifact: this.typed.artifact,
                abs: false,
            }))
        }
    }
}
