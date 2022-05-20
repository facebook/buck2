//! The output stream for bxl to print values to the console as their result
//!

use derive_more::Display;
use gazebo::any::AnyLifetime;
use itertools::Itertools;
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{
        none::NoneType, AllocValue, Freeze, Freezer, Heap, NoSerialize, NoSimpleValue,
        StarlarkValue, Trace, UnpackValue, Value, ValueLike,
    },
};

use crate::bxl::starlark_defs::BxlError::NoFreeze;

#[derive(AnyLifetime, Debug, Display, Trace, NoSerialize)]
#[display(fmt = "{:?}", self)]
pub struct OutputStream {}

impl OutputStream {
    pub fn new() -> Self {
        Self {}
    }
}

impl<'v> UnpackValue<'v> for &'v OutputStream {
    fn expected() -> String {
        OutputStream::get_type_value_static().as_str().to_owned()
    }

    fn unpack_value(x: Value<'v>) -> Option<&'v OutputStream> {
        x.downcast_ref()
    }
}

impl<'v> StarlarkValue<'v> for OutputStream {
    starlark_type!("bxl_output_stream");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_output_stream)
    }
}

impl<'v> Freeze for OutputStream {
    type Frozen = NoSimpleValue;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Err(NoFreeze("OutputStream").into())
    }
}

impl<'v> AllocValue<'v> for OutputStream {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

#[starlark_module]
fn register_output_stream(builder: &mut MethodsBuilder) {
    /// Outputs results to the console via stdout. These outputs are considered to be the results
    /// of a bxl script, which will be displayed to stdout by buck2 even when the script is cached.
    ///
    /// Prints that are not result of the bxl should be printed via stderr via the stdlib `print`
    /// and `pprint`.
    fn print(_this: &OutputStream, args: Vec<Value>) -> anyhow::Result<NoneType> {
        println!("{}", &args.iter().map(|x| x.to_str()).join(" "));
        Ok(NoneType)
    }
}
