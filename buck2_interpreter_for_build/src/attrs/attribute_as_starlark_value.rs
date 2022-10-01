use std::ops::Deref;

use buck2_node::attrs::attr::Attribute;
use gazebo::any::ProvidesStaticType;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(derive_more::Display, Debug, ProvidesStaticType, NoSerialize)]
#[display(fmt = "{}", .0)]
pub struct AttributeAsStarlarkValue(pub Attribute);

starlark_simple_value!(AttributeAsStarlarkValue);

impl<'v> StarlarkValue<'v> for AttributeAsStarlarkValue {
    starlark_type!("attribute");
}

impl Deref for AttributeAsStarlarkValue {
    type Target = Attribute;

    fn deref(&self) -> &Attribute {
        &self.0
    }
}
