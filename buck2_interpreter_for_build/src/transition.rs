use std::sync::Arc;

use buck2_core::configuration::transition::id::TransitionId;
use gazebo::any::ProvidesStaticType;

/// Implemented by starlark transition objects.
pub trait TransitionValue {
    fn transition_id(&self) -> anyhow::Result<Arc<TransitionId>>;
}

unsafe impl<'v> ProvidesStaticType for &'v dyn TransitionValue {
    type StaticType = &'static dyn TransitionValue;
}
