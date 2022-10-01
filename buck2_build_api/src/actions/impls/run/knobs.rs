use dice::UserComputationData;
use gazebo::prelude::*;

/// Knobs controlling how RunAction works.
#[derive(Copy, Clone, Dupe, Default)]
pub struct RunActionKnobs {
    /// Process dep files as they are generated.
    pub eager_dep_files: bool,

    /// Hash all commands using the same mechanism as dep files. This allows us to skip
    /// re-executing commands if their inputs and outputs haven't changed.
    pub hash_all_commands: bool,

    /// Valiate matches in the materializer (this is necessary for correctness).
    pub declare_match_in_depfiles: bool,
}

pub trait HasRunActionKnobs {
    fn set_run_action_knobs(&mut self, knobs: RunActionKnobs);

    fn get_run_action_knobs(&self) -> RunActionKnobs;
}

impl HasRunActionKnobs for UserComputationData {
    fn set_run_action_knobs(&mut self, knobs: RunActionKnobs) {
        self.data.set(knobs);
    }

    fn get_run_action_knobs(&self) -> RunActionKnobs {
        *self
            .data
            .get::<RunActionKnobs>()
            .expect("RunActionKnobs should be set")
    }
}
