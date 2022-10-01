use gazebo::dupe::Dupe;

/// Daemon-level config that can tweak how the executors work.
#[derive(Clone, Dupe, Default)]
pub struct ExecutorGlobalKnobs {
    pub declare_in_local_executor: bool,
}
