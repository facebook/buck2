use gazebo::dupe::Dupe;

/// Daemon-level config that can tweak how the ReExecutor works.
#[derive(Clone, Dupe, Default)]
pub struct ReExecutorGlobalKnobs {
    pub always_check_ttls: bool,
}
