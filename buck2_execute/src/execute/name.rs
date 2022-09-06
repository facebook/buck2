use gazebo::dupe::Dupe;

/// Name of an executor. E.g. "remote", "local".
#[derive(Debug, Copy, Clone, Dupe)]
pub struct ExecutorName(pub &'static str);
