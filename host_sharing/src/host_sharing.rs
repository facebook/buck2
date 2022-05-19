// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use futures_intrusive::sync::{SharedSemaphore, SharedSemaphoreReleaser};

use crate::NamedSemaphores;

const SINGLE_RUN: usize = 1;

/// This class is intended to represent the resources required by each test. This is then used to
/// map onto resources available on the machine where the tests are run on in order to not saturate the
/// machine and adversely impact testrunning performance and reliability.
/// Testpilot Classic uses the concept of machine cores to manage resources. Testpilot has two types of
/// tests: Normal (1 core) and Heavy (4 cores) where this would run on a machine with typically 24 cores
/// (managed through a semaphore with size 24). Since Testpilot was introduced we have moved to run
/// on Sandcastle machines with 56 cores so we want to move away from the core-analogy and instead use
/// the term "permits" to describe the limited resources available on each machine.
/// More long term we want improve this to also take into account memory usage, cpu usage etc.
#[derive(Debug, Clone, PartialEq)]
pub enum WeightClass {
    /// Tests can require any number of permits and this can be used to mimic resource utilization like
    /// memory or cpu. For now, we map the Testpilot behaviour as Normal->Permits(1) and Heavy->Permits(4).
    Permits(usize),
}

/// Some commands require that we only run one instance of this binary (using an identifier)
/// to check for other instances of the same binary.
/// Some commands required the full host to run, others just dont care.
/// This enum encapsulates all the different scenarios.
#[derive(Debug, Clone, PartialEq)]
pub enum HostSharingRequirements {
    /// Needs exclusive access to the host. No other processes should run.
    ExclusiveAccess,
    /// Can share with other processes, but not with others requiring the same token
    OnePerToken(String, WeightClass),
    /// Run with any other processes within reasonable limits.
    Shared(WeightClass),
}

impl Default for HostSharingRequirements {
    fn default() -> HostSharingRequirements {
        HostSharingRequirements::Shared(WeightClass::Permits(1))
    }
}

/// A guard for all permits and resources acquired for a HostSharingBroker.acquire request.
/// Keeps the data structures received from semaphores after acquiring.
/// Semaphores are held until this struct is dropped.
pub struct HostSharingGuard {
    _run_guard: SharedSemaphoreReleaser,
    _name_guard: Option<SharedSemaphoreReleaser>,
}

/// Used to ensure that host resources are properly reserved before executing a command spec.
pub struct HostSharingBroker {
    permits: SharedSemaphore,
    num_machine_permits: usize,
    named_semaphores: NamedSemaphores,
}

impl HostSharingBroker {
    // If a test requires Permits(4) permits but the machine only has 3 permits then we cap the
    // test's required permits to 3. Otherwise the test would never be allowed to run.
    fn requested_permits(&self, weight_class: &WeightClass) -> usize {
        match weight_class {
            WeightClass::Permits(required_permits) => {
                self.num_machine_permits.min(*required_permits)
            }
        }
    }

    pub fn new(host_sharing_strategy: HostSharingStrategy, num_machine_permits: usize) -> Self {
        let permits = match host_sharing_strategy {
            HostSharingStrategy::Fifo => SharedSemaphore::new(true, num_machine_permits),
            HostSharingStrategy::SmallerTasksFirst => {
                SharedSemaphore::new(false, num_machine_permits)
            }
        };

        Self {
            permits,
            num_machine_permits,
            named_semaphores: NamedSemaphores::new(),
        }
    }

    pub fn num_machine_permits(&self) -> usize {
        self.num_machine_permits
    }

    pub async fn acquire(
        &self,
        host_sharing_requirements: &HostSharingRequirements,
    ) -> HostSharingGuard {
        match host_sharing_requirements {
            HostSharingRequirements::Shared(weight_class) => {
                let permits = self.requested_permits(weight_class);
                let _run_guard = self.permits.acquire(permits).await;
                HostSharingGuard {
                    _run_guard,
                    _name_guard: None,
                }
            }
            HostSharingRequirements::ExclusiveAccess => {
                let _run_guard = self.permits.acquire(self.num_machine_permits).await;
                HostSharingGuard {
                    _run_guard,
                    _name_guard: None,
                }
            }
            HostSharingRequirements::OnePerToken(identifier, weight_class) => {
                // Ensure that there is only one active run per identifier.
                // Acquire the identifier semaphore first, then acquire the permits needed to actually run.
                // This is so that no permits (which map to system resources / cores) are held while waiting
                // for the previous run on this identifier to finish.
                let run_semaphore = self.named_semaphores.get(identifier);
                let _name_guard = Some(run_semaphore.acquire(SINGLE_RUN).await);
                let permits = self.requested_permits(weight_class);
                let _run_guard = self.permits.acquire(permits).await;
                HostSharingGuard {
                    _run_guard,
                    _name_guard,
                }
            }
        }
    }
}

/// Determines whether a fair or unfair semaphore is used to manage host sharing
pub enum HostSharingStrategy {
    SmallerTasksFirst,
    Fifo,
}

#[cfg(test)]
mod tests {
    use super::{HostSharingBroker, HostSharingStrategy, WeightClass};

    #[test]
    // if we only have 2 machine permits then even a test requiring 4 permits will be capped to only require 2 permits
    // (otherwise it would not run)
    fn test_heavyweight_capped_to_machine_permits() {
        let broker = HostSharingBroker::new(HostSharingStrategy::SmallerTasksFirst, 2);

        let permits = broker.requested_permits(&WeightClass::Permits(4));
        assert_eq!(2, permits);
    }
}
