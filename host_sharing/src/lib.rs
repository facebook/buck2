// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

#![deny(unused_crate_dependencies)]
mod named_semaphores;
pub use named_semaphores::NamedSemaphores;

pub mod host_sharing;
pub use crate::host_sharing::HostSharingBroker;
pub use crate::host_sharing::HostSharingRequirements;
pub use crate::host_sharing::HostSharingStrategy;
pub use crate::host_sharing::WeightClass;
