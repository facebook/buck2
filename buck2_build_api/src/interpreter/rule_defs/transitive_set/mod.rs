/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::module_inception)]

mod transitive_set;
mod transitive_set_args_projection;
mod transitive_set_definition;
mod transitive_set_iterator;
mod traversal;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub mod testing;

use thiserror::Error;

pub use self::transitive_set::FrozenTransitiveSet;
pub use self::transitive_set::TransitiveSet;
use self::transitive_set::TransitiveSetGen;
pub use self::transitive_set::TransitiveSetLike;
pub use self::transitive_set_args_projection::FrozenTransitiveSetArgsProjection;
pub use self::transitive_set_args_projection::TransitiveSetArgsProjection;
pub use self::transitive_set_definition::transitive_set_definition_from_value;
pub use self::transitive_set_definition::FrozenTransitiveSetDefinition;
pub use self::transitive_set_definition::TransitiveSetDefinition;
pub use self::transitive_set_definition::TransitiveSetOperations;
use self::transitive_set_iterator::TransitiveSetIteratorGen;

#[derive(Debug, Error)]
pub enum TransitiveSetError {
    #[error(
        "Transitive set type must be assigned to a top-level variable, e.g. `MySet = transitive_set()`"
    )]
    TransitiveSetNotAssigned,

    #[error(
        "Transitive set was provided with a definition that is not the output of transitive_set()"
    )]
    TransitiveSetDefinitionWasInvalid,

    #[error(
        "Transitive set was used before being assigned to a top-level variable, e.g. `MySet = transitive_set()`"
    )]
    TransitiveSetUsedBeforeAssignment,

    #[error("Transitive set transitive values must be transitive")]
    TransitiveValueIsNotTransitiveSet,

    #[error(
        "Transitive set transitive values must be of the same transitive set type (expected: `{}`, got: `{}`)",
        .expected,
        .got
    )]
    TransitiveValueIsOfWrongType { expected: String, got: String },

    #[error(
        "Transitive set received unexpected arguments (first argument was: `{}`)",
        .first,
    )]
    UnexpectedArgument { first: String },

    #[error(
        "The requested projection `{}` does not exist. Valid projections: {}",
        .projection,
        .valid_projections.join(", "),
    )]
    ProjectionDoesNotExist {
        projection: String,
        valid_projections: Vec<String>,
    },

    #[error("Error evaluating transitive set projection {}", .name)]
    ProjectionError {
        name: String,

        #[source]
        error: anyhow::Error,
    },

    #[error("Expected args_projection `{}` function to take a single argument", .name)]
    ProjectionSignatureError { name: String },

    #[error("Error evaluating transitive set reduction {}", .name)]
    ReductionError {
        name: String,

        #[source]
        error: anyhow::Error,
    },

    #[error(
        "The requested reduction `{}` does not exist. Valid reduction: {}",
        .reduction,
        .valid_reductions.join(", "),
    )]
    ReductionDoesNotExist {
        reduction: String,
        valid_reductions: Vec<String>,
    },
}
