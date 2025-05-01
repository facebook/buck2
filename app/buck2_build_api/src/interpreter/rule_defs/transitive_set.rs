/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod globals;
mod transitive_set;
mod transitive_set_args_projection;
pub mod transitive_set_definition;
mod transitive_set_iterator;
mod transitive_set_json_projection;
mod traversal;

pub use self::transitive_set::FrozenTransitiveSet;
pub use self::transitive_set::TransitiveSet;
use self::transitive_set::TransitiveSetGen;
pub use self::transitive_set::TransitiveSetLike;
pub use self::transitive_set_args_projection::FrozenTransitiveSetArgsProjection;
pub use self::transitive_set_args_projection::TransitiveSetArgsProjection;
pub use self::transitive_set_definition::FrozenTransitiveSetDefinition;
pub use self::transitive_set_definition::TransitiveSetDefinition;
pub use self::transitive_set_definition::TransitiveSetOperations;
pub use self::transitive_set_definition::TransitiveSetProjectionKind;
pub use self::transitive_set_definition::TransitiveSetProjectionSpec;
use self::transitive_set_iterator::BfsTransitiveSetIteratorGen;
use self::transitive_set_iterator::PostorderTransitiveSetIteratorGen;
use self::transitive_set_iterator::PreorderTransitiveSetIteratorGen;
use self::transitive_set_iterator::TopologicalTransitiveSetIteratorGen;
use self::transitive_set_iterator::TransitiveSetIteratorLike;
pub use self::transitive_set_json_projection::FrozenTransitiveSetJsonProjection;
pub use self::transitive_set_json_projection::TransitiveSetJsonProjection;
pub use self::traversal::TransitiveSetOrdering;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub(crate) enum TransitiveSetError {
    #[error(
        "Transitive set type must be assigned to a top-level variable, e.g. `MySet = transitive_set()`"
    )]
    TransitiveSetNotAssigned,

    #[error(
        "Transitive set was used before being assigned to a top-level variable, e.g. `MySet = transitive_set()`"
    )]
    TransitiveSetUsedBeforeAssignment,

    #[error("Transitive set transitive values must be transitive sets, got `{}`", .got)]
    TransitiveValueIsNotTransitiveSet { got: String },

    #[error(
        "Transitive set transitive values must be of the same transitive set type (expected: `{}`, got: `{}`)",
        .expected,
        .got
    )]
    TransitiveValueIsOfWrongType { expected: String, got: String },

    #[error(
        "The requested projection `{}` does not exist. Valid projections: {}",
        .projection,
        .valid_projections.join(", "),
    )]
    ProjectionDoesNotExist {
        projection: String,
        valid_projections: Vec<String>,
    },

    #[error(
        "Requested a {} projection, but `{}` is a `{}` projection (and should use `{}` instead)",
        .expected_kind.short_name(),
        .projection,
        .actual_kind.short_name(),
        .actual_kind.function_name(),
    )]
    ProjectionKindMismatch {
        projection: String,
        expected_kind: TransitiveSetProjectionKind,
        actual_kind: TransitiveSetProjectionKind,
    },

    #[error("Error evaluating transitive set projection {}", .name)]
    ProjectionError {
        name: String,

        #[source]
        error: buck2_error::Error,
    },

    #[error("Error evaluating transitive set reduction {}", .name)]
    ReductionError {
        name: String,

        #[source]
        error: buck2_error::Error,
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

    #[error("Expected ordering to be one of `preorder`, `postorder`, `topological`, or `bfs`,  but got `{0}`", .ordering)]
    OrderingUnexpectedValue { ordering: String },
}
