# Incrementality

Incrementality is the idea that given any changes of a collection of interdependent computations,
only the changed portions of the computation are recomputed. To record changes and to discover the changed portion,
portions of the dependency graph needs to be traversed to discover the changes.

DICE tracks the reverse dependencies (rdeps) of computations to achieve O(invalidated subset) traversals and O(changed subset) recomputations for a given request.
* 'invalidated subset' is the set of all possibly invalidated nodes intersected with the set of nodes that might be needed for the request regardless of whether a node is cached or not.
* 'changed subset' is the set nodes whose values changed intersected with the set of nodes that might be needed for the request regardless of whether a node is cached or not.

This allows DICE to minimize the amount of work performed for each new request.

# Multi-versioning
DICE supports a multi-commit transaction model, where computations/requests that is currently running do not see newly
committed changes. We do not yet support running transactions at different versions concurrently nor storing multiple versions
of nodes.

# Details
Details of the incrementality algorithm can be found in the [PDF](DiceIncrementalityAlgorithms.pdf).
