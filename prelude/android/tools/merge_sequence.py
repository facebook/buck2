#!/usr/bin/python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

"""
Applies the merge sequence to the linkable_graph and module graph to produce merged libs.

The merge sequence is a list of "merge entries". Each entry is a list of "merge specs" which is a name and a list of roots (a single entry
list can appear as the value rather than a list). So something like:

```
[
    {"group1.so": [//group1:root1, //group2:root2]},
     # TODO(cjhopman): We should consider requiring a "supergroup name" here to give to all of the subgroups so we don't need to try to
     # produce a reasonable name (right now we produce an unreasonable name given that we just use the first group's name even if the
     # group isn't a dependent of it).
    [{"group2-foo.so": [//group2/foo:root]}, {"group2-bar.so": [//group2//bar:root]}
    ...
]
```

We use this sequence to assign each target in the linkable graph to a "merge group". The list is processed in order and each entry defines a
new merge group. That group consists of all the roots in the entry (for all specs in the entry) and all of the transitive dependencies of those
roots, except for nodes that have already been assigned a merge group.

We then need to split that merge group into "split groups" for four reasons: to respect module boundaries, to preserve the module dependencies
of targets in the root module, to not merge excluded libs, and to preserve spec root deps (for merge groups with multiple specs). That is, we
have these constraints:
1. targets in a split group will all be in the same module.
2. for split groups in the root module, all targets will have the same set of transitive module dependencies (including through
dependencies in other merge groups). This is required because module loads are expensive and we don't want to merge a target into a final library
that requires module loads that the target wouldn't otherwise need. For non-root modules, this is unnecessary because non-root modules can only be
loaded if their dependencies have been loaded (and so whatever we do we can't cause unnecessary loads there).
3. for a merge group with multiple specs, all targets within a split group will be transitive dependencies of the same set of merge spec roots.
4. some targets are excluded from merging (due to the merge_sequence_blocklist, or targets that cannot be packaged as assets).


Once we have those split groups there's one final thing to resolve: target dependency cycles between the split groups of this merge group (we know
that there cannot be any cycles involving previous merge groups becomes the definition of the merge groups ensures that there can be no edges from
previous ones into the current one). We cannot have target dependency cycles between the split groups because neither the build time or runtime
linker will accept that.

To ensure there are no cycles, we further split the groups into "layers". We do this by tracking the max "exit counts" each module along paths from a
node to the leafs. A "module exit count" for a path is the number of times that the path leaves that module (i.e. has an edge where a node in the module
depends on one outside the module). For the root module, we split into layers based on the exit counts for all modules, and for non-root modules only on
the exit counts for the module itself (though we still track the counts for all modules, of course). Along with the fact that the apk module construction
itself ensures that there are no cycles between modules, this ensures that there are no cycles in our split group layering.

# TODO(cjhopman): There's a possibility that we can use just the self count for the root module as well

Since excluded targets are all in their own split group of a
single item, there's no need to track exit counts for their groups since they will only ever have the one layer.

The split group layering determines the final library constituents.

We perform both the split grouping and the split group layering in a single traversal.

Once we've done the split group layering, the split group and layer identify the "final lib" for each target. The one remaining thing to do at
that point is to produce a name for each final lib. Ideally, each merge group will end up with a single library and the naming is simple. If it
ends up having multiple libraries, we need to give each a unique name. If we need to add suffixes to the merge group name in order to uniquify
the library names, we prefer that the higher level libraries have the least suffixed names (the idea here is that the name of the merge group
spec tends to relate most closely with the spec's roots, rather than with its lower level libs and so the name should reflect that).

So, the rules for suffixing are:
1. things start with their merge group spec name (subgroups will get assigned one of their dependent group spec names)
2. if a merge group is in multiple modules, each library in a non-root module will get that module name added to its name.
3. we perform a topological traversal of the final library graph, maintaining a counter of times we've encountered
a (possibly module-suffixed) name. Each encounter after the first will get suffixed with the count.
"""
from __future__ import annotations

import argparse
import json
import os
import pathlib
import re
import sys
import typing

from collections import defaultdict
from typing import Optional

Label = typing.NewType("Label", str)


class LinkableGraphNode(typing.NamedTuple):
    """
    A node in the linkable graph input
    """

    raw_target: str
    soname: str
    deps: list[Label]
    can_be_asset: bool
    labels: list[str]

    @staticmethod
    def parse(json: dict[str, object]) -> LinkableGraphNode:
        deps = [Label(x) for x in json.pop("deps")]  # pyre-ignore
        return LinkableGraphNode(deps=deps, **json)  # pyre-ignore


class MergemapInput(typing.NamedTuple):
    """
    Parsed form of the mergemap input file
    """

    merge_sequence: list[MergeSequenceGroupSpec]
    blocklist: list[re.Pattern[str]]
    nodes_by_platform: dict[str, dict[Label, LinkableGraphNode]]

    @staticmethod
    def parse(mergemap_input: dict[str, typing.Any]) -> MergemapInput:
        merge_sequence = [
            MergeSequenceGroupSpec(x)
            for x in mergemap_input["native_library_merge_sequence"]
        ]
        blocklist = [
            re.compile(x)
            for x in mergemap_input["native_library_merge_sequence_blocklist"]
        ]
        nodes_by_platform = {}
        for platform, linkable_graph_spec in mergemap_input[
            "linkable_graphs_by_platform"
        ].items():
            nodes = {}
            for target, node_data in linkable_graph_spec.items():
                target = Label(target)
                node_data = LinkableGraphNode.parse(node_data)
                nodes[target] = node_data
            nodes_by_platform[platform] = nodes

        return MergemapInput(
            merge_sequence=merge_sequence,
            blocklist=blocklist,
            nodes_by_platform=nodes_by_platform,
        )


class ApkModuleGraph:
    """
    Parsed form of the optional apk module graph input
    """

    def __init__(self, target_to_module_mapping: Optional[dict[str, str]]) -> None:
        self.target_to_module_mapping = target_to_module_mapping

    def module_for_target(self, target: str) -> str:
        if self.target_to_module_mapping:
            module = self.target_to_module_mapping[target]
            return module
        else:
            return ROOT_MODULE


class SplitGroupKey(typing.NamedTuple):
    """
    Identifies a single "split group"
    """

    # If excluded, this holds the targets label
    excluded: typing.Optional[Label]
    module: str
    current_merge_group: int
    # If this is a subgroup, holds the primary spec name and a unique identifier.
    merge_subgroup: Optional[set[str]]
    # For the root module, holds the structified set of transitively reachable modules
    transitive_module_key: typing.Optional[frozenset[str]]


class NodeData(typing.NamedTuple):
    base_library_name: str
    module: str
    merge_group: int
    is_excluded: bool
    final_lib_key: FinalLibKey
    transitive_module_deps: frozenset[str]
    split_group_exit_counts: dict[int, int]

    def debug(self) -> object:
        return self._asdict()


class FinalLibKey(typing.NamedTuple):
    split_group: int
    cycle_breaker: typing.Hashable


class FinalLibData(typing.NamedTuple):
    module: str
    merge_group: int
    # this will be either the merge group spec name or come from the label for an excluded lib.
    base_library_name: str
    is_excluded: bool
    key: FinalLibKey
    deps: set[FinalLibKey]


class FinalLibGraph:
    """The "final lib graph" is traversed to produce the final lib names"""

    graph: dict[FinalLibKey, FinalLibData]

    def __init__(self) -> None:
        self.graph = {}

    def add_node(self, node_data: NodeData, deps_data: list[NodeData]) -> None:
        lib_key = node_data.final_lib_key
        lib_data = self.graph.get(lib_key, None)
        if not lib_data:
            lib_data = self.graph.setdefault(
                lib_key,
                FinalLibData(
                    module=node_data.module,
                    merge_group=node_data.merge_group,
                    base_library_name=node_data.base_library_name,
                    is_excluded=node_data.is_excluded,
                    key=lib_key,
                    deps=set(),
                ),
            )
        else:
            assert lib_data.module == node_data.module, (lib_data, node_data)
            assert lib_data.merge_group == node_data.merge_group, (lib_data, node_data)

        for dep_data in deps_data:
            if dep_data.final_lib_key != lib_key:
                lib_data.deps.add(dep_data.final_lib_key)

    def dump_graph(self, names: dict[FinalLibKey, str]) -> dict[str, list[str]]:
        return {
            names[k]: [names[d] for d in node.deps] for k, node in self.graph.items()
        }

    def assign_names(
        self, merge_group_module_constituents: list[set[str]]
    ) -> dict[FinalLibKey, str]:
        final_lib_graph = {}
        for key, dep_data in self.graph.items():
            final_lib_graph[key] = list(dep_data.deps)

        # this topo_sort also verifies that we produced an acyclic final lib graph
        sorted_final_lib_keys = topo_sort(final_lib_graph)

        name_counters = {}
        final_lib_names: dict[FinalLibKey, str] = {}
        for key in sorted_final_lib_keys:
            dep_data = self.graph[key]
            if dep_data.is_excluded:
                final_lib_names[key] = dep_data.base_library_name
            else:
                lib_name, ext = os.path.splitext(dep_data.base_library_name)
                if len(
                    merge_group_module_constituents[dep_data.merge_group]
                ) > 1 and not is_root_module(dep_data.module):
                    lib_name += "_" + dep_data.module

                count = name_counters.setdefault(lib_name, 0) + 1
                name_counters[lib_name] = count
                if count > 1:
                    lib_name += "_{}".format(count)
                final_lib_names[key] = lib_name + ext
        return final_lib_names


# Older python has poor typing support, so we need to use coarser types there (but pyre wants the exact ones)
if sys.version_info >= (3, 9):
    MergeSubgroupMapping = typing.Callable[[Label], typing.Optional[set[str]]]
    MergeGroupSpecDef = typing.Tuple[str, list[str]]
else:
    MergeSubgroupMapping = typing.Callable
    MergeGroupSpecDef = typing.Tuple


class MergeSequenceGroupSpec:
    has_multiple_specs: bool
    group_roots_patterns: list[re.Pattern[str]]
    merge_group_name: str

    def __init__(
        self, group_specs: typing.Union[MergeGroupSpecDef, list[MergeGroupSpecDef]]
    ) -> None:
        self.group_specs = group_specs

        def _parse() -> typing.Tuple[bool, str, list[str]]:
            if isinstance(group_specs[0], str):
                typed = typing.cast(MergeGroupSpecDef, group_specs)
                return (False, typed[0], typed[1])
            else:
                group_roots_patterns = [x for spec in group_specs for x in spec[1]]
                # TODO(cjhopman): Fix this, we don't yet actually fully support multiple specs in a group.
                return (True, group_specs[0][0], group_roots_patterns)

        has_multiple_specs, merge_group_name, group_roots_patterns = _parse()

        self.has_multiple_specs = has_multiple_specs
        self.group_roots_patterns = [re.compile(p) for p in group_roots_patterns]
        self.merge_group_name = merge_group_name

    def is_root(self, raw_target: str) -> bool:
        for p in self.group_roots_patterns:
            if p.search(raw_target):
                return True
        return False

    def group_name(self) -> str:
        return self.merge_group_name

    def compute_merge_subgroup_mapping(
        self,
        post_ordered_targets: list[Label],
        graph_map: dict[Label, LinkableGraphNode],
    ) -> MergeSubgroupMapping:
        if self.has_multiple_specs:
            reachable_merge_spec_roots = defaultdict(set)

            for s in self.group_specs:
                for root in s[1]:
                    reachable_merge_spec_roots[root].add(root)

            for target in post_ordered_targets[::-1]:
                node = graph_map.get(target)
                assert node is not None
                roots_to_add = reachable_merge_spec_roots[target]
                for child in node.deps:
                    reachable_merge_spec_roots[child].update(roots_to_add)

            return lambda x: reachable_merge_spec_roots[x]
        else:
            return lambda x: None


def get_native_linkables_by_merge_sequence(  # noqa: C901
    graph_node_map: dict[Label, LinkableGraphNode],
    native_library_merge_sequence: list[MergeSequenceGroupSpec],
    native_library_merge_sequence_blocklist: list[typing.Pattern],
    apk_module_graph: ApkModuleGraph,
) -> typing.Tuple[dict[Label, NodeData], dict[FinalLibKey, str], FinalLibGraph]:
    final_lib_graph = FinalLibGraph()
    node_data: dict[Label, NodeData] = {}

    def check_is_excluded(target: Label) -> bool:
        node = graph_node_map[target]
        if not node.can_be_asset:
            return True

        raw_target = node.raw_target
        for x in native_library_merge_sequence_blocklist:
            if x.search(raw_target):
                return True

        # TODO(cjhopman): implement the thing about wrap script, possibly by just having that mark it as can_be_asset=False
        return False

    def get_children_without_merge_group(label: Label) -> list[Label]:
        node = graph_node_map[label]
        return [child for child in node.deps if child not in node_data]

    current_merge_group = 0

    split_groups: dict[SplitGroupKey, int] = {}

    merge_group_module_constituents: list[set[str]] = []

    for current_merge_group in range(len(native_library_merge_sequence)):
        merge_group_module_constituents.append(set())
        group_specs: MergeSequenceGroupSpec = native_library_merge_sequence[
            current_merge_group
        ]
        group_roots = []
        for label, node in graph_node_map.items():
            if group_specs.is_root(node.raw_target) and label not in node_data:
                group_roots.append(label)

        merge_group_name = group_specs.merge_group_name
        post_ordered_targets = post_order_traversal_by(
            group_roots, get_children_without_merge_group
        )

        # While most of the restrictions we enforce can be computed in a single bottom-up traversal, restriction (3) requires traversing down
        # the graph to determine the set of transitive dependent spec roots, and so the computation of that is done first here.
        merge_subgroup_mapping = group_specs.compute_merge_subgroup_mapping(
            post_ordered_targets, graph_node_map
        )

        def get_split_group(
            label: Label,
            transitive_module_deps: frozenset[str],
            module: str,
            current_merge_group: int = current_merge_group,
            merge_subgroup_mapping: MergeSubgroupMapping = merge_subgroup_mapping,
        ) -> typing.Tuple[bool, int]:
            excluded = None
            if check_is_excluded(label):
                excluded = label

            merge_subgroup = merge_subgroup_mapping(label)

            if is_root_module(module):
                transitive_module_key = transitive_module_deps
            else:
                transitive_module_key = None

            split_group_key = SplitGroupKey(
                excluded=excluded,
                module=module,
                current_merge_group=current_merge_group,
                merge_subgroup=merge_subgroup,
                transitive_module_key=transitive_module_key,
            )
            return excluded is not None, split_groups.setdefault(
                split_group_key, len(split_groups)
            )

        for target in post_ordered_targets:
            assert target not in node_data, "{}: {}".format(
                target, post_ordered_targets
            )

            node = graph_node_map[target]
            module = apk_module_graph.module_for_target(node.raw_target)
            merge_group_module_constituents[current_merge_group].add(module)

            transitive_module_deps = {module}

            deps_data = [node_data[dep] for dep in node.deps]

            for dep_data in deps_data:
                transitive_module_deps.update(dep_data.transitive_module_deps)

            transitive_module_deps = frozenset(transitive_module_deps)

            is_excluded, split_group = get_split_group(
                target, transitive_module_deps, module
            )

            split_group_exit_counts: dict[int, int] = {}

            for dep_data in deps_data:
                if current_merge_group == dep_data.merge_group:
                    dep_split_group = dep_data.final_lib_key.split_group
                    is_cross_group_edge = split_group != dep_split_group

                    # if this is the first exit edge from the group, it won't apper in dep_data's map so we add it
                    # explicitly if we don't yet have a non-zero count (except if it's exited an excluded node
                    # where there's no need to track exit counts)
                    if (
                        not dep_data.is_excluded
                        and is_cross_group_edge
                        and dep_split_group not in split_group_exit_counts
                    ):
                        split_group_exit_counts[dep_split_group] = 1
                    for (group, count) in dep_data.split_group_exit_counts.items():
                        if group == dep_split_group and is_cross_group_edge:
                            count += 1
                        curr_count = split_group_exit_counts.get(group, 0)
                        if count > curr_count:
                            split_group_exit_counts[group] = count

            this_node_data = NodeData(
                base_library_name=node.raw_target if is_excluded else merge_group_name,
                module=module,
                merge_group=current_merge_group,
                final_lib_key=FinalLibKey(
                    split_group=split_group,
                    cycle_breaker=frozenset(split_group_exit_counts.items())
                    if is_root_module(module)
                    else split_group_exit_counts.get(split_group, 0),
                ),
                is_excluded=is_excluded,
                transitive_module_deps=transitive_module_deps,
                split_group_exit_counts=split_group_exit_counts,
            )
            node_data[target] = this_node_data
            final_lib_graph.add_node(this_node_data, deps_data)

    final_lib_names = final_lib_graph.assign_names(merge_group_module_constituents)
    return node_data, final_lib_names, final_lib_graph


T = typing.TypeVar("T")


def post_order_traversal_by(
    roots: list[T], get_nodes_to_traverse_func: typing.Callable[[T], list[T]]
) -> list[T]:
    """
    Returns the post-order sorted list of the nodes in the traversal.

    This implementation simply performs a dfs. We maintain a work stack here.
    When visiting a node, we first add an item to the work stack to output that
    node, and then add items to visit all the children. While a work item for a
    child will not be added if it has already been visited, if there's an item in
    the stack for that child it will still be added. When popping the visit, if
    the node had been visited, it's ignored. This ensures that a node's children are
    all visited before we output that node.
    """
    ordered = []
    visited = {}
    OUTPUT = 1
    VISIT = 2
    current_parents = []
    work = [(VISIT, n) for n in roots]
    while work:
        kind, node = work.pop()
        if kind == VISIT:
            if node not in visited:
                visited[node] = True
                current_parents.append(node)

                work.append((OUTPUT, node))
                for dep in get_nodes_to_traverse_func(node):
                    if dep in current_parents:
                        raise AssertionError(
                            "detected cycle: {}".format(
                                " -> ".join(current_parents + [dep])
                            )
                        )

                    if dep not in visited:
                        work.append((VISIT, dep))
        else:
            ordered.append(node)
            current_parents.pop()
    return ordered


ROOT_MODULE = "dex"


def is_root_module(module: str) -> bool:
    return module == ROOT_MODULE


def topo_sort(graph: dict[T, list[T]]) -> list[T]:
    """
    Topo-sort the given graph.
    """
    in_degrees = {node: 0 for node in graph}
    for _node, deps in graph.items():
        assert len(deps) == len(set(deps))
        for dep in deps:
            in_degrees[dep] += 1

    roots = []

    for node, in_degree in in_degrees.items():
        if in_degree == 0:
            roots.append(node)

    postordered = post_order_traversal_by(roots, lambda x: graph[x])
    postordered.reverse()

    return postordered


def read_apk_module_graph(path: Optional[str]) -> ApkModuleGraph:
    if not path:
        return ApkModuleGraph(None)

    target_to_module_mapping = {}
    # the format of this file is, the first line contains an integer, then follows that many lines describing dependencies
    # between modules. Each line is of the form: <module_name> <dep_name1> <dep_name2> ...
    # after that, all target->module mappings are listed, one on each line.
    # Each line has the form: <raw_target> <module_name>
    with open(path) as modules_in:
        lines = modules_in.read().splitlines()
        module_lines = int(lines[0])
        for line in lines[module_lines + 1 :]:
            target, module = line.split()
            target_to_module_mapping[target] = module
    return ApkModuleGraph(target_to_module_mapping)


def read_mergemap_input(path: str) -> MergemapInput:
    with open(path) as mergemap_input:
        mergemap_input = json.load(mergemap_input)
        return MergemapInput.parse(mergemap_input)


def main() -> int:  # noqa: C901
    parser = argparse.ArgumentParser()
    parser.add_argument("--mergemap-input", required=True)
    parser.add_argument("--apk-module-graph")
    parser.add_argument("--output")
    args = parser.parse_args()

    apk_module_graph = read_apk_module_graph(args.apk_module_graph)

    final_result = {}
    debug_results = {}
    mergemap_input = read_mergemap_input(args.mergemap_input)
    for platform, nodes in mergemap_input.nodes_by_platform.items():
        (
            node_data,
            final_lib_names,
            final_lib_graph,
        ) = get_native_linkables_by_merge_sequence(
            nodes,
            mergemap_input.merge_sequence,
            mergemap_input.blocklist,
            apk_module_graph,
        )

        final_mapping = {}
        for target in nodes.keys():
            if target in node_data:
                node = node_data[target]
                if node.is_excluded:
                    final_mapping[target] = None
                else:
                    final_mapping[target] = final_lib_names[node.final_lib_key]
            else:
                final_mapping[target] = str(target)
        debug_results[platform] = (
            {k: v.debug() for k, v in node_data.items()},
            {str(k): v for k, v in final_lib_names.items()},
            final_lib_graph.dump_graph(final_lib_names),
        )
        final_result[platform] = final_mapping

    if args.output:
        pathlib.Path(args.output).mkdir(parents=True, exist_ok=True)
        with open(os.path.join(args.output, "merge.map"), "w") as outfile:
            json.dump(final_result, outfile, indent=2)

        # When writing an output dir we also produce some debugging information.
        for platform, result in final_result.items():

            def set_default(obj: object) -> object:
                if isinstance(obj, frozenset):
                    return list(obj)
                raise TypeError

            with open(
                os.path.join(args.output, "{}.debug".format(platform)), "w"
            ) as outfile:
                json.dump(
                    debug_results[platform], outfile, indent=2, default=set_default
                )

            # The "inverted" map just provides a much simpler human-readable form of the merge mapping of the form:
            # libfoo.so
            #    target1
            #    target2
            # libfoo2.so
            # ...
            inverted = defaultdict(set)
            for label, lib in result.items():
                if not lib:
                    lib = label
                inverted[lib].add(label)
            with open(
                os.path.join(args.output, "{}.inverted".format(platform)),
                "w",
            ) as outfile:
                lines = []
                for lib in sorted(inverted.keys()):
                    lines.append(lib + "\n")
                    for label in sorted(inverted[lib]):
                        lines.append("  " + str(label) + "\n")
                outfile.writelines(lines)
    else:
        json.dump(final_result, sys.stdout, indent=2)

    return 0


if __name__ == "__main__":
    sys.exit(main())
