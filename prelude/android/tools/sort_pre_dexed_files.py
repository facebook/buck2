# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

"""
Sorts pre-dexed libraries into primary/secondary dex groups for an Android APK.

This tool replaces the _sort_pre_dexed_files Starlark function that previously
ran inside the merge_pre_dexed_libs dynamic_output lambda, taking ~3-4 seconds
in the Starlark interpreter. By running as a compiled Python tool, this
completes in ~10-50ms.

It reads:
  - All filter_dex JSON outputs (one per batch)
  - The APK module graph file (for Voltron builds)
  - A lib metadata JSON mapping identifier -> owner target label

It outputs a dex_plan.json that the simplified lambda reads to declare
merge_dexes actions.
"""

from __future__ import annotations

import argparse
import json
import pathlib
from typing import Optional, TypedDict


ROOT_MODULE: str = "dex"

# DEX 64K limit enforcement.
#
# Each DEX file is limited to 65536 method_ids, field_ids, and type_ids.
# Per-library ref counts from the DEX header are summed as a conservative
# upper bound — the actual merged DEX has fewer refs because shared
# dependencies are deduplicated. For example, merging two libs:
#
#   LibA: methods=9  fields=3  types=9
#   LibB: methods=10 fields=3  types=10
#   Sum:  methods=19 fields=6  types=19
#   Merged DEX:  methods=17 fields=6  types=13  (shared types deduped)
#
# The merge step uses ["--no-desugar", "--no-optimize"], so D8 performs a
# pure mechanical merge with no synthetic generation. Merged refs are
# always <= sum of input refs, never more.
DEX_REF_LIMIT: int = 65536

# Type aliases for the data structures used throughout.
# A single entry in a dex group: {"id": <lib_id>, "class_names": [<class>, ...]}
DexEntry = dict[str, object]
# A group is a list of DexEntry dicts that will be merged into one .dex file.
DexGroup = list[DexEntry]
# Per-module sorting result before flattening.
ModuleSortResult = dict[str, list[DexGroup]]


class FilterDexInfo(TypedDict):
    primary_dex_class_names: list[str]
    secondary_dex_class_names: list[str]
    weight_estimate: str
    method_ref_count: str
    field_ref_count: str
    type_ref_count: str


# Filter dex data: identifier -> filter info
FilterDexData = dict[str, FilterDexInfo]
# Lib metadata: identifier -> owner target label
LibMetadata = dict[str, str]
# Target to module mapping
TargetToModule = dict[str, str]
# Module metadata: module_name -> {"canary_class_name": ..., "module_deps": [...]}
ModuleMetadata = dict[str, dict[str, object]]
# Flattened group for JSON output: {"lib_ids": [...], "class_names": [...]}
FlatGroup = dict[str, list[str]]


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Sort pre-dexed files into dex merge groups.",
        fromfile_prefix_chars="@",
    )
    parser.add_argument(
        "--filter-dex-outputs",
        type=pathlib.Path,
        nargs="+",
        required=True,
        help="Paths to filter_dex JSON output files",
    )
    parser.add_argument(
        "--lib-metadata",
        type=pathlib.Path,
        required=True,
        help="JSON file mapping identifier -> owner target label",
    )
    parser.add_argument(
        "--module-graph",
        type=pathlib.Path,
        default=None,
        help="APK module graph file (for Voltron builds)",
    )
    parser.add_argument(
        "--weight-limit",
        type=int,
        required=True,
        help="Secondary dex weight limit in bytes",
    )
    parser.add_argument(
        "--enable-bootstrap-dexes",
        action="store_true",
        default=False,
        help="Whether to enable bootstrap dexes",
    )
    parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="Output dex plan JSON file",
    )
    return parser.parse_args()


def _parse_module_graph(
    module_graph_path: Optional[pathlib.Path],
) -> tuple[TargetToModule, ModuleMetadata]:
    """Parse the APK module graph file, returning target-to-module mapping and module metadata."""
    if module_graph_path is None:
        return {}, {}

    with open(module_graph_path) as f:
        lines = f.read().split("\n")

    module_count = int(lines[0])
    module_infos = lines[1 : module_count + 1]
    target_to_module_lines = lines[module_count + 1 : -1]

    target_to_module: TargetToModule = {}
    module_metadata: ModuleMetadata = {}

    for line in module_infos:
        parts = line.split(" ")
        module_name = parts[0]
        canary_class_name = parts[1]
        module_deps = [dep for dep in parts[2:] if dep]
        module_metadata[module_name] = {
            "canary_class_name": canary_class_name,
            "module_deps": module_deps,
        }

    for line in target_to_module_lines:
        if not line:
            continue
        target, module = line.split(" ")
        target_to_module[target] = module

    return target_to_module, module_metadata


def _assign_to_dex(
    dest: list[DexGroup],
    module: str,
    lib_id: str,
    weight_estimate: int,
    dex_class_names: list[str],
    current_size_map: dict[str, int],
    current_inputs_map: dict[str, DexGroup],
    dex_weight_limit: Optional[int],
    method_ref_count: int,
    field_ref_count: int,
    type_ref_count: int,
    current_method_refs_map: dict[str, int],
    current_field_refs_map: dict[str, int],
    current_type_refs_map: dict[str, int],
) -> None:
    """Assign a lib's classes to a dex group, starting a new group if limits exceeded."""
    if len(dex_class_names) == 0:
        return

    current_size = current_size_map.get(module, 0)
    should_start_new_dex = False

    # Check weight-based limit
    if (
        dex_weight_limit is not None
        and current_size + weight_estimate > dex_weight_limit
    ):
        should_start_new_dex = True

    # Check ref-count-based limits (DEX 64K limits for methods, fields, and types)
    current_methods = current_method_refs_map.get(module, 0)
    current_fields = current_field_refs_map.get(module, 0)
    current_types = current_type_refs_map.get(module, 0)
    if (
        current_methods + method_ref_count > DEX_REF_LIMIT
        or current_fields + field_ref_count > DEX_REF_LIMIT
        or current_types + type_ref_count > DEX_REF_LIMIT
    ):
        should_start_new_dex = True

    if should_start_new_dex:
        current_size = 0
        current_inputs_map[module] = []
        current_method_refs_map[module] = 0
        current_field_refs_map[module] = 0
        current_type_refs_map[module] = 0

    current_inputs = current_inputs_map.setdefault(module, [])
    if len(current_inputs) == 0:
        dest.append(current_inputs)

    current_size_map[module] = current_size + weight_estimate
    current_method_refs_map[module] = (
        current_method_refs_map.get(module, 0) + method_ref_count
    )
    current_field_refs_map[module] = (
        current_field_refs_map.get(module, 0) + field_ref_count
    )
    current_type_refs_map[module] = (
        current_type_refs_map.get(module, 0) + type_ref_count
    )
    current_inputs.append({"id": lib_id, "class_names": dex_class_names})


def _organize_lib(
    dest: list[DexGroup],
    module: str,
    lib_id: str,
    weight_estimate: int,
    dex_class_names: list[str],
    current_size_map: dict[str, int],
    current_inputs_map: dict[str, DexGroup],
    dex_weight_limit: Optional[int],
    method_ref_count: int,
    field_ref_count: int,
    type_ref_count: int,
    current_method_refs_map: dict[str, int],
    current_field_refs_map: dict[str, int],
    current_type_refs_map: dict[str, int],
) -> None:
    """Organize a lib into dex groups, chunking classes if weight or ref count exceeds the limit."""
    if len(dex_class_names) == 0:
        return

    should_start_new_dex = False
    if dex_weight_limit is not None and weight_estimate > dex_weight_limit:
        should_start_new_dex = True
    if (
        method_ref_count > DEX_REF_LIMIT
        or field_ref_count > DEX_REF_LIMIT
        or type_ref_count > DEX_REF_LIMIT
    ):
        should_start_new_dex = True

    if should_start_new_dex:
        num_classes = len(dex_class_names)
        if dex_weight_limit is not None and weight_estimate > dex_weight_limit:
            chunks = weight_estimate / dex_weight_limit
        else:
            # Subdivide based on ref counts: use the more constrained dimension
            max_refs = max(method_ref_count, field_ref_count, type_ref_count)
            chunks = max_refs / DEX_REF_LIMIT
        chunk_size = max(1, int(num_classes // chunks))
        for start_index in range(0, num_classes, chunk_size):
            end_index = min(start_index + chunk_size, num_classes)
            chunked = dex_class_names[start_index:end_index]
            _assign_to_dex(
                dest,
                module,
                lib_id,
                weight_estimate,
                chunked,
                current_size_map,
                current_inputs_map,
                dex_weight_limit,
                method_ref_count,
                field_ref_count,
                type_ref_count,
                current_method_refs_map,
                current_field_refs_map,
                current_type_refs_map,
            )
    else:
        _assign_to_dex(
            dest,
            module,
            lib_id,
            weight_estimate,
            dex_class_names,
            current_size_map,
            current_inputs_map,
            dex_weight_limit,
            method_ref_count,
            field_ref_count,
            type_ref_count,
            current_method_refs_map,
            current_field_refs_map,
            current_type_refs_map,
        )


def _sort_pre_dexed_files(
    filter_dex_data: FilterDexData,
    lib_metadata: LibMetadata,
    target_to_module: TargetToModule,
    weight_limit: int,
    enable_bootstrap_dexes: bool,
) -> dict[str, ModuleSortResult]:
    """
    Sort pre-dexed files into primary and secondary dex groups per module.

    This is a Python port of the _sort_pre_dexed_files Starlark function
    from dex_rules.bzl.
    """
    sorted_inputs: dict[str, ModuleSortResult] = {}

    current_primary_dex_size: dict[str, int] = {}
    current_primary_dex_inputs: dict[str, DexGroup] = {}
    current_secondary_dex_size: dict[str, int] = {}
    current_secondary_dex_inputs: dict[str, DexGroup] = {}

    # Tracking for ref-count-based splitting (DEX header counts).
    current_primary_method_refs: dict[str, int] = {}
    current_primary_field_refs: dict[str, int] = {}
    current_primary_type_refs: dict[str, int] = {}
    current_secondary_method_refs: dict[str, int] = {}
    current_secondary_field_refs: dict[str, int] = {}
    current_secondary_type_refs: dict[str, int] = {}

    # Process libs in the same order as the original Starlark code, which iterates
    # pre_dexed_libs in order. lib_metadata preserves this order (written from
    # pre_dexed_libs iteration). We iterate lib_metadata keys and look up filter data.
    for identifier in lib_metadata:
        filter_info = filter_dex_data.get(identifier)
        if filter_info is None:
            continue
        owner_target = lib_metadata[identifier]

        module = target_to_module.get(owner_target, ROOT_MODULE)
        if module not in sorted_inputs:
            sorted_inputs[module] = {
                "primary_groups": [],
                "secondary_groups": [],
            }

        primary_dex_class_names: list[str] = filter_info["primary_dex_class_names"]
        secondary_dex_class_names: list[str] = filter_info["secondary_dex_class_names"]
        weight_estimate = int(filter_info["weight_estimate"])

        # Exact DEX ref counts from the header. Zero is valid for resource-only
        # AARs (no code), which don't consume any 64K ref slots.
        lib_method_refs = int(filter_info["method_ref_count"])
        lib_field_refs = int(filter_info["field_ref_count"])
        lib_type_refs = int(filter_info["type_ref_count"])

        module_inputs = sorted_inputs[module]

        if len(primary_dex_class_names) > 0 and module != ROOT_MODULE:
            # Non-root modules shouldn't have primary dex classes; move to secondary
            secondary_dex_class_names = (
                secondary_dex_class_names + primary_dex_class_names
            )
            primary_dex_class_names = []

        # Organize primary dex classes
        _organize_lib(
            module_inputs["primary_groups"],
            module,
            identifier,
            weight_estimate,
            primary_dex_class_names,
            current_primary_dex_size,
            current_primary_dex_inputs,
            weight_limit if enable_bootstrap_dexes else None,
            lib_method_refs,
            lib_field_refs,
            lib_type_refs,
            current_primary_method_refs,
            current_primary_field_refs,
            current_primary_type_refs,
        )

        # Organize secondary dex classes
        _organize_lib(
            module_inputs["secondary_groups"],
            module,
            identifier,
            weight_estimate,
            secondary_dex_class_names,
            current_secondary_dex_size,
            current_secondary_dex_inputs,
            weight_limit,
            lib_method_refs,
            lib_field_refs,
            lib_type_refs,
            current_secondary_method_refs,
            current_secondary_field_refs,
            current_secondary_type_refs,
        )

    return sorted_inputs


def _flatten_groups(groups: list[DexGroup]) -> list[FlatGroup]:
    """Convert list of [{id, class_names}, ...] to {lib_ids: [...], class_names: [...]}."""
    result: list[FlatGroup] = []
    for group in groups:
        lib_ids: list[str] = [str(entry["id"]) for entry in group]
        class_names: list[str] = []
        for entry in group:
            # pyre-ignore[6]: entry["class_names"] is always list[str]
            class_names.extend(entry["class_names"])
        result.append({"lib_ids": lib_ids, "class_names": class_names})
    return result


def main() -> None:
    args = _parse_args()

    # Read all filter_dex JSON outputs and merge them
    merged_filter_data: FilterDexData = {}
    for filter_output_path in args.filter_dex_outputs:
        with open(filter_output_path) as f:
            batch_data = json.load(f)
        merged_filter_data.update(batch_data)

    # Read lib metadata (identifier -> owner target label)
    with open(args.lib_metadata) as f:
        lib_metadata: LibMetadata = json.load(f)

    # Parse module graph
    target_to_module, module_metadata = _parse_module_graph(args.module_graph)

    # Sort pre-dexed files into groups
    sorted_inputs = _sort_pre_dexed_files(
        merged_filter_data,
        lib_metadata,
        target_to_module,
        args.weight_limit,
        args.enable_bootstrap_dexes,
    )

    # Flatten groups for JSON output
    modules_output = []
    for module, data in sorted_inputs.items():
        modules_output.append(
            {
                "module": module,
                "primary_groups": _flatten_groups(data["primary_groups"]),
                "secondary_groups": _flatten_groups(data["secondary_groups"]),
            }
        )

    plan = {
        "modules": modules_output,
        "module_metadata": module_metadata,
    }

    with open(args.output, "w") as f:
        json.dump(plan, f)


if __name__ == "__main__":
    main()
