# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//apple:apple_bundle_resources.bzl",
    "AppleBundleResourcePartListOutput",  # @unused Used as a type
    "get_apple_bundle_resource_part_list",
)
load("@prelude//apple:apple_bundle_types.bzl", "AppleBundleResourceInfo")

def _extract_all_resources(part_list: AppleBundleResourcePartListOutput) -> list[Artifact]:
    all_resources = [resource_bundle_part.source for resource_bundle_part in part_list.resource_parts]
    all_resources.append(part_list.info_plist_part.source)
    return all_resources

def apple_resource_bundle_impl(ctx: AnalysisContext) -> list[Provider]:
    resource_output = get_apple_bundle_resource_part_list(ctx)

    all_resources = _extract_all_resources(resource_output)
    all_resources_json = {"resources": all_resources}
    all_resources_json_file = ctx.actions.declare_output("resources.json")
    all_resources_json_cmd_args = ctx.actions.write_json(
        all_resources_json_file,
        all_resources_json,
        with_inputs = True,
        pretty = True,
    )

    return [
        DefaultInfo(sub_targets = {
            "resources": [DefaultInfo(
                default_output = all_resources_json_file,
                other_outputs = [all_resources_json_cmd_args],
            )],
        }),
        AppleBundleResourceInfo(
            resource_output = resource_output,
        ),
    ]
