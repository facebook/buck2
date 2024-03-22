# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


# pyre-fixme[21]: Could not find name `BuildMapLoadError` in `tests.inputs_test`.
# pyre-fixme[21]: Could not find name `PartialBuildMap` in `tests.inputs_test`.
# pyre-fixme[21]: Could not find name `Target` in `tests.inputs_test`.
# pyre-fixme[21]: Could not find name `TargetEntry` in `tests.inputs_test`.
# pyre-fixme[21]: Could not find name `load_targets_and_build_maps_from_json` in
#  `tests.inputs_test`.
from .inputs_test import *  # noqa

# pyre-fixme[21]: Could not find name `ConflictInfo` in `tests.legacy_output_test`.
# pyre-fixme[21]: Could not find name `ConflictMap` in `tests.legacy_output_test`.
# pyre-fixme[21]: Could not find name `FullBuildMap` in `tests.legacy_output_test`.
# pyre-fixme[21]: Could not find name `MergeResult` in `tests.legacy_output_test`.
# pyre-fixme[21]: Could not find name `PartialBuildMap` in `tests.legacy_output_test`.
# pyre-fixme[21]: Could not find name `SourceInfo` in `tests.legacy_output_test`.
# pyre-fixme[21]: Could not find name `Target` in `tests.legacy_output_test`.
# pyre-fixme[21]: Could not find name `TargetEntry` in `tests.legacy_output_test`.
# pyre-fixme[21]: Could not find name `merge_partial_build_maps` in
#  `tests.legacy_output_test`.
from .legacy_output_test import *  # noqa

# pyre-fixme[21]: Could not find name `PartialBuildMap` in `tests.outputs_test`.
# pyre-fixme[21]: Could not find name `Target` in `tests.outputs_test`.
# pyre-fixme[21]: Could not find name `TargetEntry` in `tests.outputs_test`.
# pyre-fixme[21]: Could not find name `merge_partial_build_maps` in
#  `tests.outputs_test`.
from .outputs_test import *  # noqa
