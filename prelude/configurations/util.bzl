# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _configuration_info(constraints, values, root_values = None):
    # TODO(scottcao): Pass `root_values` directly once the minimum Buck2 binary
    # version includes `ConfigurationInfo.root_values`. Until then, omit empty
    # `root_values` kwargs and use `getattr` below so old binaries can still
    # load this prelude.
    kwargs = {}
    if root_values:
        kwargs["root_values"] = root_values
    return ConfigurationInfo(constraints = constraints, values = values, **kwargs)

def _configuration_info_union(infos):
    if len(infos) == 0:
        return _configuration_info(
            constraints = {},
            values = {},
        )
    if len(infos) == 1:
        return infos[0]
    constraints = {k: v for info in infos for (k, v) in info.constraints.items()}
    values = {k: v for info in infos for (k, v) in info.values.items()}
    root_values = {}
    for info in infos:
        rv = getattr(info, "root_values", {})
        for k, v in rv.items():
            root_values[k] = v
    return _configuration_info(
        constraints = constraints,
        values = values,
        root_values = root_values,
    )

def _constraint_values_to_configuration(values):
    return _configuration_info(
        constraints = {info[ConstraintValueInfo].setting.label: info[ConstraintValueInfo] for info in values},
        values = {},
    )

util = struct(
    configuration_info = _configuration_info,
    configuration_info_union = _configuration_info_union,
    constraint_values_to_configuration = _constraint_values_to_configuration,
)
