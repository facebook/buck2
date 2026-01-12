# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import os
from dataclasses import dataclass
from typing import Any, Dict, Type, TypeVar, Union

from buck2.tests.e2e_util.api.buck_result import BuckResult


T = TypeVar("T")


def _inner_dataclass_from_dict(
    klass: Type[T], dikt: Dict[str, Any]
) -> Union[T, Dict[str, Any]]:
    if hasattr(klass, "__annotations__") and klass.__annotations__:
        return _dataclass_from_dict(klass, dikt)
    else:
        return dikt


def _dataclass_from_dict(klass: Type[T], dikt: Dict[str, Any]) -> T:
    fieldtypes = klass.__annotations__
    result = {}
    for f, ftype in fieldtypes.items():
        if hasattr(ftype, "__origin__"):
            if ftype.__origin__ is list:
                # Handle list type
                inner_type = ftype.__args__[0]
                result[f] = [
                    _inner_dataclass_from_dict(inner_type, item)
                    for item in dikt.get(f, [])
                ]
            elif ftype.__origin__ is dict:
                # Handle dict type
                _, value_type = ftype.__args__
                result[f] = {
                    key: _inner_dataclass_from_dict(value_type, value)
                    for key, value in dikt.get(f, {}).items()
                }
            else:
                result[f] = dikt.get(f)
        else:
            result[f] = _inner_dataclass_from_dict(ftype, dikt.get(f))
    return klass(**result)


def _resolve_relative(root: str, path: str) -> str:
    return os.path.join(root, path)


@dataclass
class ClassRef:
    name: str
    methods: list[str]


@dataclass
class ClassMapEntry:
    classes: list[ClassRef]
    file_path: str
    language: str


@dataclass
class ExecInfo:
    data: dict[str, Any]
    target_name: str
    target_info: dict[str, Any]

    @staticmethod
    def from_buck_result(result: BuckResult) -> "ExecInfo":
        with open(result.stdout.strip("\n")) as json_file:
            return _dataclass_from_dict(ExecInfo, json.loads(json_file.read()))

    def read_class_map(self, root: str) -> list[ClassMapEntry]:
        classmap_file = self.data["java"]["classmap_file"]
        assert not os.path.isabs(classmap_file), (
            f"{classmap_file} should be relative to {root}"
        )

        with open(_resolve_relative(root, classmap_file)) as classmap_file:
            return [
                _dataclass_from_dict(ClassMapEntry, value)
                for value in json.loads(classmap_file.read())
            ]
