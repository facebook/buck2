#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import inspect

import pytest
from buck2.tests.e2e_util.buck_workspace import buck  # noqa F401


def pytest_collection_modifyitems(items):
    """
    Used to automatically mark async test functions with pytest.mark.asyncio decorator.
    """
    for item in items:
        if isinstance(item, pytest.Function) and inspect.iscoroutinefunction(
            item.function
        ):
            item.add_marker(pytest.mark.asyncio)


def pytest_configure(config):
    config.addinivalue_line(
        "markers", "buck_test: used by buck_test to pass data to Buck fixtures"
    )
