# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

AlphaInfo = provider(fields = [])
ZetaInfo = provider(fields = [])

def _impl(_ctx):
    return [DefaultInfo(), ZetaInfo(), AlphaInfo()]

# This bzl file cannot be interpreted with Buck1 because there's no `rule` builtin.
provider_test_rule = rule(impl = _impl, attrs = {})
