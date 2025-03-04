# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import sys
from string import Template

template_file = sys.argv[1]
is_oss = sys.argv[2].lower() == "true"

with open(template_file) as f:
    template_str = f.read()

template = Template(template_str)
print(template.substitute({"is_oss": str(is_oss).lower()}))
