# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Handle labels used to make genrules prefer local execution
"""

# Some rules prefer to be run locally for various reasons listed next to the label.
_GENRULE_PREFER_LOCAL_LABELS = {label: True for label in [
    # Used for rules that just copy large files and will be faster to do locally
    "large_copy",
]}

def genrule_labels_prefer_local(labels):
    for label in labels:
        if label in _GENRULE_PREFER_LOCAL_LABELS:
            return True
    return False
