# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @nolint

# Test that goto definition works for global symbols
x = <func2>my_f<func2_click>u</func2_click>nc2</func2>
y = <info>Defau<info_click>l</info_click>tInfo</info>
z = invalid_sy<invalid_click>m</invalid_click>bol
