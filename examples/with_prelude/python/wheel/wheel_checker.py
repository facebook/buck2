# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import importlib.resources
import zipfile


def main() -> None:
    with importlib.resources.path(__package__, "printlib.whl") as wheel:
        with zipfile.ZipFile(wheel) as zf:
            if "printlib/print.py" in zf.namelist():
                print("Wheel OK")
            else:
                print(f"print.py not found in {wheel=}. {zf.namelist()=}")


if __name__ == "__main__":
    main()
