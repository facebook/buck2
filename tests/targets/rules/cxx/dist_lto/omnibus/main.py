#!/usr/bin/env fbpython
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

from ctypes import cdll

get_one = cdll.LoadLibrary(
    "libbuck2_tests_targets_rules_cxx_dist_lto_omnibus_top_level_lib.so"
).get_one


def main() -> int:
    print(f"the result from get_one() from ombibus: {get_one()}")
    return 0


if __name__ == "__main__":
    main()
