# Copyright 2018 The Starlark in Rust Authors.
# Copyright (c) Facebook, Inc. and its affiliates.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Python: python3 benchmark.py
# Rust: starlark benchmark.py

REPEAT_100M = 100000000


def benchmark_loop():
    for _x in range(REPEAT_100M):
        pass


def benchmark_multiply():
    y = 3
    for _x in range(REPEAT_100M):
        y = y * 1
    return y


def benchmark_call_native_len():
    y = 0
    xs = []
    for _x in range(REPEAT_100M):
        y = len(xs)
    return y


def benchmark_call_native_1pos():
    y = 0
    xs = 3
    for _x in range(REPEAT_100M):
        y = abs(xs)
    return y


def op4(_x):
    pass


def benchmark_call_def_1pos():
    y = 0
    for x in range(REPEAT_100M):
        op4(x)
    return y


def op5(_a, _b, _c, _d, _e, _f, _g, _h):
    pass


def benchmark_call_def_8pos():
    y = 0
    for x in range(REPEAT_100M):
        op5(x, x, x, x, x, x, x, x)
    return y


def op6(a):
    pass


def benchmark_call_def_1name():
    y = 0
    for x in range(REPEAT_100M):
        op6(a=x)
    return y


def op7(a, b, c, d, e, f, g, h):
    pass


def benchmark_call_def_8name():
    y = 0
    for x in range(REPEAT_100M):
        op7(a=x, b=x, c=x, d=x, e=x, f=x, g=x, h=x)
    return y


print(benchmark_call_def_1name())
