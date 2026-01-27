# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

MemoryInfo = provider(fields = ["data"])

def _memory_retaining_rule_impl(ctx):
    """
    A rule that allocates and retains memory in analysis.

    This rule creates a large dictionary with unique keys and values in analysis
    phase that will be retained in the frozen heap, allowing us to test analysis
    memory tracking.
    """

    memory_size = ctx.attrs.memory_size

    # Create list to retain memory (each int is ~8 bytes)
    data = list(range(memory_size // 8))

    # Write output
    out = ctx.actions.write("out.txt", "")

    # Store in a provider to retain the memory
    # Using a custom provider ensures the list stays in the frozen heap
    return [
        DefaultInfo(default_output = out),
        MemoryInfo(data = data),
    ]

memory_retaining_rule = rule(
    impl = _memory_retaining_rule_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
        "memory_size": attrs.int(),
    },
)
