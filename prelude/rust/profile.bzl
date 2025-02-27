# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":context.bzl", "CompileContext")  # @unused: Used as type

def analyze_llvm_lines(
        ctx: AnalysisContext,
        compile_ctx: CompileContext,
        llvm_ir_noopt: Artifact) -> Artifact | None:
    if compile_ctx.toolchain_info.llvm_lines_tool == None:
        return None
    llvm_lines = ctx.actions.declare_output("llvm_lines.txt")
    ctx.actions.run(
        cmd_args(
            compile_ctx.toolchain_info.llvm_lines_output_redirect,
            compile_ctx.toolchain_info.llvm_lines_tool,
            llvm_lines.as_output(),
            "llvm-lines",
            "--files",
            llvm_ir_noopt,
        ),
        category = "analyze_llvm_lines",
    )
    return llvm_lines
