#!/bin/bash

set -e -o pipefail

# The magic number "141" is the addition of the constants declared in module a and module b (42 and 99, respectively.)
#
# The goal of this check is to ensure that llvm successfully inlined functions from module a and module b into
# main, which it would not be able to do unless performing cross-module optimizations typical of LTO.
#
# A non-LTO opt build will produce a `main` function whose assembly looks like this:
#
# 000000000021d0e0 <main>:
#  21d0e0: 55                            pushq %rbp
#  21d0e1: 48 89 e5                      movq  %rsp, %rbp
#  21d0e4: 53                            pushq %rbx
#  21d0e5: 50                            pushq %rax
#  21d0e6: e8 15 00 00 00                callq 0x21d100 <_ZN1a21returnsAKnownConstantEv>
#  21d0eb: 89 c3                         movl  %eax, %ebx
#  21d0ed: e8 1e 00 00 00                callq 0x21d110 <_ZN1b21returnsAKnownConstantEv>
#  21d0f2: 01 d8                         addl  %ebx, %eax
#  21d0f4: 48 83 c4 08                   addq  $8, %rsp
#  21d0f8: 5b                            popq  %rbx
#  21d0f9: 5d                            popq  %rbp
#  21d0fa: c3                            retq
#
# Neither of the cross-module calls to `a` or `b` could be inlined, so there are explicit calls to them.
#
# A successful LTO build would look like this:
#
# 00000000002d0970 <main>:
#  2d0970: 55                            pushq %rbp
#  2d0971: 48 89 e5                      movq  %rsp, %rbp
#  2d0974: b8 8d 00 00 00                movl  $141, %eax
#  2d0979: 5d                            popq  %rbp
#  2d097a: c3                            retq
#
# Everything gets inlined and constant-folded into the magic number `141`.
if "$1" "$2" --disassemble-symbols=main | grep -q "\$141"; then
    echo "modules a and b successfully inlined"
    exit 0
else
    echo "modules a and b were not successfully inlined"
    exit 1
fi
