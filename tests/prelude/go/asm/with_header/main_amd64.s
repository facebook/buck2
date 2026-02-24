//go:build amd64

#include "go_asm.h"

TEXT ·sizeOfFoo(SB),$0-24
  MOVQ $goAsmCheck__size, ret(FP)
  RET
