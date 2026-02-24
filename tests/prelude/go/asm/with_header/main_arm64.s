//go:build arm64

#include "go_asm.h"

TEXT ·sizeOfFoo(SB),$0-24
  MOVD $goAsmCheck__size, R0
  MOVD R0, ret(FP)
  RET
