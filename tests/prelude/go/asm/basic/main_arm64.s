//go:build arm64

TEXT ·add(SB),$0-24
  MOVD x+0(FP), R0
  MOVD y+8(FP), R1
  ADD R1, R0, R0
  MOVD R0, ret+16(FP)
  RET
