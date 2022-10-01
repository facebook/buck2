import ctypes

assert ctypes.CDLL("lib1.so").lib1_func() == 1
