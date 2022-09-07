import ctypes

assert ctypes.CDLL("lib1.so").lib_func() == 5
