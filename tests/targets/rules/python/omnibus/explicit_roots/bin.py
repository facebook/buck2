import ctypes

assert ctypes.CDLL("lib1.so").lib1_func() == 1
assert ctypes.CDLL("lib2.so").lib2_func() == 2
