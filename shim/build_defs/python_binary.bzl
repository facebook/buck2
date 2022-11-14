def python_binary(srcs = [], **kwargs):
    _unused = srcs  # @unused
    native.python_binary(**kwargs)
