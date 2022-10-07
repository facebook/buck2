def rust_protobuf_library(name, **kwargs):
    _unused = kwargs  # @unused
    native.export_file(name = name, src = "MISSING_FILE", visibility = ["PUBLIC"])
