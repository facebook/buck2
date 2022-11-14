def buck_genrule(visibility = ["PUBLIC"], **kwargs):
    native.genrule(visibility = visibility, **kwargs)

def buck_filegroup(visibility = ["PUBLIC"], **kwargs):
    native.filegroup(visibility = visibility, **kwargs)
