def fetch_python_libraries(pkgs):
    for name, pkg in pkgs.items():
        native.remote_file(
            name = "{}-download".format(name),
            url = pkg["url"],
            sha256 = pkg["sha256"],
            out = "{}.whl".format(name),
        )
        native.prebuilt_python_library(
            name = name,
            binary_src = ":{}-download".format(name),
            deps = [":{}".format(dep) for dep in pkg.get("deps", [])],
            visibility = ["PUBLIC"],
        )
