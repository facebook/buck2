def rust_library(os_deps = None, test_deps = None, test_env = None, named_deps = None, deps = [], visibility = ["PUBLIC"], **kwargs):
    _unused = (os_deps, test_deps, test_env, named_deps)  # @unused
    native.rust_library(deps = filter(None, map(_fix_dep, deps)), visibility = visibility, **kwargs)

def rust_binary(unittests = None, deps = [], **kwargs):
    _unused = unittests  # @unused
    native.rust_library(deps = filter(None, map(_fix_dep, deps)), **kwargs)

def _fix_dep(x):
    if x == "//buck2/gazebo/gazebo:gazebo":
        return "fbsource//third-party/rust:gazebo"
    elif x == "//common/rust/folly/logging:logging":
        return None
    elif x == "//watchman/rust/watchman_client:watchman_client":
        return "fbsource//third-party/rust:watchman_client"
    elif x.startswith("//common/rust/shed/"):
        return "fbsource//third-party/rust:" + x.removeprefix("//common/rust/shed/").split(":")[0]
    elif x.startswith("//common/rust/") or x.startswith("//buck2/facebook/") or x.startswith("//eden/") or x.startswith("//remote_execution/"):
        return None
    elif x.startswith("//buck2/"):
        return "root//" + x.removeprefix("//buck2/")
    else:
        return x
