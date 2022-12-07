from conans import ConanFile
from conans.model import Generator


class BucklerGenerator(Generator):
    @property
    def filename(self):
        return "conan-imports.bzl"

    @property
    def content(self):
        def public_dir(o):
            return list(filter(lambda s: not s.startswith("_"), dir(o)))
        return """\
# @generated
# Update with TODO

# TODO

# DEBUG
{DEBUG}
""".format(
            DEBUG = "\n".join("# " + l for l in [
                "self {}".format(public_dir(self)),
                "deps_build_info {}".format(public_dir(self.deps_build_info)),
                "deps_cpp_info {}".format(public_dir(self.conanfile.deps_cpp_info)),
                "deps_env_info {}".format(public_dir(self.conanfile.deps_env_info)),
                "deps_user_info {}".format(public_dir(self.conanfile.deps_user_info)),
                "env {}".format(str(self.conanfile.env)),
            ]))


class Buckler(ConanFile):
    name = "buckler"
    version = "0.1"
    description = """\
Buckler - Conan extension for Buck2

This package provides a
- [Generator][generator] to import Conan built packages into Buck2.
- [Toolchain][toolchain] to expose the Buck2 configuration to Conan.

[generator]: https://docs.conan.io/en/latest/reference/generators.html#generators-reference
[toolchain]: https://docs.conan.io/en/latest/creating_packages/toolchains.html
"""
    url = "https://github.com/facebookincubator/buck2"
    license = "Apache-2.0"
