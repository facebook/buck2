from conans import ConanFile
from conans.model import Generator


class BucklerGenerator(Generator):
    @property
    def filename(self):
        return "conan-imports.bzl"

    @property
    def content(self):
        return """\
# @generated
# Update with TODO

# TODO
"""


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
