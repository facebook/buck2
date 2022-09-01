import subprocess
import unittest

from libfb.py import parutil


class DoTest(unittest.TestCase):
    def test_resource(self):
        # This is not consistent with binaries, which is sort of confusing.
        path = parutil.get_file_path(
            "buck2/tests/targets/rules/python/binary_in_resources/outer.par"
        )
        subprocess.check_call([path])
