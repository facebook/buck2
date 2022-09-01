import os
import unittest


class TestCase(unittest.TestCase):
    def test(self):
        self.assertTrue(os.path.isabs(os.environ["ASSET"]))
