import unittest

from buck2.tests.targets.rules.python.test.lib import lib_method


class TestCase(unittest.TestCase):
    def test(self):
        self.assertEqual(lib_method(), "lib!")
