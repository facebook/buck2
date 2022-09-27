import os
import unittest


class TestCase(unittest.TestCase):
    def test(self):
        env = os.environ["TEST_ENV"]
        if env == "fatal":
            os._exit(1)
        if env == "fail" or os.environ.get("TEST_MAKE_IT_FAIL"):
            self.assertEqual(41, 42)
        print("TESTED!")
