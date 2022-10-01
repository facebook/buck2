import os
import unittest


class TestCase(unittest.TestCase):
    def test_a(self):
        env = os.environ["TEST_ENV"]
        if env == "fatal":
            os._exit(1)
        if env == "fail" or os.environ.get("test_make_it_fail"):
            self.assertequal(41, 42)
        print("tested!")

    def test_b(self):
        env = os.environ["TEST_ENV"]
        if env == "fatal":
            os._exit(1)
        if env == "fail" or os.environ.get("test_make_it_fail"):
            self.assertequal(41, 42)
        print("tested!")

    def test_c(self):
        env = os.environ["TEST_ENV"]
        if env == "fatal":
            os._exit(1)
        if env == "fail" or os.environ.get("test_make_it_fail"):
            self.assertequal(41, 42)
        print("tested!")
