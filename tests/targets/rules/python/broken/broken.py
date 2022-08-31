import os
import unittest

# Intentional exit to test broken test listing
os._exit(1)


class TestCase(unittest.TestCase):
    def test(self):
        pass
