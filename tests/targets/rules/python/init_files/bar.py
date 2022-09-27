import os

from setuptools import find_packages

assert "nested" in find_packages(where=os.path.dirname(os.path.dirname(__file__)))
