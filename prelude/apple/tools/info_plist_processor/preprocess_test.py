# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import io
import re
import unittest

from .preprocess import _process_line, _re_string, preprocess


class TestPreprocess(unittest.TestCase):
    def test_default_substitution_values(self):
        input_file = io.StringIO(
            r"""
<string>${PRODUCT_NAME}</string>
<string>${EXECUTABLE_NAME}</string>
"""
        )
        expected = r"""
<string>my_product_name</string>
<string>my_product_name</string>
"""
        substitutions_file = io.StringIO(r"{}")
        output_file = io.StringIO("")
        preprocess(input_file, output_file, substitutions_file, "my_product_name")
        self.assertEqual(output_file.getvalue(), expected)

    def test_json_substitution_values_precede_default_ones(self):
        input_file = io.StringIO(
            r"""
<string>${PRODUCT_NAME}</string>
<string>${EXECUTABLE_NAME}</string>
"""
        )
        expected = r"""
<string>foo</string>
<string>bar</string>
"""
        substitutions_file = io.StringIO(
            r"""
{
    "PRODUCT_NAME": "foo",
    "EXECUTABLE_NAME": "bar"
}
"""
        )
        output_file = io.StringIO("")
        preprocess(input_file, output_file, substitutions_file, "my_product_name")
        self.assertEqual(output_file.getvalue(), expected)

    def test_chained_substitutions(self):
        input_file = io.StringIO(r"<string>${foo}</string>")
        expected = r"<string>baz</string>"
        substitutions_file = io.StringIO(
            r"""
{
    "foo": "${bar}",
    "bar": "baz"
}
"""
        )
        output_file = io.StringIO("")
        preprocess(input_file, output_file, substitutions_file, "my_product_name")
        self.assertEqual(output_file.getvalue(), expected)

    def test_recursive_substitutions_throws(self):
        input_file = io.StringIO(r"<string>${foo}</string>")
        substitutions_file = io.StringIO(
            r"""
{
    "foo": "${bar}",
    "bar": "${foo}"
}
"""
        )
        output_file = io.StringIO("")
        with self.assertRaises(Exception) as context:
            preprocess(input_file, output_file, substitutions_file, "my_product_name")
        self.assertTrue("Recursive" in str(context.exception))

    def test_variable_with_modifier(self):
        input_file = io.StringIO(r"<string>${foo:modifier}</string>")
        expected = r"<string>bar</string>"
        substitutions_file = io.StringIO(r'{"foo":"bar"}')
        output_file = io.StringIO("")
        preprocess(input_file, output_file, substitutions_file, "my_product_name")
        self.assertEqual(output_file.getvalue(), expected)

    def test_self_reference_after_text_never_terminates(self):
        # Every substitution moves the search position, which clears the record of the
        # variables already expanded, so the self-reference is expanded again and again.
        pattern = _SearchCounter(re.compile(_re_string), limit=1000)
        with self.assertRaises(_SearchLimitReached):
            _process_line(
                "<string>com.foo.$(SUFFIX)</string>",
                pattern,
                {"SUFFIX": "app.$(SUFFIX)"},
                None,
            )

    def test_acyclic_chain_reported_as_recursive(self):
        input_file = io.StringIO(r"<string>$(BUNDLE_ID) $(BUNDLE_PREFIX)</string>")
        substitutions_file = io.StringIO(
            r"""
{
    "BUNDLE_ID": "$(BUNDLE_PREFIX).app",
    "BUNDLE_PREFIX": "com.foo"
}
"""
        )
        output_file = io.StringIO("")
        with self.assertRaises(RuntimeError) as context:
            preprocess(input_file, output_file, substitutions_file, "my_product_name")
        self.assertIn("Recursive", str(context.exception))


class _SearchLimitReached(Exception):
    pass


class _SearchCounter:
    """A pattern stand-in that gives up after `limit` searches, so a substitution loop that
    never ends fails the test instead of hanging it."""

    def __init__(self, pattern, limit):
        self._pattern = pattern
        self._limit = limit

    def search(self, string, pos=0):
        self._limit -= 1
        if self._limit < 0:
            raise _SearchLimitReached()
        return self._pattern.search(string, pos)
