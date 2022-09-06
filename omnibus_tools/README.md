This is a set of tools for interacting with Omnibus sharing. The goal of
Omnibus sharing is to get most Omnibus builds to agree on what nodes are roots
and what nodes are exclusions, so there are tools to help you annotate roots
and exclusions, and a tool to check your work.

All of those should be run with `buck2 bxl -c python.emit_omnibus_metadata=True @fbcode//mode/opt`.

# `find_implicit_roots`

This helps identify roots that should be annotated as explicit roots. You give
it a set of targets, it finds all the Python binaries, and it finds all the
roots across all their dependency graphs.

You can then codemod them to add explicit root annotations.


# `find_exclusions`

This is used to produce a set of default exclusions. Those are libraries that
will tend to end up excluded in most builds. We try to keep this small.


# `analyze_sharing`

You give this a target, and it tells you how many roots were shared roots, and
for roots that couldn't be shared, it tells you why not.
