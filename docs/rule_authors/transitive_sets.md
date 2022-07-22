# Transitive Sets

Transitive sets enable propagating data up dependency trees in a manner that is
both efficient in Starlark code (low cost of creation, low memory usage), and
efficient for execution by Buck (edges can be shared instead of having each
action depend directly on all its inputs).

Some examples where transitive sets are useful are:

- Propagating transitive link-time dependencies of a library all the way to a
  binary to build.
- Propagating transitive compile-time headers.

## Rule API

First you need to declare your transitive set type, then you can use it:

```starlark
# This is the type
MySet = transitive_set()

# Those are transitive sets:
set1 = ctx.actions.tset(MySet, value = "foo")
set2 = ctx.actions.tset(MySet, value = "bar", children = [set1])
```

Values are optional, and so are children. This means you can have set with no
value, and sets with no children.

## Projections: using transitive sets in command lines

Sets aren't useful unless you can use their contents! To use a set in a command
line, you use a concept called a projection. The projection defines how to turn
individual values found in the set into command line arguments.

To define a projection, you write a function that takes a value
of your set, and returns a command-line like object (`cmd_args`, `string`,
`attr.arg()` attributes, `artifact`, etc) or a list of them in whichever way
makes sense for your use case.

Then, you call `project_as_args` to turn a set into a value suitable for
inclusion in a command line. When expanded, this projection will expand like
a list of all the node's individual projected values.

Here is an example:

```starlark
# Declare the projection
def project_as_define(value: str.type):
  return cmd_args(value, format = "-D{}")

# Add it to the set definition
MySet = transitive_set(args_projections = { "define": project_as_define })

# Create a set
set1 = ctx.actions.tset(MySet, value = "foo")
set2 = ctx.actions.tset(MySet, value = "bar", children = [set1])

# Call the projection.
# Note "define" is the key we used above in `args_projections`.
args = set2.project_as_args("define")
```

When you use `args` in a command line, it will expand to `-Dbar -Dfoo`.

Note that creating projections is very cheap. Notably, it is independent of the
size of the set.

### Traversals in depth

Transitive sets form DAGs. Notably, this means individual nodes can exist more
than 1 time in a given transitive set.

When we traverse a transitive set, we skip nodes that have already been
visited. This means their arguments will only be emitted once.

For example:

```starlark
set1 = ctx.actions.tset(MySet, value = "foo")
set2 = ctx.actions.tset(MySet, value = "bar", children = [set1])
set3 = ctx.actions.tset(MySet, value = "qux", children = [set1, set2])

args = set3.project_as_args("define")
```

This will expand to `-Dqux -Dfoo -Dbar`, even though `set1` (`"foo"`) shows up
twice in the DAG.

## Other APIs

### Transitive set reductions

You can aggregate values of a transitive set via a reduction. This can be
helpful to e.g. propagate boolean flags up the tree.

Here's a real-world example. When defining a reduction, you receive the reduced
values of all your children, and an optional value for the current node (the
value will be `None` when you create a set and you don't pass a `value`), and
you need to merge them together to produce this node's value:

```starlark
def link_info_has_default_filelist(children: ["bool"], infos: ["LinkInfos", None]):
    if infos:
        info = infos.default
        if info.filelist:
            return True
    return any(children)

# Set of LinkInfos
LinkInfosTSet = transitive_set(
    reductions = {
        "has_default_filelist": link_info_has_default_filelist,
    },
)
```

### Transitive set iteration

You *can* iterate over a transitive set. This will yield each value once. You
can also iterate over projections.

However, note that this is generally not recommended, since unlike creating and
using a projection, this operation is `O(set)`.

You should use this as an escape hatch if and only if you need to implement
something transitive sets don't support via projections or reductions, because
in doing so you'll lose a lot of the performance benefits.

For example:

```starlark
set1 = ctx.actions.tset(MySet, value = "foo")
set2 = ctx.actions.tset(MySet, value = "bar", children = [set1])
set3 = ctx.actions.tset(MySet, value = "qux", children = [set1, set2])

values = list(set3.traverse())
```

This will yield `["qux", "foo", "bar"]`.

## Implementation details

### Ordering

Transitive set iteration is a left-to-right, pre-order traversal, and ignores
nodes that have already been visited. This order is reflected in projections as
well.

Currently, this is not customizable, but we can add support for more traversals
if useful!

### Projection evaluation

Projections are evaluated eagerly for each node of your transitive set. This
means that if your projection throws an error, you'll find out when creating a
set via `ctx.actions.tset`.
