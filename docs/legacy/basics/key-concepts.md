# Key concepts

Buck has a number of fundamental concepts:

- A [**_build rule_**](https://buck.build/concept/build_rule.html) describes how
  to produce an output file from a set of input files. Most build rules are
  specific to a particular language or platform. For example, you would use the
  [`cxx_binary`](https://buck.build/rule/cxx_binary.html) rule to create a C++
  binary, but you would use the
  [`android_binary`](https://buck.build/rule/android_binary.html) rule to create
  an Android APK.
- A [**_build target_**](https://buck.build/concept/build_target.html) is a
  string that uniquely identifies a build rule. It can be thought of as a URI
  for the build rule within the Buck project.
- A [**_build file_**](https://buck.build/concept/build_file.html) defines one
  or more build rules. In Buck, build files are typically named `BUCK`. A `BUCK`
  file is analogous to the `Makefile` used by the Make utility. In your project,
  you will usually have a separate `BUCK` file for each buildable unit of
  software—such as a binary or library. For large projects, you could have
  hundreds of `BUCK` files.

A Buck **_package_** comprises: a Buck build file (a `BUCK` file), all
files—such as source files and headers—in the same directory as the `BUCK` file
or in subdirectories, provided those subdirectories do not themselves contain a
`BUCK` file. To say it another way, a `BUCK` file defines the root of a package,
but Buck packages might not include all their subdirectories because Buck
packages do not overlap or contain other Buck packages. For example, in the
following diagram, the BUCK file in directory `app-dir-1` defines that directory
as the root of a package—which is labeled **Package A** in the diagram. The
directory `app-dir-2` is part of Package A because it is a subdirectory of
`app-dir-1`, but does not itself contain a BUCK file. Now, consider directory
`app-dir-3`. Because `app-dir-3` contains a BUCK file it is the root of a new
package (**Package B**). Although `app-dir-3` is a subdirectory of `app-dir-1`,
it is _not_ part of Package A. Buck has the concept of a **_cell_**, which
defines a directory tree of one or more Buck packages. A Buck build could
involve multiple cells. Cells often correspond to repositories, but this isn't
required. The root of a Buck cell contains a global configuration file called
[**`.buckconfig`**](https://buck.build/files-and-dirs/buckconfig.html). Note
that although the cell root should contain a `.buckconfig`, the presence of a
`.buckconfig` file doesn't in itself define a cell. Rather, _the cells involved
in a build are defined at the time Buck is invoked_; they are specified in the
`.buckconfig` for the Buck _project_ (see below). A Buck **_project_** is
defined by the `.buckconfig` where Buck is invoked, or if that directory doesn't
contain a `.buckconfig`, the project is defined by the `.buckconfig` in the
nearest ancestor directory. The `.buckconfig` for the project specifies the
cells that constitute the Buck project. Specifically, these cells are specified
in the
[`[repositories]`](https://buck.build/files-and-dirs/buckconfig.html#repositories)
section of the `.buckconfig`. Note that the directory tree rooted at this
`.buckconfig` is automatically considered a cell by Buck; in other words, the
project's `.buckconfig` doesn't need to specify the project cell
explicitly—although it is a good practice to do so.

### Buck's dependency graph

Every build rule can have zero or more dependencies. You can specify these
dependencies using, for example, the `deps` argument to the build rule. For more
information about specifying dependencies, consult the reference page for the
build rule you are using. These dependencies form a directed graph, called the
_target graph_. Buck requires the graph to be acyclic. When building the output
of a build rule, all of the rule's transitive dependencies are built first. This
means that the graph is built in a "bottom-up" fashion. A build rule knows only
which rules it depends on, not which rules depend on it. This makes the graph
easier to reason about and enables Buck to identify independent subgraphs that
can be built in parallel. It also enables Buck to determine the minimal set of
build targets that need to be rebuilt. For more information about how Buck
leverages the graph of build dependencies, see
[What Makes Buck so Fast](https://buck.build/concept/what_makes_buck_so_fast.html).

### Multiple Buck projects in a single repository

Buck is designed to build multiple deliverables from a single repository—that
is, a _monorepo_—rather than from multiple repositories. Support for the
monorepo design motivated Buck's support for cells and projects. It is
Facebook's experience that maintaining all dependencies in the same repository
makes it easier to ensure that all developers have the correct version of the
code and simplifies the process of making atomic commits.
