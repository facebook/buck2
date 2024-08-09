---
id: visibility
title: Visibility
---

Visibility determines whether a [target](./glossary.md#target) can reference
another target in its [attributes](./glossary.md#attribute). In a large project,
you may want to prevent developers from 'reaching across' the project and
pulling in additional code. Reducing the visibility of targets can help prevent
that type of behavior.

There are two types of visibility attributes available (each of which takes a
list of [target patterns](./glossary.md#target-pattern)):

- `visibility` - determines which other targets can depend on a target.
- `within_view` - determines which other targets a target can depend on.

Both attributes act as allowlists, with some exceptions. In general, if a target
is not listed, there may be no dependency relationship. If the `within_view`
list is empty or unset, however, its check is bypassed. Similarly, targets
defined in the same [BUCK file](./glossary.md#buck-file) always act as if they
were members of their siblings' `visibility` lists.

There is also a special value for `visibility` attribute: `'PUBLIC'`, which
makes a build rule visible to all targets.

In case of logically-conflicting lists, `within_view` takes precedence over
`visibility`. If `//foo:bar` defines `//hello:world` in its `visibility` list,
but `//hello:world` does not define `//foo:bar` in its `within_view` list, then
`//hello:world` may not depend on `//foo:bar`.

## Examples

A common library like Guava should be able to be included by any build rule:

```java
prebuilt_jar(
  name = 'guava',
  binary_jar = 'guava-14.0.1.jar',
  visibility = ['PUBLIC']
)
```

It is common to restrict the visibility of Android resources to the Java code
that uses it:

```java
android_resource(
  name = 'ui_res',
  res = 'res',
  package = 'com.example',
  visibility = ['//java/com/example/ui:ui']
)
```

Or it may be simpler to make it visible to the entire directory in case
additional build rules are added to `java/com/example/ui/BUCK`:

```java
android_resource(
  name = 'ui_res',
  res = 'res',
  package = 'com.example',
  visibility = ['//java/com/example/ui:']
)
```

Also, it is common to limit code for testing to be visible only to tests. If you
define all of your Java unit tests in a folder named `javatests/` in the root of
your project, then you could define the following rule to ensure that only build
rules under `javatests/` can depend on JUnit:

```java
prebuilt_jar(
  name = 'junit',
  binary_jar = 'junit-4.11.jar',
  visibility = ['//javatests/...']
)
```

Finally, restricting the view of a target can be useful for preventing
dependency creep:

```java
java_library(
  name = 'example',
  visibility = ['PUBLIC',],
  within_view = ['//foo:bar','//hello:world']
)
```
