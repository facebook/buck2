# Visibility

Visibility determines whether a build rule can include a build target in its list of `deps`. In a large project, you may want to prevent developers from "reaching across" the project and pulling in additional code. Reducing the visibility of build rules can help prevent that type of behavior.
There are two types of visibility attributes available, each of which takes a list of [build target patterns](https://buck.build/concept/build_target_pattern.html): `visibility`, which determines what other targets can depend on a target, and `within_view`, which determines what other targets a target can depend on.
Both attributes act as allowlists, with some exceptions. In general, if a target is not listed, there may be no dependency relationship. If the `within_view` list is empty or unset, however, its check is bypassed. Similarly, targets defined in the same build file always act as if they were members of their siblings' `visibility` lists.
There is also a special value, `'PUBLIC'`, which makes a build rule visible to all other rules. `'PUBLIC'` is valid in `visibility` but not `within_view`.
In case of logically-conflicting lists, `within_view` takes precedence over `visibility`. If `//foo:bar` defines `//hello:world` in its `visibility` list, but `//hello:world` does not define `//foo:bar` in its `within_view` list, then `//hello:world` may not depend on `//foo:bar`.

## Examples

A common library like Guava should be able to be included by any build rule:

```
prebuilt_jar(
  name = 'guava',
  binary_jar = 'guava-14.0.1.jar',
  visibility = ['PUBLIC',],)
```

It is common to restrict the visibility of Android resources to the Java code that uses it:

```
android_resource(
  name = 'ui_res',
  res = 'res',
  package = 'com.example',
  visibility = ['//java/com/example/ui:ui',],)
```

Or it may be simpler to make it visible to the entire directory in case additional build rules are added to `java/com/example/ui/BUCK`:

```
android_resource(
  name = 'ui_res',
  res = 'res',
  package = 'com.example',
  visibility = ['//java/com/example/ui:',],)
```

Also, it is common to limit code for testing to be visible only to tests. If you define all of your Java unit tests in a folder named `javatests/` in the root of your project, then you could define the following rule to ensure that only allow build rules under `javatests/` can depend on JUnit:

```
prebuilt_jar(
  name = 'junit',
  binary_jar = 'junit-4.11.jar',
  visibility = ['//javatests/...',],)
```

Finally, restricting the view of a target can be useful for preventing dependency creep:

```
java_library(
  name = 'example',
  visibility = ['PUBLIC',],
  within_view = ['//foo:bar','//hello:world',],)
```
