---
oncalls: ['android_devxx']
description: 'Use this skill when working with Android R resources (R.drawable, R.string, R.color, etc.) in fbandroid or whatsapp/android code, or when debugging unresolved R reference build failures'
apply_to_regex: '.*(fbandroid|whatsapp/android)/.*\.(kt|java)$'
apply_to_user_prompt: 'R\.drawable|R\.string|R\.color|R\.attr|R\.style|R\.layout|unresolved.*R\.|android resource'
tools: ['FindResourcePackage', 'AutodepsTool']
---

# Android R Resource Lookup

**Before writing code with R resources, always look up the correct package first.**

## The Problem

AI agents often assume `com.facebook.R`, but many resources use different packages:

```kotlin
// WRONG - assumed com.facebook.R
import com.facebook.R
use(R.string.clips_live_element_bottomsheet_account_grid_item_label)

// CORRECT - looked up with FindResourcePackage
import com.instagram.basel.snippets.R
use(R.string.clips_live_element_bottomsheet_account_grid_item_label)
```

## Usage

```python
FindResourcePackage(
    resource="R.drawable.icon_name",  # Required: R.<type>.<name>
    app="ig4a"                         # Optional: whatsapp, fb4a, fbandroid, ig4a, m4a, origami
)
```

Returns the correct **package** for imports and **Buck target** for dependencies.

## Workflow

1. **Look up**: `FindResourcePackage(resource="R.drawable.my_icon")`
   - If multiple packages are returned, try passing the `app` parameter to narrow down (e.g., `app="ig4a"`)
   - If still ambiguous, investigate which package is correct by checking existing code references in your module
2. **Import**: Use the returned package (e.g., `import com.instagram.icons.R`)
3. **Run AutodepsTool**: It will automatically add the correct BUCK dependency

### Fixing Unresolved R Reference Build Failures

This workflow also fixes common R reference build errors:
- `Unresolved reference: R`
- `Cannot resolve symbol 'R'`
- `error: package R does not exist`

Simply identify the R resource causing the failure and follow the steps above.

## Adding NEW Resources

When creating new resources (e.g., adding drawables to XML), `FindResourcePackage` won't help since the resource isn't indexed yet.

**To determine the package for new resources:**

1. **Check existing resources in the same target**: Look at how other resources in that `android_res` target are referenced in code
2. **Check the BUCK file**: Look for `custom_package` attribute in the `android_resource` rule
3. **Check the package from path**: Often follows the pattern `fbandroid/android_res/com/facebook/feature` → `com.facebook.R`

**Example**: Adding a new drawable to `fbandroid/android_res/com/instagram/icons/`
- Check BUCK file or existing code references for this target
- This target uses `com.facebook.R` (not `com.instagram.icons.R`)
- Reference as: `com.facebook.R.drawable.my_new_icon`

### Adding the BUCK dependency for new resources

After determining the correct R package, you must manually add the resource target to your module's BUCK file (AutodepsTool won't have new resources in its index):

1. **Find the resource dependency**:
   - Take the R import package (e.g., `com.facebook.samples.litho` from `import com.facebook.samples.litho.R`)
   - Search for a BUCK file that defines `fb_android_resource` or `android_resource` with that package value
   - Example search: `package = "com.facebook.samples.litho"` in BUCK files
   - Note the target name (e.g., `name = "res"`) and its path (e.g., `sample/BUCK`)
   - Check if there's a constant for this target in `.bzl` files (search for the target path)
2. **Update your module's BUCK file**:
   - Add the dependency constant to the `load()` statement if using a constant
   - Add the dependency to the `deps` list

**Example BUCK update**:
```python
# Using a constant (preferred if available)
load("//path/to/defs.bzl", "LITHO_SAMPLE_RES")

fb_android_library(
    deps = [
        LITHO_SAMPLE_RES,
    ],
)

# Using raw target
fb_android_library(
    deps = [
        "//fbandroid/android_res/com/instagram/icons:res",
    ],
)
```
