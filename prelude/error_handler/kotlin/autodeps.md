---
llms-gk: 'autodeps_md'
oncalls: ['android_devxx']
apply_to_regex: '^(?!.*/(?:wearables?|smartglasses)/).*\.(java|kt)$'
apply_to_clients: ['vscode']
tools: ['AutodepsTool']
---

## MANDATORY: Run AutodepsTool After Import Changes

**CRITICAL**: When you add or modify import statements in `.java` or `.kt` files, you **MUST run `AutodepsTool` BEFORE `validate_changes`**.

## Tool Parameters

Call `AutodepsTool` with:

- `file_path`: Absolute path to the modified Java/Kotlin file
- `remove_unused_deps`: **Always set to `false`**

## When to Use AutodepsTool

**ALWAYS run immediately after:**

1. Adding any `import` statement to a Java/Kotlin file
2. Modifying existing imports
3. When you see errors like "unresolved reference"

## Correct Workflow

### ✅ CORRECT Order

```
1. Add/modify/remove import statement in File.java
2. Run AutodepsTool on the file
3. Wait for completion
4. Run validate_changes
```

### ❌ WRONG Order

```
1. Add import statement
2. Run validate_changes
```

## Example

After adding this import:

```java
import com.facebook.litho.Component;
```

**Immediately call:**

```
AutodepsTool(
  file_path="/data/sandcastle/boxes/fbsource/fbandroid/java/com/instagram/feed/FeedRenderer.java",
  remove_unused_deps=false
)
```

**Then** run `validate_changes`.

## Prohibitions

- ❌ **Never skip `AutodepsTool`** after import changes
- ❌ **Never run `validate_changes` before `AutodepsTool`**
<gk project="devmate_resource_import_md">
- ⚠️ **Never manually edit BUCK files for non-resource dependencies** - use `AutodepsTool` instead. Manual BUCK edits are ONLY allowed for adding resource (`R`) dependencies (see the `android_r_resource_lookup` skill for details).
</gk>

<gk project="devmate_resource_import_md" exclude="true">
- ❌ **Never manually edit BUCK files** - use `AutodepsTool` instead
</gk>
- ❌ **Never set `remove_unused_deps=true`** - only add dependencies, never remove
