## MANDATORY: Run autodeps After Import Changes

**CRITICAL**: When you add or modify import statements in `.java` or `.kt` files, you **MUST run autodeps to update the BUCK dependencies BEFORE you validate or build**.

## Command

Run:

```
xplat/tools/autodeps2 --languages android --ignore-lint-config <ABSOLUTE_OR_REPO_RELATIVE_PATH_TO_FILE>
```

(Equivalently, `meta android.build autodeps --file-path <file>`.)

- Pass the path to the modified Java/Kotlin file.
- **Never** pass `--remove-deps` — only add dependencies, never remove them.

## When to Use

**ALWAYS run immediately after:**

1. Adding any `import` statement to a Java/Kotlin file
2. Modifying existing imports
3. When you see errors like "unresolved reference"

## Correct Workflow

### ✅ CORRECT Order

```
1. Add/modify import statement in File.kt
2. Run: xplat/tools/autodeps2 --languages android --ignore-lint-config <path to File.kt>
3. Wait for completion
4. Validate or build
```

### ❌ WRONG Order

```
1. Add import statement
2. Validate or build   (deps not yet updated → "unresolved reference")
```

## Example

After adding this import:

```kotlin
import com.facebook.litho.Component
```

**Immediately run:**

```
xplat/tools/autodeps2 --languages android --ignore-lint-config \
  fbandroid/java/com/instagram/feed/FeedRenderer.kt
```

**Then** validate or build.

## Prohibitions

- ❌ **Never skip autodeps** after import changes
- ❌ **Never validate or build before running autodeps**
- ⚠️ **Never manually edit BUCK files for non-resource dependencies** — run autodeps instead. Manual BUCK edits are ONLY allowed for adding resource (`R`) dependencies (see the `android_r_resource_lookup` skill for details).
- ❌ **Never pass `--remove-deps`** — only add dependencies, never remove
