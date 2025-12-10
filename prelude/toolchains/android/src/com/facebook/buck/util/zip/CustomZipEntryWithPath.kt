// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.buck.util.zip

import java.nio.file.Path
import java.util.Optional

data class CustomZipEntryWithPath(val entry: CustomZipEntry, val path: Optional<Path>)
