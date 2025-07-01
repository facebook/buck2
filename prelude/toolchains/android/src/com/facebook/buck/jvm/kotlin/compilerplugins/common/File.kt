/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.common

import java.io.File
import org.jetbrains.kotlin.com.intellij.openapi.fileTypes.FileType
import org.jetbrains.kotlin.com.intellij.openapi.vfs.VirtualFile
import org.jetbrains.kotlin.com.intellij.openapi.vfs.local.CoreLocalFileSystem
import org.jetbrains.kotlin.com.intellij.openapi.vfs.local.CoreLocalVirtualFile
import org.jetbrains.kotlin.com.intellij.psi.FileViewProvider
import org.jetbrains.kotlin.com.intellij.psi.PsiManager
import org.jetbrains.kotlin.com.intellij.psi.SingleRootFileViewProvider
import org.jetbrains.kotlin.com.intellij.testFramework.LightVirtualFile
import org.jetbrains.kotlin.idea.KotlinFileType
import org.jetbrains.kotlin.psi.KtFile

/**
 * Create a new kt file, along with a virtual file.
 *
 * NOTE: A KtFile without a virtual file will result in an NPE when the next plugin (e.g. kapt)
 * tries to read files.
 *
 * Inspired by [the arrow-meta compiler plugin](https://fburl.com/qbeodjkr).
 *
 * @param fileManager The PsiManager for file. A common way to acquire is from existing [KtFile] by
 *   [KtFile.getManager].
 * @param outputDir The directory to put the new file. For compiler plugin, a common way is to get
 *   it from buck by a plugin option with value `__codegen_dir__`. See
 *   [example](https://fburl.com/diffusion/1aczk9lk).
 * @param name Name for new file
 * @param content Content for new file
 * @param writable Boolean whether to make the file writable or not. Default is false.
 * @return The [KtFile] we created with a virtual file that physical exist
 */
fun createNewKtFile(
    fileManager: PsiManager,
    outputDir: String,
    name: String,
    content: String,
    writable: Boolean = false
): KtFile {
  val directory = File(outputDir).apply { mkdirs() }
  val file =
      File(directory, name).apply {
        writeText(content)
        setWritable(writable)
      }
  val virtualFile = CoreLocalVirtualFile(CoreLocalFileSystem(), file.toPath())
  return KtFile(SingleRootFileViewProvider(fileManager, virtualFile), false)
}

/**
 * Creates a [KtFile] which is based on a in-memory [LightVirtualFile].
 * https://dploeger.github.io/intellij-api-doc/com/intellij/testFramework/LightVirtualFile.html
 *
 * @param fileManager The PsiManager for file. A common way to acquire is from existing [KtFile] by
 *   [KtFile.getManager].
 * @param virtualFilePath The fake virtual file path. Will be displayed in error message. For
 *   example, if fake file is derived from slightly modifying an original file, the original file's
 *   virtualFilePath can be applied here in order to point error message to original file
 * @param name Name for new file
 * @param content Content for new file
 * @return The [KtFile] we created with a in-memory [LightVirtualFile]
 */
fun createFakeKtFile(
    fileManager: PsiManager,
    virtualFilePath: String,
    name: String,
    content: String,
): KtFile {
  val fakeVirtualFile = FakeVirtualFile(name, content, virtualFilePath = virtualFilePath)
  return FakeKtFile(SingleRootFileViewProvider(fileManager, fakeVirtualFile), false)
}

/**
 * One problem of using LightVirtualFile instead of generating a physical virtual file is,
 * [KotlinCoreEnvironment.createForProduction] uses [virtualFile] name as a sort Key. However,
 * KtFile here have [virtualFile] is null, even though it does have a [LightVirtualFile] in
 * [viewProvider.virtualFile].
 *
 * <p> To resolve that, we had to manually overwrite [getVirtualFile] to also use
 * [viewProvider.virtualFile]
 */
private class FakeKtFile(viewProvider: FileViewProvider, isCompiled: Boolean) :
    KtFile(viewProvider, isCompiled) {
  override fun getVirtualFile(): VirtualFile {
    return viewProvider.virtualFile
  }
}

/**
 * Another problem of using LightVirtualFile instead of generating actual physical file is,
 * LightVirtualFile have empty [getParent], so error message becomes "/MyClass.kt".
 *
 * <p> To solve that, we manually override getPath to be old file's path, so error message correctly
 * redirect user to original file.
 */
private class FakeVirtualFile(
    name: String,
    content: String,
    fileType: FileType = KotlinFileType.INSTANCE,
    private val virtualFilePath: String
) : LightVirtualFile(name, fileType, content) {
  override fun getPath(): String {
    return virtualFilePath
  }
}
