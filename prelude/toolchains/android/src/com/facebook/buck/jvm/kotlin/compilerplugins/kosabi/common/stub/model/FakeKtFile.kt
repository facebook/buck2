// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.common.stub.model

import com.facebook.kotlin.compilerplugins.kosabi.common.stub.render.RenderedKStub
import org.jetbrains.kotlin.com.intellij.openapi.project.Project
import org.jetbrains.kotlin.com.intellij.openapi.vfs.VirtualFile
import org.jetbrains.kotlin.com.intellij.psi.FileViewProvider
import org.jetbrains.kotlin.com.intellij.psi.PsiManager
import org.jetbrains.kotlin.com.intellij.psi.SingleRootFileViewProvider
import org.jetbrains.kotlin.com.intellij.testFramework.LightVirtualFile
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtPsiFactory

/**
 * [FakeKtFile] and [FakeVirtualFile] will be a part of the asttools common API. Currently this code
 * has a duplicates in other Meta compiler plugins.
 */

/**
 * [StubsAdditionalSourcesExtension] fakes KtFiles without having a properly created [VirtualFile]
 * anywhere in [VirtualFileSystem].
 *
 * Every [VirtualFile] fakes its path with and defaults to dummy.kt
 */
class FakeKtFile(viewProvider: FileViewProvider, isCompiled: Boolean) :
    KtFile(viewProvider, isCompiled) {
  constructor(f: KtFile) : this(f.viewProvider, f.isCompiled)

  // `KotlinCoreEnvironment::createForProduction` uses virtualFile name as a sort Key
  // Providing a default Dummy.kt
  override fun getVirtualFile(): VirtualFile {
    return viewProvider.virtualFile
  }
}

private class FakeVirtualFile(name: String, content: String, private val virtualFilePath: String) :
    LightVirtualFile(name, content) {
  override fun getPath(): String {
    return virtualFilePath
  }
}

fun generateFakeKtFile(project: Project, renderedKStub: RenderedKStub): FakeKtFile {
  // KtPsiFactory creates a [LightVirtualFile] which is a in-memory implementation of [VirtualFile]
  // https://dploeger.github.io/intellij-api-doc/com/intellij/testFramework/LightVirtualFile.html
  // For Implementation details see KtPsiFactoryImpl
  return FakeKtFile(
      KtPsiFactory(project)
          .createFile(
              "kosabi_stub.${if (renderedKStub.stub.pkg?.isNotEmpty() == true) "${renderedKStub.stub.pkg}." else ""}${renderedKStub.stub.name}.kt",
              renderedKStub.render,
          )
  )
}

fun generateFakeKtFile(
    fileManager: PsiManager,
    virtualFilePath: String,
    name: String,
    content: String,
): KtFile {
  val fakeVirtualFile = FakeVirtualFile(name, content, virtualFilePath)
  return FakeKtFile(SingleRootFileViewProvider(fileManager, fakeVirtualFile), false)
}
