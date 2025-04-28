/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.signature.SignatureVisitor;

class SourceAbiCompatibleSignatureVisitor extends SignatureVisitorWrapper {
  public SourceAbiCompatibleSignatureVisitor(SignatureVisitor sv) {
    super(Opcodes.ASM9, sv);
  }

  @Override
  public SignatureVisitor visitClassBound() {
    // Because we can't know whether inferred types are classes or interfaces, we treat everything
    // as interfaces.
    return visitInterfaceBound();
  }
}
