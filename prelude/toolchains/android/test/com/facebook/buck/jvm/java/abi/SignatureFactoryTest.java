/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi;

import static org.junit.Assert.assertTrue;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiParameterized;
import com.google.common.base.Joiner;
import java.util.List;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.objectweb.asm.signature.SignatureReader;
import org.objectweb.asm.signature.SignatureWriter;

@RunWith(CompilerTreeApiParameterized.class)
public class SignatureFactoryTest extends DescriptorAndSignatureFactoryTestBase {
  @Test
  public void testAllTheThings() throws Exception {
    test(
        () -> {
          SignatureFactory signatureFactory = new SignatureFactory(new DescriptorFactory(elements));
          List<String> errors =
              getTestErrors(
                  field -> treatDependencyBoundsAsInterfaces(field.signature),
                  method -> treatDependencyBoundsAsInterfaces(method.signature),
                  type -> treatDependencyBoundsAsInterfaces(type.signature),
                  signatureFactory::getSignature);

          assertTrue("Signature mismatch!\n\n" + Joiner.on('\n').join(errors), errors.isEmpty());
        });
  }

  /**
   * We can't tell whether an inferred class is a class, interface, annotation, or enum. This is
   * problematic for expressing generic type bounds, because the bytecode is different depending on
   * whether it is a class or an interface. As it happens, it's safe (from the compiler's
   * perspective) to treat everything as an interface. This method is used to rework the "expected"
   * signature so that we can use the same test data for testing with and without deps.
   */
  private String treatDependencyBoundsAsInterfaces(String signature) {
    if (signature == null) {
      return null;
    }

    if (isTestingWithDependencies() || !signature.contains(":Lcom/facebook/foo/Dep")) {
      return signature;
    }

    SignatureWriter writer = new SignatureWriter();
    new SignatureReader(signature).accept(new SourceAbiCompatibleSignatureVisitor(writer));
    return writer.toString();
  }
}
