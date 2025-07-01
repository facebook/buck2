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

@RunWith(CompilerTreeApiParameterized.class)
public class DescriptorFactoryTest extends DescriptorAndSignatureFactoryTestBase {
  @Test
  public void testAllTheThings() throws Exception {
    test(
        () -> {
          DescriptorFactory descriptorFactory = new DescriptorFactory(elements);
          List<String> errors =
              getTestErrors(
                  field -> field.desc,
                  method -> method.desc,
                  (t) -> null,
                  descriptorFactory::getDescriptor);

          assertTrue("Descriptor mismatch!\n\n" + Joiner.on('\n').join(errors), errors.isEmpty());
        });
  }
}
