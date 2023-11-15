// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

package com.facebook.kapt;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.Collections;
import java.util.List;

/**
 * Main entry point for encoding KAPT options in base64. Usage:
 * <param_key1=param_val1,param_key2=param_val2> <output_path>
 */
public class KaptBase64Encoder {

  public static void main(String[] args) throws IOException {
    String optionsFile = args[0];
    String outputPath = args[1];
    List<String> params = Files.readAllLines(Paths.get(optionsFile));

    try (ByteArrayOutputStream os = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(os)) {

      oos.writeInt(params.size());
      for (String param : params) {
        String[] splitParam = param.split("=");
        oos.writeUTF(splitParam[0]);
        oos.writeUTF(splitParam[1]);
      }

      oos.flush();
      Files.write(
          Paths.get(outputPath),
          Collections.singleton(Base64.getEncoder().encodeToString(os.toByteArray())));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    System.exit(0);
  }
}
