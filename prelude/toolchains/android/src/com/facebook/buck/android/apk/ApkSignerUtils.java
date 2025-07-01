/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.apk;

import com.android.apksig.ApkSigner;
import com.facebook.buck.android.apk.sdk.ApkCreationException;
import com.google.common.collect.ImmutableList;
import java.io.File;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/** Use Google apksigner to v1 + v2 sign the final APK */
public class ApkSignerUtils {

  /** Sign the APK using Google's {@link ApkSigner} */
  public static void signApkFile(
      File inputApk, File outputApk, ImmutableList<ApkSigner.SignerConfig> signerConfigs)
      throws ApkCreationException {
    ApkSigner.Builder apkSignerBuilder = new ApkSigner.Builder(signerConfigs);
    try {
      apkSignerBuilder
          .setV1SigningEnabled(true)
          .setV2SigningEnabled(true)
          .setV3SigningEnabled(false)
          .setInputApk(inputApk)
          .setOutputApk(outputApk)
          .build()
          .sign();
    } catch (Exception e) {
      throw new ApkCreationException(e, "Failed to sign APK");
    }
  }

  public static ImmutableList<ApkSigner.SignerConfig> getSignerConfigs(
      KeystoreProperties keystoreProperties, InputStream keystoreInputStream)
      throws KeyStoreException {
    char[] keystorePassword = keystoreProperties.getStorepass().toCharArray();
    String keyAlias = keystoreProperties.getAlias();
    char[] keyPassword = keystoreProperties.getKeypass().toCharArray();
    KeyStore keystore = loadKeyStore(keystoreInputStream, keystorePassword);
    PrivateKey key = loadPrivateKey(keystore, keyAlias, keyPassword);
    List<X509Certificate> certs = loadCertificates(keystore, keyAlias);
    ApkSigner.SignerConfig signerConfig =
        new ApkSigner.SignerConfig.Builder("CERT", key, certs).build();
    return ImmutableList.of(signerConfig);
  }

  private static KeyStore loadKeyStore(InputStream keystoreInputStream, char[] keystorePassword)
      throws KeyStoreException {
    try {
      String ksType = KeyStore.getDefaultType();
      KeyStore keystore = KeyStore.getInstance(ksType);
      keystore.load(keystoreInputStream, keystorePassword);
      return keystore;
    } catch (Exception e) {
      throw new KeyStoreException("Failed to load keystore", e);
    }
  }

  private static PrivateKey loadPrivateKey(KeyStore keystore, String keyAlias, char[] keyPassword)
      throws KeyStoreException {
    PrivateKey key;
    try {
      key = (PrivateKey) keystore.getKey(keyAlias, keyPassword);
      // key can be null if alias/password is incorrect.
      Objects.requireNonNull(key);
    } catch (Exception e) {
      throw new KeyStoreException(
          "key.alias [" + keyAlias + "] does not exist or does not identify a key-related entry");
    }
    return key;
  }

  private static List<X509Certificate> loadCertificates(KeyStore keystore, String keyAlias)
      throws KeyStoreException {
    Certificate[] certChain = keystore.getCertificateChain(keyAlias);
    if ((certChain == null) || (certChain.length == 0)) {
      throw new KeyStoreException(
          keystore + " entry \"" + keyAlias + "\" does not contain certificates");
    }
    List<X509Certificate> certs = new ArrayList<>(certChain.length);
    for (Certificate cert : certChain) {
      certs.add((X509Certificate) cert);
    }
    return certs;
  }
}
