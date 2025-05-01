/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.android.ddmlib.IDevice;
import com.android.ddmlib.IShellOutputReceiver;
import com.android.ddmlib.InstallException;
import com.facebook.buck.android.TestDevice;
import com.facebook.buck.installer.android.IsolatedAndroidInstallerPrinter;
import com.facebook.buck.testutil.TestConsole;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;
import org.junit.Test;

public class RealAndroidDeviceTest {

  private static final Logger LOGGER = Logger.getLogger(RealAndroidDeviceTest.class.getName());

  private TestDevice createDeviceForShellCommandTest(String output) {
    return new TestDevice() {
      @Override
      public void executeShellCommand(
          String cmd, IShellOutputReceiver receiver, long timeout, TimeUnit timeoutUnit) {
        byte[] outputBytes = output.getBytes(StandardCharsets.UTF_8);
        receiver.addOutput(outputBytes, 0, outputBytes.length);
        receiver.flush();
      }
    };
  }

  private static RealAndroidDevice createAndroidDevice(IDevice device) {
    return new RealAndroidDevice(
        new IsolatedAndroidInstallerPrinter(LOGGER),
        device,
        TestConsole.createNullConsole(),
        null,
        -1,
        /* isZstdCompressionEnabled */ true,
        0,
        0);
  }

  /** Verify that successful installation on device results in true. */
  @Test
  public void testSuccessfulDeviceInstall() {
    File apk = new File("/some/file.apk");
    AtomicReference<String> apkPath = new AtomicReference<>();

    TestDevice device =
        new TestDevice() {
          @Override
          public void installPackage(String s, boolean b, String... strings) {
            apkPath.set(s);
          }
        };
    device.setSerialNumber("serial#1");
    device.setName("testDevice");

    assertTrue(createAndroidDevice(device).installApkOnDevice(apk, false, false, false, false));
    assertEquals(apk.getAbsolutePath(), apkPath.get());
  }

  /** Also make sure we're not erroneously parsing "Exception" and "Error". */
  @Test
  public void testDeviceStartActivitySuccess() {
    TestDevice device =
        createDeviceForShellCommandTest(
            "Starting: Intent { cmp=com.example.ExceptionErrorActivity }\r\n");
    assertNull(
        createAndroidDevice(device)
            .deviceStartIntent(createActivityIntent("com.foo", "com.foo/.Activity", false, false)));
  }

  @Test
  public void testDeviceStartActivityAmDoesntExist() {
    TestDevice device = createDeviceForShellCommandTest("sh: am: not found\r\n");
    assertNotNull(
        createAndroidDevice(device)
            .deviceStartIntent(createActivityIntent("com.foo", "com.foo/.Activity", false, false)));
  }

  @Test
  public void testDeviceStartActivityActivityDoesntExist() {
    String errorLine = "Error: Activity class {com.foo/.Activiqy} does not exist.\r\n";
    TestDevice device =
        createDeviceForShellCommandTest(
            "Starting: Intent { cmp=com.foo/.Activiqy }\r\n" + "Error type 3\r\n" + errorLine);
    assertEquals(
        errorLine.trim(),
        createAndroidDevice(device)
            .deviceStartIntent(createActivityIntent("com.foo", "com.foo/.Activity", false, false))
            .trim());
  }

  @Test
  public void testDeviceStartActivityException() {
    String errorLine =
        "java.lang.SecurityException: Permission Denial: "
            + "starting Intent { flg=0x10000000 cmp=com.foo/.Activity } from null "
            + "(pid=27581, uid=2000) not exported from uid 10002\r\n";
    TestDevice device =
        createDeviceForShellCommandTest(
            "Starting: Intent { cmp=com.foo/.Activity }\r\n"
                + errorLine
                + "  at android.os.Parcel.readException(Parcel.java:1425)\r\n"
                + "  at android.os.Parcel.readException(Parcel.java:1379)\r\n"
                +
                // (...)
                "  at dalvik.system.NativeStart.main(Native Method)\r\n");
    assertEquals(
        errorLine.trim(),
        createAndroidDevice(device)
            .deviceStartIntent(createActivityIntent("com.foo", "com.foo/.Activity", false, false))
            .trim());
  }

  /** Verify that if failure reason is returned, installation is marked as failed. */
  @Test
  public void testFailedDeviceInstallWithReason() {
    File apk = new File("/some/file.apk");
    TestDevice device =
        new TestDevice() {
          @Override
          public void installPackage(String s, boolean b, String... strings)
              throws InstallException {
            throw new InstallException("[SOME REASON]");
          }
        };
    device.setSerialNumber("serial#1");
    device.setName("testDevice");
    assertFalse(createAndroidDevice(device).installApkOnDevice(apk, false, false, false, false));
  }

  /** Verify that if exception is thrown during installation, installation is marked as failed. */
  @Test
  public void testFailedDeviceInstallWithException() {
    File apk = new File("/some/file.apk");

    TestDevice device =
        new TestDevice() {
          @Override
          public void installPackage(String s, boolean b, String... strings)
              throws InstallException {
            throw new InstallException("Failed to install on test device.", (String) null);
          }
        };
    device.setSerialNumber("serial#1");
    device.setName("testDevice");
    assertFalse(createAndroidDevice(device).installApkOnDevice(apk, false, false, false, false));
  }

  @Test
  public void testDeviceStartActivityWaitForDebugger() {
    AtomicReference<String> runDeviceCommand = new AtomicReference<>();
    TestDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(
              String command,
              IShellOutputReceiver receiver,
              long maxTimeToOutputResponse,
              TimeUnit maxTimeUnits) {
            runDeviceCommand.set(command);
          }
        };
    assertNull(
        createAndroidDevice(device)
            .deviceStartIntent(createActivityIntent("com.foo", "com.foo/.Activity", true, false)));
    assertTrue(runDeviceCommand.get().contains(" -D"));
  }

  @Test
  public void testDeviceStartActivityDoNotWaitForDebugger() {
    AtomicReference<String> runDeviceCommand = new AtomicReference<>();
    TestDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(
              String command,
              IShellOutputReceiver receiver,
              long maxTimeToOutputResponse,
              TimeUnit maxTimeUnits) {
            runDeviceCommand.set(command);
          }
        };
    assertNull(
        createAndroidDevice(device)
            .deviceStartIntent(createActivityIntent("com.foo", "com.foo/.Activity", false, false)));
    assertFalse(runDeviceCommand.get().contains(" -D"));
  }

  @Test
  public void testDeviceStartActivityInvokesSetDebugApp() {
    AtomicReference<ArrayList<String>> commands = new AtomicReference<>(new ArrayList<>());
    TestDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(
              String command,
              IShellOutputReceiver receiver,
              long maxTimeToOutputResponse,
              TimeUnit maxTimeUnits) {
            commands.get().add(command);
          }
        };
    assertNull(
        createAndroidDevice(device)
            .deviceStartIntent(createActivityIntent("com.foo", "com.foo/.Activity", false, false)));

    assertTrue(commands.get().get(0).contains("am set-debug-app"));
    assertTrue(commands.get().get(1).contains("am start"));
  }

  @Test
  public void testDeviceStartActivitySkipSetDebugApp() {
    AtomicReference<ArrayList<String>> commands = new AtomicReference<>(new ArrayList<>());
    TestDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(
              String command,
              IShellOutputReceiver receiver,
              long maxTimeToOutputResponse,
              TimeUnit maxTimeUnits) {
            commands.get().add(command);
          }
        };
    assertNull(
        createAndroidDevice(device)
            .deviceStartIntent(createActivityIntent("com.foo", "com.foo/.Activity", false, true)));

    // With the parameter --skip-set-debug-app specified, we should only invoke `am start`.
    assertEquals(commands.get().size(), 1);
  }

  @Test
  public void testDeviceStartIntentWithURI() {
    AtomicReference<String> runDeviceCommand = new AtomicReference<>();
    TestDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(
              String command,
              IShellOutputReceiver receiver,
              long maxTimeToOutputResponse,
              TimeUnit maxTimeUnits) {
            runDeviceCommand.set(command);
          }
        };
    assertNull(
        createAndroidDevice(device).deviceStartIntent(createURIIntent("foo://bar", false, false)));
    assertFalse(runDeviceCommand.get().contains(" -D"));
    assertTrue(runDeviceCommand.get().contains(" -d foo://bar"));
  }

  @Test
  public void testDeviceStartIntentWithURIWaitForDebugger() {
    AtomicReference<String> runDeviceCommand = new AtomicReference<>();
    TestDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(
              String command,
              IShellOutputReceiver receiver,
              long maxTimeToOutputResponse,
              TimeUnit maxTimeUnits) {
            runDeviceCommand.set(command);
          }
        };
    assertNull(
        createAndroidDevice(device).deviceStartIntent(createURIIntent("foo://bar", true, false)));
    assertTrue(runDeviceCommand.get().contains(" -D"));
    assertTrue(runDeviceCommand.get().contains(" -d foo://bar"));
  }

  private static AndroidIntent createActivityIntent(
      String packageName, String activity, boolean waitForDebugger, boolean skipSetDebugApp) {
    return new AndroidIntent(
        packageName,
        activity,
        AndroidIntent.ACTION_MAIN,
        AndroidIntent.CATEGORY_LAUNCHER,
        null,
        null,
        waitForDebugger,
        skipSetDebugApp);
  }

  private static AndroidIntent createURIIntent(
      String uri, boolean waitForDebugger, boolean skipSetDebugApp) {
    return new AndroidIntent(
        "com.foo",
        null,
        AndroidIntent.ACTION_VIEW,
        null,
        uri,
        null,
        waitForDebugger,
        skipSetDebugApp);
  }
}
