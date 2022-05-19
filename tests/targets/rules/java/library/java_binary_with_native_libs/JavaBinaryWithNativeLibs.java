import java.io.IOException;

public class JavaBinaryWithNativeLibs {

  public static void main(String[] args) throws IOException {
    System.out.println("Hello from java_binary() with native_libs");
    boolean debug = args.length > 0 && args[0].equals("--debug");
    if (debug) {
      System.out.println("Library search path: " + System.getenv(getLibrarySearchPathName()));
    }

    String platform = getOsPlatform();
    if (isLinux(platform)) {

      System.loadLibrary(
          "buck2_tests_targets_rules_java_library_java_binary_with_native_libs_native");
      System.out.println("Library loaded!");
    }
  }

  private static String getLibrarySearchPathName() {
    String platform = getOsPlatform();
    if (isLinux(platform)) {
      return "LD_LIBRARY_PATH";
    }

    if (isMacOs(platform)) {
      return "DYLD_LIBRARY_PATH";
    }

    if (isWindowsOs(platform)) {
      return "PATH";
    }

    System.err.println("WARNING: using \"LD_LIBRARY_PATH\" for unrecognized platform " + platform);
    return "LD_LIBRARY_PATH";
  }

  private static String getOsPlatform() {
    return java.util.Objects.requireNonNull(System.getProperty("os.name"));
  }

  private static boolean isWindowsOs(String osPlatform) {
    return osPlatform.startsWith("Windows");
  }

  private static boolean isMacOs(String osPlatform) {
    return osPlatform.startsWith("Mac OS");
  }

  private static boolean isLinux(String osPlatform) {
    return osPlatform.startsWith("Linux");
  }
}
