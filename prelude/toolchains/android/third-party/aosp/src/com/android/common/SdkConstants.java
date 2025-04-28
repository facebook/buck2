/*
 * Copyright (C) 2007 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.android.common;

import java.io.File;

/**
 * Constant definition class.<br>
 * <br>
 * Most constants have a prefix defining the content.
 * <ul>
 * <li><code>OS_</code> OS path constant. These paths are different depending on the platform.</li>
 * <li><code>FN_</code> File name constant.</li>
 * <li><code>FD_</code> Folder name constant.</li>
 * <li><code>TAG_</code> XML element tag name</li>
 * <li><code>ATTR_</code> XML attribute name</li>
 * <li><code>VALUE_</code> XML attribute value</li>
 * <li><code>CLASS_</code> Class name</li>
 * <li><code>DOT_</code> File name extension, including the dot </li>
 * <li><code>EXT_</code> File name extension, without the dot </li>
 * </ul>
 */
@SuppressWarnings({"javadoc", "unused"}) // Not documenting all the fields here
public final class SdkConstants {
    public static final int PLATFORM_UNKNOWN = 0;
    public static final int PLATFORM_LINUX = 1;
    public static final int PLATFORM_WINDOWS = 2;
    public static final int PLATFORM_DARWIN = 3;

    /**
     * Returns current platform, one of {@link #PLATFORM_WINDOWS}, {@link #PLATFORM_DARWIN},
     * {@link #PLATFORM_LINUX} or {@link #PLATFORM_UNKNOWN}.
     */
    public static final int CURRENT_PLATFORM = currentPlatform();

    /** Environment variable that specifies the path of an Android SDK. */
    public static final String ANDROID_HOME_ENV = "ANDROID_HOME";

    /** Property in local.properties file that specifies the path of the Android SDK.  */
    public static final String SDK_DIR_PROPERTY = "sdk.dir";

    /** Property in local.properties file that specifies the path of the Android NDK.  */
    public static final String NDK_DIR_PROPERTY = "ndk.dir";

    /** Property in gradle-wrapper.properties file that specifies the URL to the correct Gradle distribution. */
    public static final String GRADLE_DISTRIBUTION_URL_PROPERTY = "distributionUrl";  //$NON-NLS-1$

    /**
     * The encoding we strive to use for all files we write.
     * <p>
     * When possible, use the APIs which take a {@link java.nio.charset.Charset} and pass in
     * {@link com.google.common.base.Charsets#UTF_8} instead of using the String encoding
     * method.
     */
    public static final String UTF_8 = "UTF-8";                                       //$NON-NLS-1$

    /**
     * Charset for the ini file handled by the SDK.
     */
    public static final String INI_CHARSET = UTF_8;

    /** Path separator used by Gradle */
    public static final String GRADLE_PATH_SEPARATOR = ":";                           //$NON-NLS-1$

    /** An SDK Project's AndroidManifest.xml file */
    public static final String FN_ANDROID_MANIFEST_XML= "AndroidManifest.xml";        //$NON-NLS-1$
    /** pre-dex jar filename. i.e. "classes.jar" */
    public static final String FN_CLASSES_JAR = "classes.jar";                        //$NON-NLS-1$
    /** Dex filename inside the APK. i.e. "classes.dex" */
    public static final String FN_APK_CLASSES_DEX = "classes.dex";                    //$NON-NLS-1$
    /** Dex filename inside the APK. i.e. "classes.dex" */
    public static final String FN_APK_CLASSES_N_DEX = "classes%d.dex";                //$NON-NLS-1$

    /** An SDK Project's build.xml file */
    public static final String FN_BUILD_XML = "build.xml";                            //$NON-NLS-1$
    /** An SDK Project's build.gradle file */
    public static final String FN_BUILD_GRADLE = "build.gradle";                      //$NON-NLS-1$
    /** An SDK Project's settings.gradle file */
    public static final String FN_SETTINGS_GRADLE = "settings.gradle";                //$NON-NLS-1$
    /** An SDK Project's gradle.properties file */
    public static final String FN_GRADLE_PROPERTIES = "gradle.properties";            //$NON-NLS-1$
    /** An SDK Project's gradle daemon executable */
    public static final String FN_GRADLE_UNIX = "gradle";                             //$NON-NLS-1$
    /** An SDK Project's gradle.bat daemon executable (gradle for windows) */
    public static final String FN_GRADLE_WIN = FN_GRADLE_UNIX + ".bat";               //$NON-NLS-1$
    /** An SDK Project's gradlew file */
    public static final String FN_GRADLE_WRAPPER_UNIX = "gradlew";                    //$NON-NLS-1$
    /** An SDK Project's gradlew.bat file (gradlew for windows) */
    public static final String FN_GRADLE_WRAPPER_WIN = FN_GRADLE_WRAPPER_UNIX + ".bat"; //$NON-NLS-1$
    /** An SDK Project's gradle wrapper library */
    public static final String FN_GRADLE_WRAPPER_JAR = "gradle-wrapper.jar";          //$NON-NLS-1$
    /** Name of the framework library, i.e. "android.jar" */
    public static final String FN_FRAMEWORK_LIBRARY = "android.jar";                  //$NON-NLS-1$
    /** Name of the framework library, i.e. "uiautomator.jar" */
    public static final String FN_UI_AUTOMATOR_LIBRARY = "uiautomator.jar";           //$NON-NLS-1$
    /** Name of the layout attributes, i.e. "attrs.xml" */
    public static final String FN_ATTRS_XML = "attrs.xml";                            //$NON-NLS-1$
    /** Name of the layout attributes, i.e. "attrs_manifest.xml" */
    public static final String FN_ATTRS_MANIFEST_XML = "attrs_manifest.xml";          //$NON-NLS-1$
    /** framework aidl import file */
    public static final String FN_FRAMEWORK_AIDL = "framework.aidl";                  //$NON-NLS-1$
    /** framework renderscript folder */
    public static final String FN_FRAMEWORK_RENDERSCRIPT = "renderscript";            //$NON-NLS-1$
    /** framework include folder */
    public static final String FN_FRAMEWORK_INCLUDE = "include";                      //$NON-NLS-1$
    /** framework include (clang) folder */
    public static final String FN_FRAMEWORK_INCLUDE_CLANG = "clang-include";          //$NON-NLS-1$
    /** layoutlib.jar file */
    public static final String FN_LAYOUTLIB_JAR = "layoutlib.jar";                    //$NON-NLS-1$
    /** widget list file */
    public static final String FN_WIDGETS = "widgets.txt";                            //$NON-NLS-1$
    /** Intent activity actions list file */
    public static final String FN_INTENT_ACTIONS_ACTIVITY = "activity_actions.txt";   //$NON-NLS-1$
    /** Intent broadcast actions list file */
    public static final String FN_INTENT_ACTIONS_BROADCAST = "broadcast_actions.txt"; //$NON-NLS-1$
    /** Intent service actions list file */
    public static final String FN_INTENT_ACTIONS_SERVICE = "service_actions.txt";     //$NON-NLS-1$
    /** Intent category list file */
    public static final String FN_INTENT_CATEGORIES = "categories.txt";               //$NON-NLS-1$

    /** annotations support jar */
    public static final String FN_ANNOTATIONS_JAR = "annotations.jar";                //$NON-NLS-1$

    /** platform build property file */
    public static final String FN_BUILD_PROP = "build.prop";                          //$NON-NLS-1$
    /** plugin properties file */
    public static final String FN_PLUGIN_PROP = "plugin.prop";                        //$NON-NLS-1$
    /** add-on manifest file */
    public static final String FN_MANIFEST_INI = "manifest.ini";                      //$NON-NLS-1$
    /** add-on layout device XML file. */
    public static final String FN_DEVICES_XML = "devices.xml";                        //$NON-NLS-1$
    /** hardware properties definition file */
    public static final String FN_HARDWARE_INI = "hardware-properties.ini";           //$NON-NLS-1$

    /** project property file */
    public static final String FN_PROJECT_PROPERTIES = "project.properties";          //$NON-NLS-1$

    /** project local property file */
    public static final String FN_LOCAL_PROPERTIES = "local.properties";              //$NON-NLS-1$

    /** project ant property file */
    public static final String FN_ANT_PROPERTIES = "ant.properties";                  //$NON-NLS-1$

    /** project local property file */
    public static final String FN_GRADLE_WRAPPER_PROPERTIES = "gradle-wrapper.properties"; //$NON-NLS-1$

    /** Skin layout file */
    public static final String FN_SKIN_LAYOUT = "layout";                             //$NON-NLS-1$

    /** dx.jar file */
    public static final String FN_DX_JAR = "dx.jar";                                  //$NON-NLS-1$

    /** dx executable (with extension for the current OS) */
    public static final String FN_DX =
        "dx" + ext(".bat", "");                           //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** aapt executable (with extension for the current OS) */
    public static final String FN_AAPT =
        "aapt" + ext(".exe", "");                         //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** aapt2 executable (with extension for the current OS) */
    public static final String FN_AAPT2 =
            "aapt2" + ext(".exe", "");                    //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** aidl executable (with extension for the current OS) */
    public static final String FN_AIDL =
        "aidl" + ext(".exe", "");                         //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** renderscript executable (with extension for the current OS) */
    public static final String FN_RENDERSCRIPT =
        "llvm-rs-cc" + ext(".exe", "");                   //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** renderscript support exe (with extension for the current OS) */
    public static final String FN_BCC_COMPAT =
            "bcc_compat" + ext(".exe", "");               //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** renderscript support linker for ARM (with extension for the current OS) */
    public static final String FN_LD_ARM =
            "arm-linux-androideabi-ld" + ext(".exe", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** renderscript support linker for ARM64 (with extension for the current OS) */
    public static final String FN_LD_ARM64 =
            "aarch64-linux-android-ld" + ext(".exe", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** renderscript support linker for X86 (with extension for the current OS) */
    public static final String FN_LD_X86 =
            "i686-linux-android-ld" + ext(".exe", "");   //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** renderscript support linker for X86_64 (with extension for the current OS) */
    public static final String FN_LD_X86_64 =
            "x86_64-linux-android-ld" + ext(".exe", "");   //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** renderscript support linker for MIPS (with extension for the current OS) */
    public static final String FN_LD_MIPS =
            "mipsel-linux-android-ld" + ext(".exe", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** adb executable (with extension for the current OS) */
    public static final String FN_ADB =
        "adb" + ext(".exe", "");                          //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** emulator executable for the current OS */
    public static final String FN_EMULATOR =
        "emulator" + ext(".exe", "");                     //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** emulator-check executable for the current OS */
    public static final String FN_EMULATOR_CHECK =
      "emulator-check" + ext(".exe", "");                 //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** zipalign executable (with extension for the current OS) */
    public static final String FN_ZIPALIGN =
        "zipalign" + ext(".exe", "");                     //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** dexdump executable (with extension for the current OS) */
    public static final String FN_DEXDUMP =
        "dexdump" + ext(".exe", "");                      //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** proguard executable (with extension for the current OS) */
    public static final String FN_PROGUARD =
        "proguard" + ext(".bat", ".sh");                  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** find_lock for Windows (with extension for the current OS) */
    public static final String FN_FIND_LOCK =
        "find_lock" + ext(".exe", "");                    //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** hprof-conv executable (with extension for the current OS) */
    public static final String FN_HPROF_CONV =
        "hprof-conv" + ext(".exe", "");                   //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    /** jack.jar */
    public static final String FN_JACK = "jack.jar";                                  //$NON-NLS-1$
    /** jill.jar */
    public static final String FN_JILL = "jill.jar";                                  //$NON-NLS-1$
    /** code coverage plugin for jack */
    public static final String FN_JACK_COVERAGE_PLUGIN = "jack-coverage-plugin.jar";  //$NON-NLS-1$
    /** jack-jacoco-report.jar */
    public static final String FN_JACK_JACOCO_REPORTER = "jack-jacoco-reporter.jar";  //$NON-NLS-1$

    /** split-select */
    public static final String FN_SPLIT_SELECT = "split-select" + ext(".exe", "");

    /** glslc */
    public static final String FD_SHADER_TOOLS = "shader-tools";
    public static final String FN_GLSLC = "glslc" + ext(".exe", "");


    /** properties file for SDK Updater packages */
    public static final String FN_SOURCE_PROP = "source.properties";                  //$NON-NLS-1$
    /** properties file for content hash of installed packages */
    public static final String FN_CONTENT_HASH_PROP = "content_hash.properties";      //$NON-NLS-1$
    /** properties file for the SDK */
    public static final String FN_SDK_PROP = "sdk.properties";                        //$NON-NLS-1$


    public static final String FN_RENDERSCRIPT_V8_JAR = "renderscript-v8.jar"; //$NON-NLS-1$

    /**
     * filename for gdbserver.
     */
    public static final String FN_GDBSERVER = "gdbserver";              //$NON-NLS-1$
    public static final String FN_GDB_SETUP = "gdb.setup";              //$NON-NLS-1$

    /** filename for wrap.sh */
    public static final String FN_WRAP_SH = "wrap.sh";

    /** global Android proguard config file */
    public static final String FN_ANDROID_PROGUARD_FILE = "proguard-android.txt";   //$NON-NLS-1$
    /** global Android proguard config file with optimization enabled */
    public static final String FN_ANDROID_OPT_PROGUARD_FILE = "proguard-android-optimize.txt";  //$NON-NLS-1$
    /** default proguard config file with new file extension (for project specific stuff) */
    public static final String FN_PROJECT_PROGUARD_FILE = "proguard-project.txt";   //$NON-NLS-1$

    /* Folder Names for Android Projects . */

    /** Resources folder name, i.e. "res". */
    public static final String FD_RESOURCES = "res";                    //$NON-NLS-1$
    /** Assets folder name, i.e. "assets" */
    public static final String FD_ASSETS = "assets";                    //$NON-NLS-1$
    /**
     * Default source folder name in an SDK project, i.e. "src".
     * <p>
     * Note: this is not the same as {@link #FD_PKG_SOURCES}
     * which is an SDK sources folder for packages. */
    public static final String FD_SOURCES = "src";                      //$NON-NLS-1$
    /** Default main source set folder name, i.e. "main" */
    public static final String FD_MAIN = "main";                        //$NON-NLS-1$
    /** Default test source set folder name, i.e. "androidTest" */
    public static final String FD_TEST = "androidTest";                 //$NON-NLS-1$
    /** Default java code folder name, i.e. "java" */
    public static final String FD_JAVA = "java";                        //$NON-NLS-1$
    /** Default native code folder name, i.e. "jni" */
    public static final String FD_JNI = "jni";                          //$NON-NLS-1$
    /** Default gradle folder name, i.e. "gradle" */
    public static final String FD_GRADLE = "gradle";                    //$NON-NLS-1$
    /** Default gradle wrapper folder name, i.e. "gradle/wrapper" */
    public static final String FD_GRADLE_WRAPPER = FD_GRADLE + File.separator + "wrapper"; //$NON-NLS-1$
    /** Default generated source folder name, i.e. "gen" */
    public static final String FD_GEN_SOURCES = "gen";                  //$NON-NLS-1$
    /** Default native library folder name inside the project, i.e. "libs"
     * While the folder inside the .apk is "lib", we call that one libs because
     * that's what we use in ant for both .jar and .so and we need to make the 2 development ways
     * compatible. */
    public static final String FD_NATIVE_LIBS = "libs";                 //$NON-NLS-1$
    /** Native lib folder inside the APK: "lib" */
    public static final String FD_APK_NATIVE_LIBS = "lib";              //$NON-NLS-1$
    /** Default output folder name, i.e. "bin" */
    public static final String FD_OUTPUT = "bin";                       //$NON-NLS-1$
    /** Classes output folder name, i.e. "classes" */
    public static final String FD_CLASSES_OUTPUT = "classes";           //$NON-NLS-1$
    /** proguard output folder for mapping, etc.. files */
    public static final String FD_PROGUARD = "proguard";                //$NON-NLS-1$
    /** aidl output folder for copied aidl files */
    public static final String FD_AIDL = "aidl";                        //$NON-NLS-1$

    /** rs Libs output folder for support mode */
    public static final String FD_RS_LIBS = "rsLibs";                   //$NON-NLS-1$
    /** rs Libs output folder for support mode */
    public static final String FD_RS_OBJ = "rsObj";                     //$NON-NLS-1$

    /** jars folder */
    public static final String FD_JARS = "jars";                        //$NON-NLS-1$

    /* Folder Names for the Android SDK */

    /** Name of the SDK platforms folder. */
    public static final String FD_PLATFORMS = "platforms";              //$NON-NLS-1$
    /** Name of the SDK addons folder. */
    public static final String FD_ADDONS = "add-ons";                   //$NON-NLS-1$
    /** Name of the SDK system-images folder. */
    public static final String FD_SYSTEM_IMAGES = "system-images";      //$NON-NLS-1$
    /** Name of the SDK sources folder where source packages are installed.
     * <p>
     * Note this is not the same as {@link #FD_SOURCES} which is the folder name where sources
     * are installed inside a project. */
    public static final String FD_PKG_SOURCES = "sources";              //$NON-NLS-1$
    /** Name of the SDK tools folder. */
    public static final String FD_TOOLS = "tools";                      //$NON-NLS-1$
    /** Name of the SDK tools/support folder. */
    public static final String FD_SUPPORT = "support";                  //$NON-NLS-1$
    /** Name of the SDK platform tools folder. */
    public static final String FD_PLATFORM_TOOLS = "platform-tools";    //$NON-NLS-1$
    /** Name of the SDK build tools folder. */
    public static final String FD_BUILD_TOOLS = "build-tools";          //$NON-NLS-1$
    /** Name of the SDK tools/lib folder. */
    public static final String FD_LIB = "lib";                          //$NON-NLS-1$
    /** Name of the SDK docs folder. */
    public static final String FD_DOCS = "docs";                        //$NON-NLS-1$
    /** Name of the doc folder containing API reference doc (javadoc) */
    public static final String FD_DOCS_REFERENCE = "reference";         //$NON-NLS-1$
    /** Name of the SDK images folder. */
    public static final String FD_IMAGES = "images";                    //$NON-NLS-1$
    /** Name of the ABI to support. */
    public static final String ABI_ARMEABI      = "armeabi";            //$NON-NLS-1$
    public static final String ABI_ARMEABI_V7A  = "armeabi-v7a";        //$NON-NLS-1$
    public static final String ABI_ARM64_V8A    = "arm64-v8a";          //$NON-NLS-1$
    public static final String ABI_INTEL_ATOM   = "x86";                //$NON-NLS-1$
    public static final String ABI_INTEL_ATOM64 = "x86_64";             //$NON-NLS-1$
    public static final String ABI_MIPS         = "mips";               //$NON-NLS-1$
    public static final String ABI_MIPS64       = "mips64";             //$NON-NLS-1$
    /** Name of the CPU arch to support. */
    public static final String CPU_ARCH_ARM          = "arm";           //$NON-NLS-1$
    public static final String CPU_ARCH_ARM64        = "arm64";         //$NON-NLS-1$
    public static final String CPU_ARCH_INTEL_ATOM   = "x86";           //$NON-NLS-1$
    public static final String CPU_ARCH_INTEL_ATOM64 = "x86_64";        //$NON-NLS-1$
    public static final String CPU_ARCH_MIPS         = "mips";          //$NON-NLS-1$
    /** TODO double-check this is appropriate value for mips64 */
    public static final String CPU_ARCH_MIPS64       = "mips64";        //$NON-NLS-1$
    /** Name of the CPU model to support. */
    public static final String CPU_MODEL_CORTEX_A8 = "cortex-a8";       //$NON-NLS-1$

    /** Name of the SDK skins folder. */
    public static final String FD_SKINS = "skins";                      //$NON-NLS-1$
    /** Name of the SDK samples folder. */
    public static final String FD_SAMPLES = "samples";                  //$NON-NLS-1$
    /** Name of the SDK extras folder. */
    public static final String FD_EXTRAS = "extras";                    //$NON-NLS-1$
    public static final String FD_M2_REPOSITORY = "m2repository";       //$NON-NLS-1$
    public static final String FD_NDK = "ndk-bundle";                   //$NON-NLS-1$
    public static final String FD_LLDB = "lldb";                        //$NON-NLS-1$
    public static final String FD_CMAKE = "cmake";                      //$NON-NLS-1$
    /**
     * Name of an extra's sample folder.
     * Ideally extras should have one {@link #FD_SAMPLES} folder containing
     * one or more sub-folders (one per sample). However some older extras
     * might contain a single "sample" folder with directly the samples files
     * in it. When possible we should encourage extras' owners to move to the
     * multi-samples format.
     */
    public static final String FD_SAMPLE = "sample";                    //$NON-NLS-1$
    /** Name of the SDK templates folder, i.e. "templates" */
    public static final String FD_TEMPLATES = "templates";              //$NON-NLS-1$
    /** Name of the SDK Ant folder, i.e. "ant" */
    public static final String FD_ANT = "ant";                          //$NON-NLS-1$
    /** Name of the SDK data folder, i.e. "data" */
    public static final String FD_DATA = "data";                        //$NON-NLS-1$
    /** Name of the SDK renderscript folder, i.e. "rs" */
    public static final String FD_RENDERSCRIPT = "rs";                  //$NON-NLS-1$
    /** Name of the Java resources folder, i.e. "resources" */
    public static final String FD_JAVA_RES = "resources";               //$NON-NLS-1$
    /** Name of the SDK resources folder, i.e. "res" */
    public static final String FD_RES = "res";                          //$NON-NLS-1$
    /** Name of the SDK font folder, i.e. "fonts" */
    public static final String FD_FONTS = "fonts";                      //$NON-NLS-1$
    /** Name of the android sources directory and the root of the SDK sources package folder. */
    public static final String FD_ANDROID_SOURCES = "sources";          //$NON-NLS-1$
    /** Name of the addon libs folder. */
    public static final String FD_ADDON_LIBS = "libs";                  //$NON-NLS-1$

    /** Name of the cache folder in the $HOME/.android. */
    public static final String FD_CACHE = "cache";                      //$NON-NLS-1$

    /** API codename of a release (non preview) system image or platform. **/
    public static final String CODENAME_RELEASE = "REL";                //$NON-NLS-1$

    /** Namespace for the resource XML, i.e. "http://schemas.android.com/apk/res/android" */
    public static final String NS_RESOURCES =
        "http://schemas.android.com/apk/res/android";                   //$NON-NLS-1$

    /**
     * Namespace pattern for the custom resource XML, i.e. "http://schemas.android.com/apk/res/%s"
     * <p>
     * This string contains a %s. It must be combined with the desired Java package, e.g.:
     * <pre>
     *    String.format(SdkConstants.NS_CUSTOM_RESOURCES_S, "android");
     *    String.format(SdkConstants.NS_CUSTOM_RESOURCES_S, "com.test.mycustomapp");
     * </pre>
     *
     * Note: if you need an URI specifically for the "android" namespace, consider using
     * {@link SdkConstants#NS_RESOURCES} instead.
     */
    public static final String NS_CUSTOM_RESOURCES_S = "http://schemas.android.com/apk/res/%1$s"; //$NON-NLS-1$


    /** The name of the uses-library that provides "android.test.runner" */
    public static final String ANDROID_TEST_RUNNER_LIB =
        "android.test.runner";                                          //$NON-NLS-1$

    /* Folder path relative to the SDK root */
    /** Path of the documentation directory relative to the sdk folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_SDK_DOCS_FOLDER = FD_DOCS + File.separator;

    /** Path of the tools directory relative to the sdk folder, or to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_SDK_TOOLS_FOLDER = FD_TOOLS + File.separator;

    /** Path of the lib directory relative to the sdk folder, or to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_SDK_TOOLS_LIB_FOLDER =
            OS_SDK_TOOLS_FOLDER + FD_LIB + File.separator;

    /**
     * Path of the lib directory relative to the sdk folder, or to a platform
     * folder. This is an OS path, ending with a separator.
     */
    public static final String OS_SDK_TOOLS_LIB_EMULATOR_FOLDER = OS_SDK_TOOLS_LIB_FOLDER
            + "emulator" + File.separator;                              //$NON-NLS-1$

    /** Path of the platform tools directory relative to the sdk folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_SDK_PLATFORM_TOOLS_FOLDER = FD_PLATFORM_TOOLS + File.separator;

    /** Path of the build tools directory relative to the sdk folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_SDK_BUILD_TOOLS_FOLDER = FD_BUILD_TOOLS + File.separator;

    /** Path of the Platform tools Lib directory relative to the sdk folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_SDK_PLATFORM_TOOLS_LIB_FOLDER =
            OS_SDK_PLATFORM_TOOLS_FOLDER + FD_LIB + File.separator;

    /** Path of the bin folder of proguard folder relative to the sdk folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_SDK_TOOLS_PROGUARD_BIN_FOLDER =
        OS_SDK_TOOLS_FOLDER +
        "proguard" + File.separator +                                   //$NON-NLS-1$
        "bin" + File.separator;                                         //$NON-NLS-1$

    /** Path of the template gradle wrapper folder relative to the sdk folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_SDK_TOOLS_TEMPLATES_GRADLE_WRAPPER_FOLDER =
      OS_SDK_TOOLS_FOLDER + FD_TEMPLATES + File.separator + FD_GRADLE_WRAPPER + File.separator;

    /* Folder paths relative to a platform or add-on folder */

    /** Path of the images directory relative to a platform or addon folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_IMAGES_FOLDER = FD_IMAGES + File.separator;

    /** Path of the skin directory relative to a platform or addon folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_SKINS_FOLDER = FD_SKINS + File.separator;

    /* Folder paths relative to a Platform folder */

    /** Path of the data directory relative to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_PLATFORM_DATA_FOLDER = FD_DATA + File.separator;

    /** Path of the renderscript directory relative to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_PLATFORM_RENDERSCRIPT_FOLDER = FD_RENDERSCRIPT + File.separator;


    /** Path of the samples directory relative to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_PLATFORM_SAMPLES_FOLDER = FD_SAMPLES + File.separator;

    /** Path of the resources directory relative to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_PLATFORM_RESOURCES_FOLDER =
            OS_PLATFORM_DATA_FOLDER + FD_RES + File.separator;

    /** Path of the fonts directory relative to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_PLATFORM_FONTS_FOLDER =
            OS_PLATFORM_DATA_FOLDER + FD_FONTS + File.separator;

    /** Path of the android source directory relative to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_PLATFORM_SOURCES_FOLDER = FD_ANDROID_SOURCES + File.separator;

    /** Path of the android templates directory relative to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_PLATFORM_TEMPLATES_FOLDER = FD_TEMPLATES + File.separator;

    /** Path of the Ant build rules directory relative to a platform folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_PLATFORM_ANT_FOLDER = FD_ANT + File.separator;

    /** Path of the attrs.xml file relative to a platform folder. */
    public static final String OS_PLATFORM_ATTRS_XML =
            OS_PLATFORM_RESOURCES_FOLDER + SdkConstants.FD_RES_VALUES + File.separator +
            FN_ATTRS_XML;

    /** Path of the attrs_manifest.xml file relative to a platform folder. */
    public static final String OS_PLATFORM_ATTRS_MANIFEST_XML =
            OS_PLATFORM_RESOURCES_FOLDER + SdkConstants.FD_RES_VALUES + File.separator +
            FN_ATTRS_MANIFEST_XML;

    /** Path of the layoutlib.jar file relative to a platform folder. */
    public static final String OS_PLATFORM_LAYOUTLIB_JAR =
            OS_PLATFORM_DATA_FOLDER + FN_LAYOUTLIB_JAR;

    /** Path of the renderscript include folder relative to a platform folder. */
    public static final String OS_FRAMEWORK_RS =
            FN_FRAMEWORK_RENDERSCRIPT + File.separator + FN_FRAMEWORK_INCLUDE;
    /** Path of the renderscript (clang) include folder relative to a platform folder. */
    public static final String OS_FRAMEWORK_RS_CLANG =
            FN_FRAMEWORK_RENDERSCRIPT + File.separator + FN_FRAMEWORK_INCLUDE_CLANG;

    /* Folder paths relative to a addon folder */
    /** Path of the images directory relative to a folder folder.
     *  This is an OS path, ending with a separator. */
    public static final String OS_ADDON_LIBS_FOLDER = FD_ADDON_LIBS + File.separator;

    /** Skin default **/
    public static final String SKIN_DEFAULT = "default";                    //$NON-NLS-1$

    /** SDK property: ant templates revision */
    public static final String PROP_SDK_ANT_TEMPLATES_REVISION =
        "sdk.ant.templates.revision";                                       //$NON-NLS-1$

    /** SDK property: default skin */
    public static final String PROP_SDK_DEFAULT_SKIN = "sdk.skin.default"; //$NON-NLS-1$

    /* Android Class Constants */
    public static final String CLASS_ACTIVITY = "android.app.Activity"; //$NON-NLS-1$
    public static final String CLASS_APPLICATION = "android.app.Application"; //$NON-NLS-1$
    public static final String CLASS_SERVICE = "android.app.Service"; //$NON-NLS-1$
    public static final String CLASS_BROADCASTRECEIVER = "android.content.BroadcastReceiver"; //$NON-NLS-1$
    public static final String CLASS_CONTENTPROVIDER = "android.content.ContentProvider"; //$NON-NLS-1$
    public static final String CLASS_ATTRIBUTE_SET = "android.util.AttributeSet"; //$NON-NLS-1$
    public static final String CLASS_INSTRUMENTATION = "android.app.Instrumentation"; //$NON-NLS-1$
    public static final String CLASS_INSTRUMENTATION_RUNNER =
        "android.test.InstrumentationTestRunner"; //$NON-NLS-1$
    public static final String CLASS_BUNDLE = "android.os.Bundle"; //$NON-NLS-1$
    public static final String CLASS_R = "android.R"; //$NON-NLS-1$
    public static final String CLASS_R_PREFIX = CLASS_R + "."; //$NON-NLS-1$
    public static final String CLASS_MANIFEST_PERMISSION = "android.Manifest$permission"; //$NON-NLS-1$
    public static final String CLASS_INTENT = "android.content.Intent"; //$NON-NLS-1$
    public static final String CLASS_CONTEXT = "android.content.Context"; //$NON-NLS-1$
    public static final String CLASS_RESOURCES = "android.content.res.Resources"; //$NON-NLS-1$
    public static final String CLS_TYPED_ARRAY = "android.content.res.TypedArray";   //$NON-NLS-1$
    public static final String CLASS_VIEW = "android.view.View"; //$NON-NLS-1$
    public static final String CLASS_VIEWGROUP = "android.view.ViewGroup"; //$NON-NLS-1$
    public static final String CLASS_NAME_LAYOUTPARAMS = "LayoutParams"; //$NON-NLS-1$
    public static final String CLASS_VIEWGROUP_LAYOUTPARAMS =
        CLASS_VIEWGROUP + "$" + CLASS_NAME_LAYOUTPARAMS; //$NON-NLS-1$
    public static final String CLASS_NAME_FRAMELAYOUT = "FrameLayout"; //$NON-NLS-1$
    public static final String CLASS_FRAMELAYOUT =
        "android.widget." + CLASS_NAME_FRAMELAYOUT; //$NON-NLS-1$
    public static final String CLASS_PREFERENCE = "android.preference.Preference"; //$NON-NLS-1$
    public static final String CLASS_NAME_PREFERENCE_SCREEN = "PreferenceScreen"; //$NON-NLS-1$
    public static final String CLASS_PREFERENCES =
        "android.preference." + CLASS_NAME_PREFERENCE_SCREEN; //$NON-NLS-1$
    public static final String CLASS_PREFERENCEGROUP = "android.preference.PreferenceGroup"; //$NON-NLS-1$
    public static final String CLASS_PARCELABLE = "android.os.Parcelable"; //$NON-NLS-1$
    public static final String CLASS_PARCEL = "android.os.Parcel"; //$NON-NLS-1$
    public static final String CLASS_FRAGMENT = "android.app.Fragment"; //$NON-NLS-1$
    public static final String CLASS_V4_FRAGMENT = "android.support.v4.app.Fragment"; //$NON-NLS-1$
    public static final String CLASS_ACTION_PROVIDER = "android.view.ActionProvider"; //$NON-NLS-1$
    public static final String CLASS_BACKUP_AGENT = "android.app.backup.BackupAgent"; //$NON-NLS-1$
    /** MockView is part of the layoutlib bridge and used to display classes that have
     * no rendering in the graphical layout editor. */
    public static final String CLASS_MOCK_VIEW = "com.android.layoutlib.bridge.MockView"; //$NON-NLS-1$
    public static final String CLASS_LAYOUT_INFLATER = "android.view.LayoutInflater"; //$NON-NLS-1$
    public static final String CLASS_DATA_BINDING_COMPONENT = "android.databinding.DataBindingComponent"; //$NON-NLS-1$
    public static final String CLASS_NAME_DATA_BINDING_COMPONENT = "DataBindingComponent"; //$NON-NLS-1$
    public static final String CLASS_DATA_BINDING_BASE_BINDING = "android.databinding.ViewDataBinding";

    /* Android Support Class Constants */
    public static final String CLASS_COORDINATOR_LAYOUT = "android.support.design.widget.CoordinatorLayout"; //$NON-NLS-1$
    public static final String CLASS_APP_BAR_LAYOUT = "android.support.design.widget.AppBarLayout"; //$NON-NLS-1$
    public static final String CLASS_FLOATING_ACTION_BUTTON = "android.support.design.widget.FloatingActionButton"; //$NON-NLS-1$
    public static final String CLASS_COLLAPSING_TOOLBAR_LAYOUT = "android.support.design.widget.CollapsingToolbarLayout"; //$NON-NLS-1$
    public static final String CLASS_NAVIGATION_VIEW = "android.support.design.widget.NavigationView"; //$NON-NLS-1$
    public static final String CLASS_SNACKBAR = "android.support.design.widget.Snackbar"; //$NON-NLS-1$
    public static final String CLASS_TAB_LAYOUT = "android.support.design.widget.TabLayout"; //$NON-NLS-1$
    public static final String CLASS_TAB_ITEM = "android.support.design.widget.TabItem"; //$NON-NLS-1$
    public static final String CLASS_TEXT_INPUT_LAYOUT = "android.support.design.widget.TextInputLayout"; //$NON-NLS-1$
    public static final String CLASS_NESTED_SCROLL_VIEW = "android.support.v4.widget.NestedScrollView";  //$NON-NLS-1$
    public static final String CLASS_VIEW_PAGER = "android.support.v4.view.ViewPager";  //$NON-NLS-1$
    public static final String CLASS_DRAWER_LAYOUT = "android.support.v4.widget.DrawerLayout";  //$NON-NLS-1$
    public static final String CLASS_GRID_LAYOUT_V7 = "android.support.v7.widget.GridLayout";  //$NON-NLS-1$
    public static final String CLASS_TOOLBAR_V7 = "android.support.v7.widget.Toolbar";  //$NON-NLS-1$
    public static final String CLASS_RECYCLER_VIEW = "android.support.v7.widget.RecyclerView";  //$NON-NLS-1$
    public static final String CLASS_CARD_VIEW = "android.support.v7.widget.CardView";  //$NON-NLS-1$
    public static final String CLASS_ACTION_MENU_VIEW = "android.support.v7.widget.ActionMenuView";  //$NON-NLS-1$
    public static final String CLASS_AD_VIEW = "com.google.android.gms.ads.AdView";  //$NON-NLS-1$
    public static final String CLASS_MAP_FRAGMENT = "com.google.android.gms.maps.MapFragment";  //$NON-NLS-1$
    public static final String CLASS_MAP_VIEW = "com.google.android.gms.maps.MapView";  //$NON-NLS-1$
    public static final String CLASS_BROWSE_FRAGMENT = "android.support.v17.leanback.app.BrowseFragment";  //$NON-NLS-1$
    public static final String CLASS_DETAILS_FRAGMENT = "android.support.v17.leanback.app.DetailsFragment";  //$NON-NLS-1$
    public static final String CLASS_PLAYBACK_OVERLAY_FRAGMENT = "android.support.v17.leanback.app.PlaybackOverlayFragment";  //$NON-NLS-1$
    public static final String CLASS_SEARCH_FRAGMENT = "android.support.v17.leanback.app.SearchFragment";  //$NON-NLS-1$
    public static final String CLASS_PERCENT_RELATIVE_LAYOUT = "android.support.percent.PercentRelativeLayout"; //$NON-NLS-1$
    public static final String CLASS_PERCENT_FRAME_LAYOUT = "android.support.percent.PercentFrameLayout"; //$NON-NLS-1$

    /* Android ConstraintLayout Constants */
    public static final String CLASS_CONSTRAINT_LAYOUT = "android.support.constraint.ConstraintLayout";     //$NON-NLS-1$
    public static final String CLASS_CONSTRAINT_LAYOUT_PARAMS = "android.support.constraint.ConstraintLayout$LayoutParams";     //$NON-NLS-1$
    public static final String CLASS_TABLE_CONSTRAINT_LAYOUT = "android.support.constraint.TableConstraintLayout";     //$NON-NLS-1$
    public static final String CLASS_CONSTRAINT_LAYOUT_GUIDELINE = "android.support.constraint.Guideline";     //$NON-NLS-1$
    public static final String CONSTRAINT_LAYOUT_LIB_GROUP_ID = "com.android.support.constraint";
    public static final String CONSTRAINT_LAYOUT_LIB_ARTIFACT_ID = "constraint-layout";
    public static final String CONSTRAINT_LAYOUT_LIB_ARTIFACT = CONSTRAINT_LAYOUT_LIB_GROUP_ID + ":" + CONSTRAINT_LAYOUT_LIB_ARTIFACT_ID;
    /** Latest known version of the ConstraintLayout library (as a string) */
    public static final String LATEST_CONSTRAINT_LAYOUT_VERSION = "1.0.0-alpha7";

    /** Returns the appropriate name for the 'android' command, which is 'android.exe' for
     * Windows and 'android' for all other platforms. */
    public static String androidCmdName() {
        String os = System.getProperty("os.name");          //$NON-NLS-1$
        String cmd = "android";                             //$NON-NLS-1$
        if (os.startsWith("Windows")) {                     //$NON-NLS-1$
            cmd += ".bat";                                  //$NON-NLS-1$
        }
        return cmd;
    }

    /**
     * Returns the appropriate name for the 'mksdcard' command, which is 'mksdcard.exe' for Windows
     * and 'mksdcard' for all other platforms.
     */
    public static String mkSdCardCmdName() {
        String os = System.getProperty("os.name");          //$NON-NLS-1$
        String cmd = "mksdcard";                            //$NON-NLS-1$
        if (os.startsWith("Windows")) {                     //$NON-NLS-1$
            cmd += ".exe";                                  //$NON-NLS-1$
        }
        return cmd;
    }

    /**
     * Returns current platform
     *
     * @return one of {@link #PLATFORM_WINDOWS}, {@link #PLATFORM_DARWIN},
     * {@link #PLATFORM_LINUX} or {@link #PLATFORM_UNKNOWN}.
     */
    public static int currentPlatform() {
        String os = System.getProperty("os.name");          //$NON-NLS-1$
        if (os.startsWith("Mac OS")) {                      //$NON-NLS-1$
            return PLATFORM_DARWIN;
        } else if (os.startsWith("Windows")) {              //$NON-NLS-1$
            return PLATFORM_WINDOWS;
        } else if (os.startsWith("Linux")) {                //$NON-NLS-1$
            return PLATFORM_LINUX;
        }

        return PLATFORM_UNKNOWN;
    }

    /**
     * Returns current platform's UI name
     *
     * @return one of "Windows", "Mac OS X", "Linux" or "other".
     */
    public static String currentPlatformName() {
        String os = System.getProperty("os.name");          //$NON-NLS-1$
        if (os.startsWith("Mac OS")) {                      //$NON-NLS-1$
            return "Mac OS X";                              //$NON-NLS-1$
        } else if (os.startsWith("Windows")) {              //$NON-NLS-1$
            return "Windows";                               //$NON-NLS-1$
        } else if (os.startsWith("Linux")) {                //$NON-NLS-1$
            return "Linux";                                 //$NON-NLS-1$
        }

        return "Other";
    }

    private static String ext(String windowsExtension, String nonWindowsExtension) {
        if (CURRENT_PLATFORM == PLATFORM_WINDOWS) {
            return windowsExtension;
        } else {
            return nonWindowsExtension;
        }
    }

    /** Default anim resource folder name, i.e. "anim" */
    public static final String FD_RES_ANIM = "anim"; //$NON-NLS-1$
    /** Default animator resource folder name, i.e. "animator" */
    public static final String FD_RES_ANIMATOR = "animator"; //$NON-NLS-1$
    /** Default color resource folder name, i.e. "color" */
    public static final String FD_RES_COLOR = "color"; //$NON-NLS-1$
    /** Default drawable resource folder name, i.e. "drawable" */
    public static final String FD_RES_DRAWABLE = "drawable"; //$NON-NLS-1$
    /** Default interpolator resource folder name, i.e. "interpolator" */
    public static final String FD_RES_INTERPOLATOR = "interpolator"; //$NON-NLS-1$
    /** Default layout resource folder name, i.e. "layout" */
    public static final String FD_RES_LAYOUT = "layout"; //$NON-NLS-1$
    /** Default menu resource folder name, i.e. "menu" */
    public static final String FD_RES_MENU = "menu"; //$NON-NLS-1$
    /** Default menu resource folder name, i.e. "mipmap" */
    public static final String FD_RES_MIPMAP = "mipmap"; //$NON-NLS-1$
    /** Default values resource folder name, i.e. "values" */
    public static final String FD_RES_VALUES = "values"; //$NON-NLS-1$
    /** Default xml resource folder name, i.e. "xml" */
    public static final String FD_RES_XML = "xml"; //$NON-NLS-1$
    /** Default raw resource folder name, i.e. "raw" */
    public static final String FD_RES_RAW = "raw"; //$NON-NLS-1$
    /** Separator between the resource folder qualifier. */
    public static final String RES_QUALIFIER_SEP = "-"; //$NON-NLS-1$
    /** Namespace used in XML files for Android attributes */

    // ---- XML ----

    /** URI of the reserved "xmlns"  prefix */
    public static final String XMLNS_URI = "http://www.w3.org/2000/xmlns/";  //$NON-NLS-1$
    /** The "xmlns" attribute name */
    public static final String XMLNS = "xmlns";                              //$NON-NLS-1$
    /** The default prefix used for the {@link #XMLNS_URI} */
    public static final String XMLNS_PREFIX = "xmlns:";                      //$NON-NLS-1$
    /** Qualified name of the xmlns android declaration element */
    public static final String XMLNS_ANDROID = "xmlns:android";              //$NON-NLS-1$
    /** The default prefix used for the {@link #ANDROID_URI} name space */
    public static final String ANDROID_NS_NAME = "android";                  //$NON-NLS-1$
    /** The default prefix used for the {@link #ANDROID_URI} name space including the colon  */
    public static final String ANDROID_NS_NAME_PREFIX = "android:";          //$NON-NLS-1$
    public static final int ANDROID_NS_NAME_PREFIX_LEN = ANDROID_NS_NAME_PREFIX.length();

    /** The default prefix used for the app */
    public static final String APP_PREFIX = "app";                          //$NON-NLS-1$
    /** The entity for the ampersand character */
    public static final String AMP_ENTITY = "&amp;";                         //$NON-NLS-1$
    /** The entity for the quote character */
    public static final String QUOT_ENTITY = "&quot;";                       //$NON-NLS-1$
    /** The entity for the apostrophe character */
    public static final String APOS_ENTITY = "&apos;";                       //$NON-NLS-1$
    /** The entity for the less than character */
    public static final String LT_ENTITY = "&lt;";                           //$NON-NLS-1$
    /** The entity for the greater than character */
    public static final String GT_ENTITY = "&gt;";                           //$NON-NLS-1$

    // ---- Elements and Attributes ----

    /** Namespace prefix used for all resources */
    public static final String URI_PREFIX =
            "http://schemas.android.com/apk/res/";                     //$NON-NLS-1$
    /** Namespace used in XML files for Android attributes */
    public static final String ANDROID_URI =
            "http://schemas.android.com/apk/res/android";              //$NON-NLS-1$
    /** Namespace used in XML files for Android Tooling attributes */
    public static final String TOOLS_URI =
            "http://schemas.android.com/tools";                        //$NON-NLS-1$
    /** Namespace used for auto-adjusting namespaces */
    public static final String AUTO_URI =
            "http://schemas.android.com/apk/res-auto";                 //$NON-NLS-1$
    /** Namespace for xliff in string resources. */
    public static final String XLIFF_URI = "urn:oasis:names:tc:xliff:document:1.2";
    /** Default prefix used for tools attributes */
    public static final String TOOLS_PREFIX = "tools";                 //$NON-NLS-1$
    /** Default prefix used for xliff tags. */
    public static final String XLIFF_PREFIX = "xliff";                 //$NON-NLS-1$
    public static final String R_CLASS = "R";                          //$NON-NLS-1$
    public static final String ANDROID_PKG = "android";                //$NON-NLS-1$

    public static final String SHERPA_PREFIX = "app";                  //$NON-NLS-1$
    public static final String SHERPA_URI = "http://schemas.android.com/apk/res-auto";              //$NON-NLS-1$

    // Tags: Manifest
    public static final String TAG_SERVICE = "service";                //$NON-NLS-1$
    public static final String TAG_PERMISSION = "permission";          //$NON-NLS-1$
    public static final String TAG_USES_FEATURE = "uses-feature";      //$NON-NLS-1$
    public static final String TAG_USES_PERMISSION = "uses-permission";//$NON-NLS-1$
    public static final String TAG_USES_PERMISSION_SDK_23 = "uses-permission-sdk-23";//$NON-NLS-1$
    public static final String TAG_USES_PERMISSION_SDK_M = "uses-permission-sdk-m";//$NON-NLS-1$
    public static final String TAG_USES_LIBRARY = "uses-library";      //$NON-NLS-1$
    public static final String TAG_APPLICATION = "application";        //$NON-NLS-1$
    public static final String TAG_INTENT_FILTER = "intent-filter";    //$NON-NLS-1$
    public static final String TAG_USES_SDK = "uses-sdk";              //$NON-NLS-1$
    public static final String TAG_ACTIVITY = "activity";              //$NON-NLS-1$
    public static final String TAG_RECEIVER = "receiver";              //$NON-NLS-1$
    public static final String TAG_PROVIDER = "provider";              //$NON-NLS-1$
    public static final String TAG_GRANT_PERMISSION = "grant-uri-permission"; //$NON-NLS-1$
    public static final String TAG_PATH_PERMISSION = "path-permission"; //$NON-NLS-1$
    public static final String TAG_ACTION = "action";                   //$NON-NLS-1$

    // Tags: Resources
    public static final String TAG_RESOURCES = "resources";            //$NON-NLS-1$
    public static final String TAG_STRING = "string";                  //$NON-NLS-1$
    public static final String TAG_ARRAY = "array";                    //$NON-NLS-1$
    public static final String TAG_STYLE = "style";                    //$NON-NLS-1$
    public static final String TAG_ITEM = "item";                      //$NON-NLS-1$
    public static final String TAG_GROUP = "group";                    //$NON-NLS-1$
    public static final String TAG_STRING_ARRAY = "string-array";      //$NON-NLS-1$
    public static final String TAG_PLURALS = "plurals";                //$NON-NLS-1$
    public static final String TAG_INTEGER_ARRAY = "integer-array";    //$NON-NLS-1$
    public static final String TAG_COLOR = "color";                    //$NON-NLS-1$
    public static final String TAG_DIMEN = "dimen";                    //$NON-NLS-1$
    public static final String TAG_DRAWABLE = "drawable";              //$NON-NLS-1$
    public static final String TAG_MENU = "menu";                      //$NON-NLS-1$
    public static final String TAG_ENUM = "enum";                      //$NON-NLS-1$
    public static final String TAG_FLAG = "flag";                      //$NON-NLS-1$
    public static final String TAG_ATTR = "attr";                      //$NON-NLS-1$
    public static final String TAG_DECLARE_STYLEABLE = "declare-styleable"; //$NON-NLS-1$
    public static final String TAG_EAT_COMMENT = "eat-comment";        //$NON-NLS-1$
    public static final String TAG_SKIP = "skip";                      //$NON-NLS-1$
    public static final String TAG_SELECTOR = "selector";              //$NON-NLS-1$

    // Tags: XML
    public static final String TAG_HEADER = "header";                  //$NON-NLS-1$
    public static final String TAG_APPWIDGET_PROVIDER = "appwidget-provider"; //$NON-NLS-1$
    public static final String TAG_PREFERENCE_SCREEN = "PreferenceScreen"; //$NON-NLS-1$

    // Tags: Layouts
    public static final String VIEW_TAG = "view";                      //$NON-NLS-1$
    public static final String VIEW_INCLUDE = "include";               //$NON-NLS-1$
    public static final String VIEW_MERGE = "merge";                   //$NON-NLS-1$
    public static final String VIEW_FRAGMENT = "fragment";             //$NON-NLS-1$
    public static final String REQUEST_FOCUS = "requestFocus";         //$NON-NLS-1$
    public static final String TAG = "tag";                            //$NON-NLS-1$

    public static final String VIEW = "View";                          //$NON-NLS-1$
    public static final String VIEW_GROUP = "ViewGroup";               //$NON-NLS-1$
    public static final String FRAME_LAYOUT = "FrameLayout";           //$NON-NLS-1$
    public static final String LINEAR_LAYOUT = "LinearLayout";         //$NON-NLS-1$
    public static final String RELATIVE_LAYOUT = "RelativeLayout";     //$NON-NLS-1$
    public static final String GRID_LAYOUT = "GridLayout";             //$NON-NLS-1$
    public static final String SCROLL_VIEW = "ScrollView";             //$NON-NLS-1$
    public static final String BUTTON = "Button";                      //$NON-NLS-1$
    public static final String COMPOUND_BUTTON = "CompoundButton";     //$NON-NLS-1$
    public static final String ADAPTER_VIEW = "AdapterView";           //$NON-NLS-1$
    public static final String STACK_VIEW = "StackView";               //$NON-NLS-1$
    public static final String GALLERY = "Gallery";                    //$NON-NLS-1$
    public static final String GRID_VIEW = "GridView";                 //$NON-NLS-1$
    public static final String TAB_HOST = "TabHost";                   //$NON-NLS-1$
    public static final String RADIO_GROUP = "RadioGroup";             //$NON-NLS-1$
    public static final String RADIO_BUTTON = "RadioButton";           //$NON-NLS-1$
    public static final String SWITCH = "Switch";                      //$NON-NLS-1$
    public static final String EDIT_TEXT = "EditText";                 //$NON-NLS-1$
    public static final String LIST_VIEW = "ListView";                 //$NON-NLS-1$
    public static final String TEXT_VIEW = "TextView";                 //$NON-NLS-1$
    public static final String CHECKED_TEXT_VIEW = "CheckedTextView";  //$NON-NLS-1$
    public static final String IMAGE_VIEW = "ImageView";               //$NON-NLS-1$
    public static final String SURFACE_VIEW = "SurfaceView";           //$NON-NLS-1$
    public static final String ABSOLUTE_LAYOUT = "AbsoluteLayout";     //$NON-NLS-1$
    public static final String TABLE_LAYOUT = "TableLayout";           //$NON-NLS-1$
    public static final String TABLE_ROW = "TableRow";                 //$NON-NLS-1$
    public static final String TAB_WIDGET = "TabWidget";               //$NON-NLS-1$
    public static final String IMAGE_BUTTON = "ImageButton";           //$NON-NLS-1$
    public static final String ZOOM_BUTTON = "ZoomButton";             //$NON-NLS-1$
    public static final String SEEK_BAR = "SeekBar";                   //$NON-NLS-1$
    public static final String VIEW_STUB = "ViewStub";                 //$NON-NLS-1$
    public static final String SPINNER = "Spinner";                    //$NON-NLS-1$
    public static final String WEB_VIEW = "WebView";                   //$NON-NLS-1$
    public static final String TOGGLE_BUTTON = "ToggleButton";         //$NON-NLS-1$
    public static final String CHECK_BOX = "CheckBox";                 //$NON-NLS-1$
    public static final String ABS_LIST_VIEW = "AbsListView";          //$NON-NLS-1$
    public static final String PROGRESS_BAR = "ProgressBar";           //$NON-NLS-1$
    public static final String RATING_BAR = "RatingBar";               //$NON-NLS-1$
    public static final String ABS_SPINNER = "AbsSpinner";             //$NON-NLS-1$
    public static final String ABS_SEEK_BAR = "AbsSeekBar";            //$NON-NLS-1$
    public static final String VIEW_ANIMATOR = "ViewAnimator";         //$NON-NLS-1$
    public static final String VIEW_FLIPPER = "ViewFlipper";           //$NON-NLS-1$
    public static final String VIEW_SWITCHER = "ViewSwitcher";         //$NON-NLS-1$
    public static final String TEXT_SWITCHER = "TextSwitcher";         //$NON-NLS-1$
    public static final String IMAGE_SWITCHER = "ImageSwitcher";       //$NON-NLS-1$
    public static final String EXPANDABLE_LIST_VIEW = "ExpandableListView";    //$NON-NLS-1$
    public static final String HORIZONTAL_SCROLL_VIEW = "HorizontalScrollView"; //$NON-NLS-1$
    public static final String MULTI_AUTO_COMPLETE_TEXT_VIEW = "MultiAutoCompleteTextView"; //$NON-NLS-1$
    public static final String AUTO_COMPLETE_TEXT_VIEW = "AutoCompleteTextView"; //$NON-NLS-1$
    public static final String CHECKABLE = "Checkable";                //$NON-NLS-1$
    public static final String TEXTURE_VIEW = "TextureView";           //$NON-NLS-1$
    public static final String DIALER_FILTER = "DialerFilter";         //$NON-NLS-1$
    public static final String ADAPTER_VIEW_FLIPPER = "AdapterViewFlipper"; //$NON-NLS-1$
    public static final String ADAPTER_VIEW_ANIMATOR = "AdapterViewAnimator"; //$NON-NLS-1$
    public static final String VIDEO_VIEW = "VideoView"; //$NON-NLS-1$
    public static final String SEARCH_VIEW = "SearchView"; //$NON-NLS-1$

    /* Android Support Tag Constants */
    public static final String COORDINATOR_LAYOUT = CLASS_COORDINATOR_LAYOUT;
    public static final String APP_BAR_LAYOUT = CLASS_APP_BAR_LAYOUT;
    public static final String FLOATING_ACTION_BUTTON = CLASS_FLOATING_ACTION_BUTTON;
    public static final String COLLAPSING_TOOLBAR_LAYOUT = CLASS_COLLAPSING_TOOLBAR_LAYOUT;
    public static final String NAVIGATION_VIEW = CLASS_NAVIGATION_VIEW;
    public static final String SNACKBAR = CLASS_SNACKBAR;
    public static final String TAB_LAYOUT = CLASS_TAB_LAYOUT;
    public static final String TAB_ITEM = CLASS_TAB_ITEM;
    public static final String TEXT_INPUT_LAYOUT = CLASS_TEXT_INPUT_LAYOUT;
    public static final String NESTED_SCROLL_VIEW = CLASS_NESTED_SCROLL_VIEW;
    public static final String DRAWER_LAYOUT = CLASS_DRAWER_LAYOUT;
    public static final String VIEW_PAGER = CLASS_VIEW_PAGER;
    public static final String GRID_LAYOUT_V7 = CLASS_GRID_LAYOUT_V7;
    public static final String TOOLBAR_V7 = CLASS_TOOLBAR_V7;
    public static final String RECYCLER_VIEW = CLASS_RECYCLER_VIEW;
    public static final String CARD_VIEW = CLASS_CARD_VIEW;
    public static final String ACTION_MENU_VIEW = CLASS_ACTION_MENU_VIEW;
    public static final String AD_VIEW = CLASS_AD_VIEW;
    public static final String MAP_FRAGMENT = CLASS_MAP_FRAGMENT;
    public static final String MAP_VIEW = CLASS_MAP_VIEW;
    public static final String BROWSE_FRAGMENT = CLASS_BROWSE_FRAGMENT;
    public static final String DETAILS_FRAGMENT = CLASS_DETAILS_FRAGMENT;
    public static final String PLAYBACK_OVERLAY_FRAGMENT = CLASS_PLAYBACK_OVERLAY_FRAGMENT;
    public static final String SEARCH_FRAGMENT = CLASS_SEARCH_FRAGMENT;

    /* Android ConstraintLayout Tag Constants */
    public static final String CONSTRAINT_LAYOUT = CLASS_CONSTRAINT_LAYOUT;
    public static final String TABLE_CONSTRAINT_LAYOUT = CLASS_TABLE_CONSTRAINT_LAYOUT;
    public static final String CONSTRAINT_LAYOUT_GUIDELINE = CLASS_CONSTRAINT_LAYOUT_GUIDELINE;

    // Tags: Drawables
    public static final String TAG_BITMAP = "bitmap";                  //$NON-NLS-1$
    public static final String TAG_VECTOR = "vector";                  //$NON-NLS-1$

    // Tags: Data-Binding
    public static final String TAG_LAYOUT = "layout";                  //$NON-NLS-1$
    public static final String TAG_DATA = "data";                      //$NON-NLS-1$
    public static final String TAG_VARIABLE = "variable";              //$NON-NLS-1$
    public static final String TAG_IMPORT = "import";                  //$NON-NLS-1$

    // Attributes: Manifest
    public static final String ATTR_EXPORTED = "exported";             //$NON-NLS-1$
    public static final String ATTR_PERMISSION = "permission";         //$NON-NLS-1$
    public static final String ATTR_MIN_SDK_VERSION = "minSdkVersion"; //$NON-NLS-1$
    public static final String ATTR_TARGET_SDK_VERSION = "targetSdkVersion"; //$NON-NLS-1$
    public static final String ATTR_ICON = "icon";                     //$NON-NLS-1$
    public static final String ATTR_PACKAGE = "package";               //$NON-NLS-1$
    public static final String ATTR_CORE_APP = "coreApp";              //$NON-NLS-1$
    public static final String ATTR_THEME = "theme";                   //$NON-NLS-1$
    public static final String ATTR_SCHEME = "scheme";                 //$NON_NLS-1$
    public static final String ATTR_HOST = "host";                     //$NON_NLS-1$
    public static final String ATTR_PATH = "path";                     //$NON-NLS-1$
    public static final String ATTR_PATH_PREFIX = "pathPrefix";        //$NON-NLS-1$
    public static final String ATTR_PATH_PATTERN = "pathPattern";      //$NON-NLS-1$
    public static final String ATTR_ALLOW_BACKUP = "allowBackup";      //$NON_NLS-1$
    public static final String ATTR_DEBUGGABLE = "debuggable";         //$NON-NLS-1$
    public static final String ATTR_READ_PERMISSION = "readPermission"; //$NON_NLS-1$
    public static final String ATTR_WRITE_PERMISSION = "writePermission"; //$NON_NLS-1$
    public static final String ATTR_VERSION_CODE = "versionCode";      //$NON_NLS-1$
    public static final String ATTR_VERSION_NAME = "versionName";      //$NON_NLS-1$
    public static final String ATTR_FULL_BACKUP_CONTENT = "fullBackupContent"; //$NON_NLS-1$

    // Attributes: Resources
    public static final String ATTR_NAME = "name";                     //$NON-NLS-1$
    public static final String ATTR_FRAGMENT = "fragment";             //$NON-NLS-1$
    public static final String ATTR_TYPE = "type";                     //$NON-NLS-1$
    public static final String ATTR_PARENT = "parent";                 //$NON-NLS-1$
    public static final String ATTR_TRANSLATABLE = "translatable";     //$NON-NLS-1$
    public static final String ATTR_COLOR = "color";                   //$NON-NLS-1$
    public static final String ATTR_DRAWABLE = "drawable";             //$NON-NLS-1$
    public static final String ATTR_VALUE = "value";                   //$NON-NLS-1$
    public static final String ATTR_QUANTITY = "quantity";             //$NON-NLS-1$
    public static final String ATTR_FORMAT = "format";                 //$NON-NLS-1$
    public static final String ATTR_PREPROCESSING = "preprocessing";   //$NON-NLS-1$

    // Attributes: Data-Binding
    public static final String ATTR_ALIAS = "alias";                  //$NON-NLS-1$

    // Attributes: Layout
    public static final String ATTR_LAYOUT_RESOURCE_PREFIX = "layout_";//$NON-NLS-1$
    public static final String ATTR_CLASS = "class";                   //$NON-NLS-1$
    public static final String ATTR_STYLE = "style";                   //$NON-NLS-1$
    public static final String ATTR_CONTEXT = "context";               //$NON-NLS-1$
    public static final String ATTR_ID = "id";                         //$NON-NLS-1$
    public static final String ATTR_TEXT = "text";                     //$NON-NLS-1$
    public static final String ATTR_TEXT_SIZE = "textSize";            //$NON-NLS-1$
    public static final String ATTR_LABEL = "label";                   //$NON-NLS-1$
    public static final String ATTR_HINT = "hint";                     //$NON-NLS-1$
    public static final String ATTR_PROMPT = "prompt";                 //$NON-NLS-1$
    public static final String ATTR_ON_CLICK = "onClick";              //$NON-NLS-1$
    public static final String ATTR_INPUT_TYPE = "inputType";          //$NON-NLS-1$
    public static final String ATTR_INPUT_METHOD = "inputMethod";      //$NON-NLS-1$
    public static final String ATTR_LAYOUT_GRAVITY = "layout_gravity"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_WIDTH = "layout_width";     //$NON-NLS-1$
    public static final String ATTR_LAYOUT_HEIGHT = "layout_height";   //$NON-NLS-1$
    public static final String ATTR_LAYOUT_WEIGHT = "layout_weight";   //$NON-NLS-1$
    public static final String ATTR_PADDING = "padding";               //$NON-NLS-1$
    public static final String ATTR_PADDING_BOTTOM = "paddingBottom";  //$NON-NLS-1$
    public static final String ATTR_PADDING_TOP = "paddingTop";        //$NON-NLS-1$
    public static final String ATTR_PADDING_RIGHT = "paddingRight";    //$NON-NLS-1$
    public static final String ATTR_PADDING_LEFT = "paddingLeft";      //$NON-NLS-1$
    public static final String ATTR_PADDING_START = "paddingStart";    //$NON-NLS-1$
    public static final String ATTR_PADDING_END = "paddingEnd";        //$NON-NLS-1$
    public static final String ATTR_FOREGROUND = "foreground";         //$NON-NLS-1$
    public static final String ATTR_BACKGROUND = "background";         //$NON-NLS-1$
    public static final String ATTR_ORIENTATION = "orientation";       //$NON-NLS-1$
    public static final String ATTR_LAYOUT = "layout";                 //$NON-NLS-1$
    public static final String ATTR_ROW_COUNT = "rowCount";            //$NON-NLS-1$
    public static final String ATTR_COLUMN_COUNT = "columnCount";      //$NON-NLS-1$
    public static final String ATTR_LABEL_FOR = "labelFor";            //$NON-NLS-1$
    public static final String ATTR_BASELINE_ALIGNED = "baselineAligned";       //$NON-NLS-1$
    public static final String ATTR_CONTENT_DESCRIPTION = "contentDescription"; //$NON-NLS-1$
    public static final String ATTR_IME_ACTION_LABEL = "imeActionLabel";        //$NON-NLS-1$
    public static final String ATTR_PRIVATE_IME_OPTIONS = "privateImeOptions";  //$NON-NLS-1$
    public static final String VALUE_NONE = "none";                    //$NON-NLS-1$
    public static final String VALUE_NO = "no";                        //$NON-NLS-1$
    public static final String ATTR_NUMERIC = "numeric";               //$NON-NLS-1$
    public static final String ATTR_IME_ACTION_ID = "imeActionId";     //$NON-NLS-1$
    public static final String ATTR_IME_OPTIONS = "imeOptions";        //$NON-NLS-1$
    public static final String ATTR_FREEZES_TEXT = "freezesText";      //$NON-NLS-1$
    public static final String ATTR_EDITOR_EXTRAS = "editorExtras";    //$NON-NLS-1$
    public static final String ATTR_EDITABLE = "editable";             //$NON-NLS-1$
    public static final String ATTR_DIGITS = "digits";                 //$NON-NLS-1$
    public static final String ATTR_CURSOR_VISIBLE = "cursorVisible";  //$NON-NLS-1$
    public static final String ATTR_CAPITALIZE = "capitalize";         //$NON-NLS-1$
    public static final String ATTR_PHONE_NUMBER = "phoneNumber";      //$NON-NLS-1$
    public static final String ATTR_PASSWORD = "password";             //$NON-NLS-1$
    public static final String ATTR_BUFFER_TYPE = "bufferType";        //$NON-NLS-1$
    public static final String ATTR_AUTO_TEXT = "autoText";            //$NON-NLS-1$
    public static final String ATTR_ENABLED = "enabled";               //$NON-NLS-1$
    public static final String ATTR_SINGLE_LINE = "singleLine";        //$NON-NLS-1$
    public static final String ATTR_SELECT_ALL_ON_FOCUS = "selectAllOnFocus";  //$NON-NLS-1$
    public static final String ATTR_SCALE_TYPE = "scaleType";          //$NON-NLS-1$
    public static final String ATTR_VISIBILITY = "visibility";         //$NON-NLS-1$
    public static final String ATTR_TEXT_IS_SELECTABLE =
            "textIsSelectable";                                        //$NON-NLS-1$
    public static final String ATTR_IMPORTANT_FOR_ACCESSIBILITY =
            "importantForAccessibility";                               //$NON-NLS-1$
    public static final String ATTR_ACCESSIBILITY_TRAVERSAL_BEFORE =
            "accessibilityTraversalBefore";                            //$NON-NLS-1$
    public static final String ATTR_ACCESSIBILITY_TRAVERSAL_AFTER =
            "accessibilityTraversalAfter";                             //$NON-NLS-1$
    public static final String ATTR_LIST_PREFERRED_ITEM_PADDING_LEFT =
            "listPreferredItemPaddingLeft";                            //$NON-NLS-1$
    public static final String ATTR_LIST_PREFERRED_ITEM_PADDING_RIGHT =
            "listPreferredItemPaddingRight";                           //$NON-NLS-1$
    public static final String ATTR_LIST_PREFERRED_ITEM_PADDING_START =
            "listPreferredItemPaddingStart";                           //$NON-NLS-1$
    public static final String ATTR_LIST_PREFERRED_ITEM_PADDING_END =
            "listPreferredItemPaddingEnd";                             //$NON-NLS-1$
    public static final String ATTR_INDEX = "index";                   //$NON-NLS-1$
    public static final String ATTR_ACTION_BAR_NAV_MODE = "actionBarNavMode"; //$NON-NLS-1$
    public static final String ATTR_MENU = "menu";                     //$NON-NLS-1$
    public static final String ATTR_OPEN_DRAWER = "openDrawer";        //$NON-NLS-1$
    public static final String ATTR_SHOW_IN = "showIn";                //$NON-NLS-1$
    public static final String ATTR_PARENT_TAG = "parentTag";          //$NON-NLS-1$
    public static final String ATTR_WIDTH = "width";                   //$NON-NLS-1$
    public static final String ATTR_HEIGHT = "height";                 //$NON-NLS-1$

    // TextView attributes
    public static final String ATTR_TEXT_APPEARANCE = "textAppearance"; //$NON-NLS-1$
    public static final String ATTR_FONT_FAMILY = "fontFamily";        //$NON-NLS-1$
    public static final String ATTR_TYPEFACE = "typeface";             //$NON-NLS-1$
    public static final String ATTR_LINE_SPACING_EXTRA = "lineSpacingExtra"; //$NON-NLS-1$
    public static final String ATTR_TEXT_STYLE = "textStyle";          //$NON-NLS-1$
    public static final String ATTR_TEXT_ALIGNMENT = "textAlignment";  //$NON-NLS-1$
    public static final String ATTR_TEXT_COLOR = "textColor";          //$NON-NLS-1$
    public static final String ATTR_TEXT_COLOR_HINT = "textColorHint"; //$NON-NLS-1$
    public static final String ATTR_TEXT_ALL_CAPS = "textAllCaps";     //$NON-NLS-1$

    // Tools attributes for AdapterView inheritors
    public static final String ATTR_LISTFOOTER = "listfooter";         //$NON-NLS-1$
    public static final String ATTR_LISTHEADER = "listheader";         //$NON-NLS-1$
    public static final String ATTR_LISTITEM = "listitem";             //$NON-NLS-1$

    // Tools attributes for scrolling
    public static final String ATTR_SCROLLX = "scrollX";               //$NON-NLS-1$
    public static final String ATTR_SCROLLY = "scrollY";               //$NON-NLS-1$

    // AbsoluteLayout layout params
    public static final String ATTR_LAYOUT_Y = "layout_y";             //$NON-NLS-1$
    public static final String ATTR_LAYOUT_X = "layout_x";             //$NON-NLS-1$

    // GridLayout layout params
    public static final String ATTR_LAYOUT_ROW = "layout_row";         //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ROW_SPAN = "layout_rowSpan";//$NON-NLS-1$
    public static final String ATTR_LAYOUT_COLUMN = "layout_column";   //$NON-NLS-1$
    public static final String ATTR_LAYOUT_COLUMN_SPAN = "layout_columnSpan";       //$NON-NLS-1$

    // ProgressBar/RatingBar attributes
    public static final String ATTR_MAXIMUM = "max";                                    //$NON-NLS-1$
    public static final String ATTR_PROGRESS = "progress";                              //$NON-NLS-1$
    public static final String ATTR_PROGRESS_DRAWABLE = "progressDrawable";             //$NON-NLS-1$
    public static final String ATTR_PROGRESS_TINT = "progressTint";                     //$NON-NLS-1$
    public static final String ATTR_INDETERMINATE = "indeterminate";                    //$NON-NLS-1$
    public static final String ATTR_INDETERMINATE_DRAWABLE = "indeterminateDrawable";   //$NON-NLS-1$
    public static final String ATTR_INDETERMINATE_TINT = "indeterminateTint";           //$NON-NLS-1$
    public static final String ATTR_RATING = "rating";                                  //$NON-NLS-1$
    public static final String ATTR_NUM_STARS = "numStars";                             //$NON-NLS-1$
    public static final String ATTR_STEP_SIZE = "stepSize";                             //$NON-NLS-1$
    public static final String ATTR_IS_INDICATOR = "isIndicator";                       //$NON-NLS-1$
    public static final String ATTR_THUMB = "thumb";                                    //$NON-NLS-1$

    // ImageView attributes
    public static final String ATTR_ADJUST_VIEW_BOUNDS = "adjustViewBounds"; //$NON-NLS-1$
    public static final String ATTR_CROP_TO_PADDING = "cropToPadding"; //$NON-NLS-1$

    // ConstraintLayout layout params
    public static final String ATTR_LAYOUT_EDITOR_ABSOLUTE_X = "layout_editor_absoluteX"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_EDITOR_ABSOLUTE_Y = "layout_editor_absoluteY"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_LEFT_CREATOR = "layout_constraintLeft_creator"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_RIGHT_CREATOR = "layout_constraintRight_creator"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_TOP_CREATOR = "layout_constraintTop_creator"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_BOTTOM_CREATOR = "layout_constraintBottom_creator"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_BASELINE_CREATOR = "layout_constraintBaseline_creator"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_CENTER_CREATOR = "layout_constraintCenter_creator"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_CENTER_X_CREATOR = "layout_constraintCenterX_creator"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_CENTER_Y_CREATOR = "layout_constraintCenterY_creator"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_LEFT_TO_LEFT_OF = "layout_constraintLeft_toLeftOf";         //$NON-NLS-1$
    public static final String ATTR_LAYOUT_LEFT_TO_RIGHT_OF = "layout_constraintLeft_toRightOf";       //$NON-NLS-1$
    public static final String ATTR_LAYOUT_RIGHT_TO_LEFT_OF = "layout_constraintRight_toLeftOf";       //$NON-NLS-1$
    public static final String ATTR_LAYOUT_RIGHT_TO_RIGHT_OF = "layout_constraintRight_toRightOf";     //$NON-NLS-1$
    public static final String ATTR_LAYOUT_TOP_TO_TOP_OF = "layout_constraintTop_toTopOf";             //$NON-NLS-1$
    public static final String ATTR_LAYOUT_TOP_TO_BOTTOM_OF = "layout_constraintTop_toBottomOf";       //$NON-NLS-1$
    public static final String ATTR_LAYOUT_BOTTOM_TO_TOP_OF = "layout_constraintBottom_toTopOf";       //$NON-NLS-1$
    public static final String ATTR_LAYOUT_BOTTOM_TO_BOTTOM_OF = "layout_constraintBottom_toBottomOf"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_BASELINE_TO_BASELINE_OF = "layout_constraintBaseline_toBaselineOf"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_HORIZONTAL_BIAS = "layout_constraintHorizontal_bias"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_VERTICAL_BIAS = "layout_constraintVertical_bias"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_DIMENSION_RATIO = "layout_constraintDimensionRatio"; //$NON-NLS-1$
    public static final String ATTR_GUIDELINE_ORIENTATION_HORIZONTAL = "horizontal"; //$NON-NLS-1$
    public static final String ATTR_GUIDELINE_ORIENTATION_VERTICAL = "vertical"; //$NON-NLS-1$
    public static final String LAYOUT_CONSTRAINT_GUIDE_BEGIN = "layout_constraintGuide_begin"; //$NON-NLS-1$
    public static final String LAYOUT_CONSTRAINT_GUIDE_END = "layout_constraintGuide_end"; //$NON-NLS-1$
    public static final String LAYOUT_CONSTRAINT_GUIDE_PERCENT = "layout_constraintGuide_percent"; //$NON-NLS-1$
    public static final String LAYOUT_CONSTRAINT_DEPRECATED_GUIDE_PERCENT = "layout_constraintGuide_Percent"; //$NON-NLS-1$

    // TableRow
    public static final String ATTR_LAYOUT_SPAN = "layout_span";       //$NON-NLS-1$

    // RelativeLayout layout params:
    public static final String ATTR_LAYOUT_ALIGN_LEFT = "layout_alignLeft";        //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_RIGHT = "layout_alignRight";      //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_START = "layout_alignStart";      //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_END = "layout_alignEnd";          //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_TOP = "layout_alignTop";          //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_BOTTOM = "layout_alignBottom";    //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_PARENT_LEFT = "layout_alignParentLeft";        //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_PARENT_RIGHT = "layout_alignParentRight";      //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_PARENT_START = "layout_alignParentStart";      //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_PARENT_END = "layout_alignParentEnd";          //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_PARENT_TOP = "layout_alignParentTop";          //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_PARENT_BOTTOM = "layout_alignParentBottom";    //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_WITH_PARENT_MISSING = "layout_alignWithParentIfMissing";   //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ALIGN_BASELINE = "layout_alignBaseline";             //$NON-NLS-1$
    public static final String ATTR_LAYOUT_CENTER_IN_PARENT = "layout_centerInParent";          //$NON-NLS-1$
    public static final String ATTR_LAYOUT_CENTER_VERTICAL = "layout_centerVertical";           //$NON-NLS-1$
    public static final String ATTR_LAYOUT_CENTER_HORIZONTAL = "layout_centerHorizontal";       //$NON-NLS-1$
    public static final String ATTR_LAYOUT_TO_RIGHT_OF = "layout_toRightOf";        //$NON-NLS-1$
    public static final String ATTR_LAYOUT_TO_LEFT_OF = "layout_toLeftOf";          //$NON-NLS-1$
    public static final String ATTR_LAYOUT_TO_START_OF = "layout_toStartOf";        //$NON-NLS-1$
    public static final String ATTR_LAYOUT_TO_END_OF = "layout_toEndOf";            //$NON-NLS-1$
    public static final String ATTR_LAYOUT_BELOW = "layout_below";                  //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ABOVE = "layout_above";                  //$NON-NLS-1$

    // Margins
    public static final String ATTR_LAYOUT_MARGIN = "layout_margin";               //$NON-NLS-1$
    public static final String ATTR_LAYOUT_MARGIN_LEFT = "layout_marginLeft";      //$NON-NLS-1$
    public static final String ATTR_LAYOUT_MARGIN_RIGHT = "layout_marginRight";    //$NON-NLS-1$
    public static final String ATTR_LAYOUT_MARGIN_START = "layout_marginStart";    //$NON-NLS-1$
    public static final String ATTR_LAYOUT_MARGIN_END = "layout_marginEnd";        //$NON-NLS-1$
    public static final String ATTR_LAYOUT_MARGIN_TOP = "layout_marginTop";        //$NON-NLS-1$
    public static final String ATTR_LAYOUT_MARGIN_BOTTOM = "layout_marginBottom";  //$NON-NLS-1$

    // Attributes: Drawables
    public static final String ATTR_TILE_MODE = "tileMode";            //$NON-NLS-1$

    // Attributes: Design and support lib
    public static final String ATTR_LAYOUT_ANCHOR = "layout_anchor";                //$NON-NLS-1$
    public static final String ATTR_LAYOUT_ANCHOR_GRAVITY = "layout_anchorGravity"; //$NON-NLS-1$
    public static final String ATTR_LAYOUT_BEHAVIOR = "layout_behavior";            //$NON-NLS-1$
    public static final String ATTR_LAYOUT_KEYLINE = "layout_keyline";              //$NON-NLS-1$
    public static final String ATTR_BACKGROUND_TINT = "backgroundTint";             //$NON-NLS-1$
    public static final String ATTR_RIPPLE_COLOR = "rippleColor";                   //$NON-NLS-1$
    public static final String ATTR_TINT = "tint";                                  //$NON-NLS-1$
    public static final String ATTR_FAB_SIZE = "fabSize";                           //$NON-NLS-1$
    public static final String ATTR_ELEVATION = "elevation";                        //$NON-NLS-1$
    public static final String ATTR_FITS_SYSTEM_WINDOWS = "fitsSystemWindows";      //$NON-NLS-1$
    public static final String ATTR_EXPANDED = "expanded";                          //$NON-NLS-1$
    public static final String ATTR_LAYOUT_SCROLL_FLAGS = "layout_scrollFlags";     //$NON-NLS-1$
    public static final String ATTR_LAYOUT_COLLAPSE_MODE = "layout_collapseMode";   //$NON-NLS-1$
    public static final String ATTR_COLLAPSE_PARALLAX_MULTIPLIER = "layout_collapseParallaxMultiplier"; //$NON-NLS-1$
    public static final String ATTR_SCROLLBAR_STYLE = "scrollbarStyle";             //$NON-NLS-1$
    public static final String ATTR_FILL_VIEWPORT = "fillViewport";                 //$NON-NLS-1$
    public static final String ATTR_CLIP_TO_PADDING = "clipToPadding";              //$NON-NLS-1$
    public static final String ATTR_CLIP_CHILDREN = "clipChildren";                 //$NON-NLS-1$
    public static final String ATTR_HEADER_LAYOUT = "headerLayout";                 //$NON-NLS-1$
    public static final String ATTR_ITEM_BACKGROUND = "itemBackground";             //$NON-NLS-1$
    public static final String ATTR_ITEM_ICON_TINT = "itemIconTint";                //$NON-NLS-1$
    public static final String ATTR_ITEM_TEXT_APPEARANCE = "itemTextAppearance";    //$NON-NLS-1$
    public static final String ATTR_ITEM_TEXT_COLOR = "itemTextColor";              //$NON-NLS-1$
    public static final String ATTR_POPUP_THEME = "popupTheme";                     //$NON-NLS-1$
    public static final String ATTR_MIN_HEIGHT = "minHeight";                       //$NON-NLS-1$
    public static final String ATTR_ACTION_BAR = "actionBar";                       //$NON-NLS-1$
    public static final String ATTR_CONTENT_SCRIM = "contentScrim";                 //$NON-NLS-1$
    public static final String ATTR_TOOLBAR_ID = "toolbarId";                       //$NON-NLS-1$
    public static final String ATTR_CACHE_COLOR_HINT = "cacheColorHint";            //$NON-NLS-1$
    public static final String ATTR_DIVIDER = "divider";                            //$NON-NLS-1$
    public static final String ATTR_DIVIDER_PADDING = "dividerPadding";             //$NON-NLS-1$
    public static final String ATTR_DIVIDER_HEIGHT = "dividerHeight";               //$NON-NLS-1$
    public static final String ATTR_FOOTER_DIVIDERS_ENABLED = "footerDividersEnabled";//$NON-NLS-1$
    public static final String ATTR_HEADER_DIVIDERS_ENABLED = "headerDividersEnabled";//$NON-NLS-1$
    public static final String ATTR_CARD_BACKGROUND_COLOR = "cardBackgroundColor";  //$NON-NLS-1$
    public static final String ATTR_CARD_CORNER_RADIUS = "cardCornerRadius";        //$NON-NLS-1$
    public static final String ATTR_CONTENT_PADDING = "contentPadding";             //$NON-NLS-1$
    public static final String ATTR_CARD_ELEVATION = "cardElevation";               //$NON-NLS-1$
    public static final String ATTR_CARD_PREVENT_CORNER_OVERLAP = "cardPreventCornerOverlap";//$NON-NLS-1$
    public static final String ATTR_CARD_USE_COMPAT_PADDING = "cardUseCompatPadding";        //$NON-NLS-1$
    public static final String ATTR_SPINNER_MODE = "spinnerMode";                   //$NON-NLS-1$
    public static final String ATTR_ENTRIES = "entries";                            //$NON-NLS-1$
    public static final String ATTR_POPUP_BACKGROUND = "popupBackground";           //$NON-NLS-1$
    public static final String ATTR_MIN_WIDTH = "minWidth";                         //$NON-NLS-1$
    public static final String ATTR_DROPDOWN_HEIGHT = "dropDownHeight";             //$NON-NLS-1$
    public static final String ATTR_DROPDOWN_WIDTH = "dropDownWidth";               //$NON-NLS-1$
    public static final String ATTR_DRAW_SELECTOR_ON_TOP = "drawSelectorOnTop";     //$NON-NLS-1$
    public static final String ATTR_SCROLLBARS = "scrollbars";                      //$NON-NLS-1$
    public static final String ATTR_COMPLETION_HINT = "completionHint";             //$NON-NLS-1$
    public static final String ATTR_TAB_MODE = "tabMode";                           //$NON-NLS-1$
    public static final String ATTR_TAB_GRAVITY = "tabGravity";                     //$NON-NLS-1$
    public static final String ATTR_TAB_CONTENT_START = "tabContentStart";          //$NON-NLS-1$
    public static final String ATTR_TAB_INDICATOR_COLOR = "tabIndicatorColor";      //$NON-NLS-1$
    public static final String ATTR_TAB_SELECTED_TEXT_COLOR = "tabSelectedTextColor";//$NON-NLS-1$
    public static final String ATTR_TAB_TEXT_APPEARANCE = "tabTextAppearance";      //$NON-NLS-1$

    // Values: Manifest
    public static final String VALUE_SPLIT_ACTION_BAR_WHEN_NARROW = "splitActionBarWhenNarrow"; // NON-NLS-$1

    // Values: Layouts
    public static final String VALUE_FILL_PARENT = "fill_parent";       //$NON-NLS-1$
    public static final String VALUE_MATCH_PARENT = "match_parent";     //$NON-NLS-1$
    public static final String VALUE_VERTICAL = "vertical";             //$NON-NLS-1$
    public static final String VALUE_TRUE = "true";                     //$NON-NLS-1$
    public static final String VALUE_EDITABLE = "editable";             //$NON-NLS-1$
    public static final String VALUE_AUTO_FIT = "auto_fit";             //$NON-NLS-1$
    public static final String VALUE_SELECTABLE_ITEM_BACKGROUND =
            "?android:attr/selectableItemBackground";                   //$NON-NLS-1$

    // Values: Resources
    public static final String VALUE_ID = "id";                        //$NON-NLS-1$

    // Values: Drawables
    public static final String VALUE_DISABLED = "disabled";            //$NON-NLS-1$
    public static final String VALUE_CLAMP = "clamp";                  //$NON-NLS-1$

    // Menus
    public static final String ATTR_CHECKABLE = "checkable";
    public static final String ATTR_CHECKABLE_BEHAVIOR = "checkableBehavior";
    public static final String ATTR_ORDER_IN_CATEGORY = "orderInCategory";
    public static final String ATTR_SHOW_AS_ACTION = "showAsAction";
    public static final String ATTR_TITLE = "title";
    public static final String ATTR_VISIBLE = "visible";
    public static final String VALUE_IF_ROOM = "ifRoom";               //$NON-NLS-1$
    public static final String VALUE_ALWAYS = "always";                //$NON-NLS-1$

    // Units
    public static final String UNIT_DP = "dp";                         //$NON-NLS-1$
    public static final String UNIT_DIP = "dip";                       //$NON-NLS-1$
    public static final String UNIT_SP = "sp";                         //$NON-NLS-1$
    public static final String UNIT_PX = "px";                         //$NON-NLS-1$
    public static final String UNIT_IN = "in";                         //$NON-NLS-1$
    public static final String UNIT_MM = "mm";                         //$NON-NLS-1$
    public static final String UNIT_PT = "pt";                         //$NON-NLS-1$

    // Filenames and folder names
    public static final String ANDROID_MANIFEST_XML = "AndroidManifest.xml"; //$NON-NLS-1$
    public static final String OLD_PROGUARD_FILE = "proguard.cfg";     //$NON-NLS-1$
    public static final String CLASS_FOLDER =
            "bin" + File.separator + "classes";                        //$NON-NLS-1$ //$NON-NLS-2$
    public static final String GEN_FOLDER = "gen";                     //$NON-NLS-1$
    public static final String SRC_FOLDER = "src";                     //$NON-NLS-1$
    public static final String LIBS_FOLDER = "libs";                   //$NON-NLS-1$
    public static final String BIN_FOLDER = "bin";                     //$NON-NLS-1$

    public static final String RES_FOLDER = "res";                     //$NON-NLS-1$
    public static final String DOT_XML = ".xml";                       //$NON-NLS-1$
    public static final String DOT_XSD = ".xsd";                       //$NON-NLS-1$
    public static final String DOT_GIF = ".gif";                       //$NON-NLS-1$
    public static final String DOT_JPG = ".jpg";                       //$NON-NLS-1$
    public static final String DOT_JPEG = ".jpeg";                     //$NON-NLS-1$
    public static final String DOT_WEBP = ".webp";                     //$NON-NLS-1$
    public static final String DOT_PNG = ".png";                       //$NON-NLS-1$
    public static final String DOT_9PNG = ".9.png";                    //$NON-NLS-1$
    public static final String DOT_JAVA = ".java";                     //$NON-NLS-1$
    public static final String DOT_CLASS = ".class";                   //$NON-NLS-1$
    public static final String DOT_JAR = ".jar";                       //$NON-NLS-1$
    public static final String DOT_GRADLE = ".gradle";                 //$NON-NLS-1$
    public static final String DOT_PROPERTIES = ".properties";         //$NON-NLS-1$
    public static final String DOT_JSON = ".json";                     //$NON-NLS-1$
    public static final String DOT_PSD = ".psd";                       //$NON-NLS-1$

    /** Extension of the Application package Files, i.e. "apk". */
    public static final String EXT_ANDROID_PACKAGE = "apk"; //$NON-NLS-1$
    /** Extension of java files, i.e. "java" */
    public static final String EXT_JAVA = "java"; //$NON-NLS-1$
    /** Extension of compiled java files, i.e. "class" */
    public static final String EXT_CLASS = "class"; //$NON-NLS-1$
    /** Extension of xml files, i.e. "xml" */
    public static final String EXT_XML = "xml"; //$NON-NLS-1$
    /** Extension of gradle files, i.e. "gradle" */
    public static final String EXT_GRADLE = "gradle"; //$NON-NLS-1$
    /** Extension of jar files, i.e. "jar" */
    public static final String EXT_JAR = "jar"; //$NON-NLS-1$
    /** Extension of ZIP files, i.e. "zip" */
    public static final String EXT_ZIP = "zip"; //$NON-NLS-1$
    /** Extension of aidl files, i.e. "aidl" */
    public static final String EXT_AIDL = "aidl"; //$NON-NLS-1$
    /** Extension of Renderscript files, i.e. "rs" */
    public static final String EXT_RS = "rs"; //$NON-NLS-1$
    /** Extension of Renderscript files, i.e. "rsh" */
    public static final String EXT_RSH = "rsh"; //$NON-NLS-1$
    /** Extension of FilterScript files, i.e. "fs" */
    public static final String EXT_FS = "fs"; //$NON-NLS-1$
    /** Extension of Renderscript bitcode files, i.e. "bc" */
    public static final String EXT_BC = "bc"; //$NON-NLS-1$
    /** Extension of dependency files, i.e. "d" */
    public static final String EXT_DEP = "d"; //$NON-NLS-1$
    /** Extension of native libraries, i.e. "so" */
    public static final String EXT_NATIVE_LIB = "so"; //$NON-NLS-1$
    /** Extension of dex files, i.e. "dex" */
    public static final String EXT_DEX = "dex"; //$NON-NLS-1$
    /** Extension for temporary resource files, ie "ap_ */
    public static final String EXT_RES = "ap_"; //$NON-NLS-1$
    /** Extension for pre-processable images. Right now pngs */
    public static final String EXT_PNG = "png"; //$NON-NLS-1$
    /** Extension for Android archive files */
    public static final String EXT_AAR = "aar"; //$NON-NLS-1$
    /** Extension for Java heap dumps. */
    public static final String EXT_HPROF = "hprof"; //$NON-NLS-1$
    public static final String EXT_GZ = "gz"; //$NON-NLS-1$

    private static final String DOT = "."; //$NON-NLS-1$

    /** Dot-Extension of the Application package Files, i.e. ".apk". */
    public static final String DOT_ANDROID_PACKAGE = DOT + EXT_ANDROID_PACKAGE;
    /** Dot-Extension of aidl files, i.e. ".aidl" */
    public static final String DOT_AIDL = DOT + EXT_AIDL;
    /** Dot-Extension of renderscript files, i.e. ".rs" */
    public static final String DOT_RS = DOT + EXT_RS;
    /** Dot-Extension of renderscript header files, i.e. ".rsh" */
    public static final String DOT_RSH = DOT + EXT_RSH;
    /** Dot-Extension of FilterScript files, i.e. ".fs" */
    public static final String DOT_FS = DOT + EXT_FS;
    /** Dot-Extension of renderscript bitcode files, i.e. ".bc" */
    public static final String DOT_BC = DOT + EXT_BC;
    /** Dot-Extension of dependency files, i.e. ".d" */
    public static final String DOT_DEP = DOT + EXT_DEP;
    /** Dot-Extension of native dynamic libraries, i.e. ".so" */
    public static final String DOT_NATIVE_LIBS = DOT + EXT_NATIVE_LIB;
    /** Dot-Extension of dex files, i.e. ".dex" */
    public static final String DOT_DEX = DOT + EXT_DEX;
    /** Dot-Extension for temporary resource files, ie "ap_ */
    public static final String DOT_RES = DOT + EXT_RES;
    /** Dot-Extension for BMP files, i.e. ".bmp" */
    public static final String DOT_BMP = ".bmp"; //$NON-NLS-1$
    /** Dot-Extension for SVG files, i.e. ".svg" */
    public static final String DOT_SVG = ".svg"; //$NON-NLS-1$
    /** Dot-Extension for template files */
    public static final String DOT_FTL = ".ftl"; //$NON-NLS-1$
    /** Dot-Extension of text files, i.e. ".txt" */
    public static final String DOT_TXT = ".txt"; //$NON-NLS-1$
    /** Dot-Extension for Android archive files */
    public static final String DOT_AAR = DOT + EXT_AAR; //$NON-NLS-1$
    /** Dot-Extension for Java heap dumps. */
    public static final String DOT_HPROF = DOT + EXT_HPROF; //$NON-NLS-1$

    /** Resource base name for java files and classes */
    public static final String FN_RESOURCE_BASE = "R"; //$NON-NLS-1$
    /** Resource java class  filename, i.e. "R.java" */
    public static final String FN_RESOURCE_CLASS = FN_RESOURCE_BASE + DOT_JAVA;
    /** Resource class file  filename, i.e. "R.class" */
    public static final String FN_COMPILED_RESOURCE_CLASS = FN_RESOURCE_BASE + DOT_CLASS;
    /** Resource text filename, i.e. "R.txt" */
    public static final String FN_RESOURCE_TEXT = FN_RESOURCE_BASE + DOT_TXT;
    /** Filename for public resources in AAR archives */
    public static final String FN_PUBLIC_TXT = "public.txt";
    /** Generated manifest class name */
    public static final String FN_MANIFEST_BASE = "Manifest";          //$NON-NLS-1$
    /** Generated BuildConfig class name */
    public static final String FN_BUILD_CONFIG_BASE = "BuildConfig";   //$NON-NLS-1$
    /** Manifest java class filename, i.e. "Manifest.java" */
    public static final String FN_MANIFEST_CLASS = FN_MANIFEST_BASE + DOT_JAVA;
    /** BuildConfig java class filename, i.e. "BuildConfig.java" */
    public static final String FN_BUILD_CONFIG = FN_BUILD_CONFIG_BASE + DOT_JAVA;

    public static final String DRAWABLE_FOLDER = "drawable";           //$NON-NLS-1$
    public static final String DRAWABLE_XHDPI = "drawable-xhdpi";      //$NON-NLS-1$
    public static final String DRAWABLE_XXHDPI = "drawable-xxhdpi";    //$NON-NLS-1$
    public static final String DRAWABLE_XXXHDPI = "drawable-xxxhdpi";    //$NON-NLS-1$
    public static final String DRAWABLE_HDPI = "drawable-hdpi";        //$NON-NLS-1$
    public static final String DRAWABLE_MDPI = "drawable-mdpi";        //$NON-NLS-1$
    public static final String DRAWABLE_LDPI = "drawable-ldpi";        //$NON-NLS-1$

    // Resources
    public static final String PREFIX_RESOURCE_REF = "@";               //$NON-NLS-1$
    public static final String PREFIX_THEME_REF = "?";                  //$NON-NLS-1$
    public static final String PREFIX_BINDING_EXPR = "@{";              //$NON-NLS-1$
    public static final String PREFIX_TWOWAY_BINDING_EXPR = "@={";      //$NON-NLS-1$
    public static final String MANIFEST_PLACEHOLDER_PREFIX = "${";      //$NON-NLS-1$
    public static final String MANIFEST_PLACEHOLDER_SUFFIX = "}";       //$NON-NLS-1$
    public static final String ANDROID_PREFIX = "@android:";            //$NON-NLS-1$
    public static final String ANDROID_THEME_PREFIX = "?android:";      //$NON-NLS-1$
    public static final String LAYOUT_RESOURCE_PREFIX = "@layout/";     //$NON-NLS-1$
    public static final String STYLE_RESOURCE_PREFIX = "@style/";       //$NON-NLS-1$
    public static final String COLOR_RESOURCE_PREFIX = "@color/";       //$NON-NLS-1$
    public static final String NEW_ID_PREFIX = "@+id/";                 //$NON-NLS-1$
    public static final String ID_PREFIX = "@id/";                      //$NON-NLS-1$
    public static final String DRAWABLE_PREFIX = "@drawable/";          //$NON-NLS-1$
    public static final String STRING_PREFIX = "@string/";              //$NON-NLS-1$
    public static final String DIMEN_PREFIX = "@dimen/";                //$NON-NLS-1$
    public static final String MIPMAP_PREFIX = "@mipmap/";              //$NON-NLS-1$

    public static final String ANDROID_LAYOUT_RESOURCE_PREFIX = "@android:layout/"; //$NON-NLS-1$
    public static final String ANDROID_STYLE_RESOURCE_PREFIX = "@android:style/";   //$NON-NLS-1$
    public static final String ANDROID_COLOR_RESOURCE_PREFIX = "@android:color/";   //$NON-NLS-1$
    public static final String ANDROID_NEW_ID_PREFIX = "@android:+id/";             //$NON-NLS-1$
    public static final String ANDROID_ID_PREFIX = "@android:id/";                  //$NON-NLS-1$
    public static final String ANDROID_DRAWABLE_PREFIX = "@android:drawable/";      //$NON-NLS-1$
    public static final String ANDROID_STRING_PREFIX = "@android:string/";          //$NON-NLS-1$

    public static final String RESOURCE_CLZ_ID = "id";                  //$NON-NLS-1$
    public static final String RESOURCE_CLZ_COLOR = "color";            //$NON-NLS-1$
    public static final String RESOURCE_CLZ_ARRAY = "array";            //$NON-NLS-1$
    public static final String RESOURCE_CLZ_ATTR = "attr";              //$NON-NLS-1$
    public static final String RESOURCE_CLR_STYLEABLE = "styleable";    //$NON-NLS-1$
    public static final String NULL_RESOURCE = "@null";                 //$NON-NLS-1$
    public static final String TRANSPARENT_COLOR = "@android:color/transparent";      //$NON-NLS-1$
    public static final String REFERENCE_STYLE = "style/";                     //$NON-NLS-1$
    public static final String PREFIX_ANDROID = "android:";                    //$NON-NLS-1$
    public static final String PREFIX_APP = "app:";                     //$NON-NLS-1$

    // Resource Types
    public static final String DRAWABLE_TYPE = "drawable";              //$NON-NLS-1$
    public static final String MENU_TYPE = "menu";                      //$NON-NLS-1$

    // Packages
    public static final String ANDROID_PKG_PREFIX = "android.";         //$NON-NLS-1$
    public static final String WIDGET_PKG_PREFIX = "android.widget.";   //$NON-NLS-1$
    public static final String VIEW_PKG_PREFIX = "android.view.";       //$NON-NLS-1$

    // Project properties
    public static final String ANDROID_LIBRARY = "android.library";     //$NON-NLS-1$
    public static final String PROGUARD_CONFIG = "proguard.config";     //$NON-NLS-1$
    public static final String ANDROID_LIBRARY_REFERENCE_FORMAT = "android.library.reference.%1$d";//$NON-NLS-1$
    public static final String PROJECT_PROPERTIES = "project.properties";//$NON-NLS-1$

    // Java References
    public static final String ATTR_REF_PREFIX = "?attr/";               //$NON-NLS-1$
    public static final String R_PREFIX = "R.";                          //$NON-NLS-1$
    public static final String R_ID_PREFIX = "R.id.";                    //$NON-NLS-1$
    public static final String R_LAYOUT_RESOURCE_PREFIX = "R.layout.";   //$NON-NLS-1$
    public static final String R_DRAWABLE_PREFIX = "R.drawable.";        //$NON-NLS-1$
    public static final String R_STYLEABLE_PREFIX = "R.styleable.";      //$NON-NLS-1$
    public static final String R_ATTR_PREFIX = "R.attr.";                //$NON-NLS-1$

    // Attributes related to tools
    public static final String ATTR_IGNORE = "ignore";                   //$NON-NLS-1$
    public static final String ATTR_LOCALE = "locale";                   //$NON-NLS-1$

    // SuppressLint
    public static final String SUPPRESS_ALL = "all";                     //$NON-NLS-1$
    public static final String SUPPRESS_LINT = "SuppressLint";           //$NON-NLS-1$
    public static final String TARGET_API = "TargetApi";                 //$NON-NLS-1$
    public static final String ATTR_TARGET_API = "targetApi";            //$NON-NLS-1$
    public static final String FQCN_SUPPRESS_LINT = "android.annotation." + SUPPRESS_LINT; //$NON-NLS-1$
    public static final String FQCN_TARGET_API = "android.annotation." + TARGET_API; //$NON-NLS-1$

    // Class Names
    public static final String CONSTRUCTOR_NAME = "<init>";                          //$NON-NLS-1$
    public static final String CLASS_CONSTRUCTOR = "<clinit>";                       //$NON-NLS-1$
    public static final String ANDROID_VIEW_VIEW = "android/view/View";              //$NON-NLS-1$

    // Method Names
    public static final String FORMAT_METHOD = "format";                             //$NON-NLS-1$
    public static final String GET_STRING_METHOD = "getString";                      //$NON-NLS-1$


    public static final String ATTR_TAG = "tag";                        //$NON-NLS-1$
    public static final String ATTR_NUM_COLUMNS = "numColumns";         //$NON-NLS-1$

    // Some common layout element names
    public static final String CALENDAR_VIEW = "CalendarView";          //$NON-NLS-1$
    public static final String CHRONOMETER = "Chronometer";             //$NON-NLS-1$
    public static final String TEXT_CLOCK = "TextClock";                //$NON-NLS-1$
    public static final String SPACE = "Space";                         //$NON-NLS-1$
    public static final String GESTURE_OVERLAY_VIEW = "GestureOverlayView";//$NON-NLS-1$
    public static final String QUICK_CONTACT_BADGE = "QuickContactBadge";//$NON-NLS-1$

    public static final String ATTR_HANDLE = "handle";                  //$NON-NLS-1$
    public static final String ATTR_BUTTON = "button";                  //$NON-NLS-1$
    public static final String ATTR_BUTTON_TINT = "buttonTint";         //$NON-NLS-1$
    public static final String ATTR_CONTENT = "content";                //$NON-NLS-1$
    public static final String ATTR_CHECKED = "checked";                //$NON-NLS-1$
    public static final String ATTR_CHECK_MARK = "checkMark";           //$NON-NLS-1$
    public static final String ATTR_CHECK_MARK_TINT = "checkMarkTint";  //$NON-NLS-1$
    public static final String ATTR_DUPLICATE_PARENT_STATE = "duplicateParentState";  //$NON-NLS-1$
    public static final String ATTR_FOCUSABLE = "focusable";            //$NON-NLS-1$
    public static final String ATTR_CLICKABLE = "clickable";            //$NON-NLS-1$
    public static final String ATTR_TEXT_OFF = "textOff";               //$NON-NLS-1$
    public static final String ATTR_TEXT_ON = "textOn";                 //$NON-NLS-1$
    public static final String ATTR_CHECKED_BUTTON = "checkedButton";   //$NON-NLS-1$
    public static final String ATTR_SWITCH_TEXT_APPEARANCE = "switchTextAppearance"; //$NON-NLS-1$
    public static final String ATTR_SWITCH_MIN_WIDTH = "switchMinWidth";//$NON-NLS-1$
    public static final String ATTR_SWITCH_PADDING = "switchPadding";   //$NON-NLS-1$
    public static final String ATTR_THUMB_TINT = "thumbTint";           //$NON-NLS-1$
    public static final String ATTR_TRACK = "track";                    //$NON-NLS-1$
    public static final String ATTR_TRACK_TINT = "trackTint";           //$NON-NLS-1$
    public static final String ATTR_SHOW_TEXT = "showText";             //$NON-NLS-1$
    public static final String ATTR_SPLIT_TRACK = "splitTrack";         //$NON-NLS-1$
    public static final String ATTR_STATE_LIST_ANIMATOR = "stateListAnimator"; //$NON-NLS-1$

    // TextView
    public static final String ATTR_DRAWABLE_RIGHT = "drawableRight";              //$NON-NLS-1$
    public static final String ATTR_DRAWABLE_LEFT = "drawableLeft";                //$NON-NLS-1$
    public static final String ATTR_DRAWABLE_START = "drawableStart";              //$NON-NLS-1$
    public static final String ATTR_DRAWABLE_END = "drawableEnd";                  //$NON-NLS-1$
    public static final String ATTR_DRAWABLE_BOTTOM = "drawableBottom";            //$NON-NLS-1$
    public static final String ATTR_DRAWABLE_TOP = "drawableTop";                  //$NON-NLS-1$
    public static final String ATTR_DRAWABLE_PADDING = "drawablePadding";          //$NON-NLS-1$

    public static final String ATTR_USE_DEFAULT_MARGINS = "useDefaultMargins";      //$NON-NLS-1$
    public static final String ATTR_MARGINS_INCLUDED_IN_ALIGNMENT = "marginsIncludedInAlignment"; //$NON-NLS-1$

    public static final String VALUE_WRAP_CONTENT = "wrap_content";             //$NON-NLS-1$
    public static final String VALUE_FALSE= "false";                            //$NON-NLS-1$
    public static final String VALUE_N_DP = "%ddp";                             //$NON-NLS-1$
    public static final String VALUE_ZERO_DP = "0dp";                           //$NON-NLS-1$
    public static final String VALUE_ONE_DP = "1dp";                            //$NON-NLS-1$
    public static final String VALUE_TOP = "top";                               //$NON-NLS-1$
    public static final String VALUE_BOTTOM = "bottom";                         //$NON-NLS-1$
    public static final String VALUE_CENTER_VERTICAL = "center_vertical";       //$NON-NLS-1$
    public static final String VALUE_CENTER_HORIZONTAL = "center_horizontal";   //$NON-NLS-1$
    public static final String VALUE_FILL_HORIZONTAL = "fill_horizontal";       //$NON-NLS-1$
    public static final String VALUE_FILL_VERTICAL = "fill_vertical";           //$NON-NLS-1$
    public static final String VALUE_0 = "0";                                   //$NON-NLS-1$
    public static final String VALUE_1 = "1";                                   //$NON-NLS-1$

    // Gravity values. These have the GRAVITY_ prefix in front of value because we already
    // have VALUE_CENTER_HORIZONTAL defined for layouts, and its definition conflicts
    // (centerHorizontal versus center_horizontal)
    public static final String GRAVITY_VALUE_ = "center";                             //$NON-NLS-1$
    public static final String GRAVITY_VALUE_CENTER = "center";                       //$NON-NLS-1$
    public static final String GRAVITY_VALUE_LEFT = "left";                           //$NON-NLS-1$
    public static final String GRAVITY_VALUE_RIGHT = "right";                         //$NON-NLS-1$
    public static final String GRAVITY_VALUE_START = "start";                         //$NON-NLS-1$
    public static final String GRAVITY_VALUE_END = "end";                             //$NON-NLS-1$
    public static final String GRAVITY_VALUE_BOTTOM = "bottom";                       //$NON-NLS-1$
    public static final String GRAVITY_VALUE_TOP = "top";                             //$NON-NLS-1$
    public static final String GRAVITY_VALUE_FILL_HORIZONTAL = "fill_horizontal";     //$NON-NLS-1$
    public static final String GRAVITY_VALUE_FILL_VERTICAL = "fill_vertical";         //$NON-NLS-1$
    public static final String GRAVITY_VALUE_CENTER_HORIZONTAL = "center_horizontal"; //$NON-NLS-1$
    public static final String GRAVITY_VALUE_CENTER_VERTICAL = "center_vertical";     //$NON-NLS-1$
    public static final String GRAVITY_VALUE_CLIP_HORIZONTAL = "clip_horizontal";     //$NON-NLS-1$
    public static final String GRAVITY_VALUE_CLIP_VERTICAL = "clip_vertical";         //$NON-NLS-1$
    public static final String GRAVITY_VALUE_FILL = "fill";                           //$NON-NLS-1$

    public static final class ImageViewAttributes {
        public static final String TINT = "tint";
    }

    public static final class PreferenceTags {
        public static final String CHECK_BOX_PREFERENCE = "CheckBoxPreference";
        public static final String EDIT_TEXT_PREFERENCE = "EditTextPreference";
        public static final String LIST_PREFERENCE = "ListPreference";
        public static final String MULTI_SELECT_LIST_PREFERENCE = "MultiSelectListPreference";
        public static final String PREFERENCE_CATEGORY = "PreferenceCategory";
        public static final String PREFERENCE_SCREEN = "PreferenceScreen";
        public static final String RINGTONE_PREFERENCE = "RingtonePreference";
        public static final String SWITCH_PREFERENCE = "SwitchPreference";
    }

    public static final class PreferenceAttributes {
        public static final String ATTR_DEFAULT_VALUE = "defaultValue";
        public static final String ATTR_DEPENDENCY = "dependency";
        public static final String ATTR_DIALOG_ICON = "dialogIcon";
        public static final String ATTR_DISABLE_DEPENDENTS_STATE = "disableDependentsState";
        public static final String ATTR_ENTRIES = "entries";
        public static final String ATTR_ENTRY_VALUES = "entryValues";
        public static final String ATTR_ICON = "icon";
        public static final String ATTR_KEY = "key";
        public static final String ATTR_PERSISTENT = "persistent";
        public static final String ATTR_RINGTONE_TYPE = "ringtoneType";
        public static final String ATTR_SHOW_DEFAULT = "showDefault";
        public static final String ATTR_SHOW_SILENT = "showSilent";
        public static final String ATTR_SINGLE_LINE = "singleLine";
        public static final String ATTR_SUMMARY = "summary";
        public static final String ATTR_SUMMARY_ON = "summaryOn";
        public static final String ATTR_SUMMARY_OFF = "summaryOff";
        public static final String ATTR_SWITCH_TEXT_ON = "switchTextOn";
        public static final String ATTR_SWITCH_TEXT_OFF = "switchTextOff";
    }

    // Text Alignment values.
    public static class TextAlignment {
        public static final String NONE = "none";                                     //$NON-NLS-1$
        public static final String INHERIT = "inherit";                               //$NON-NLS-1$
        public static final String GRAVITY = "gravity";                               //$NON-NLS-1$
        public static final String TEXT_START = "textStart";                          //$NON-NLS-1$
        public static final String TEXT_END = "textEnd";                              //$NON-NLS-1$
        public static final String CENTER = "center";                                 //$NON-NLS-1$
        public static final String VIEW_START = "viewStart";                          //$NON-NLS-1$
        public static final String VIEW_END = "viewEnd";                              //$NON-NLS-1$
    }

    public static class TextStyle {
        public static final String VALUE_BOLD = "bold";                               //$NON-NLS-1$
        public static final String VALUE_ITALIC = "italic";                           //$NON-NLS-1$
    }

    public static final class ViewAttributes {
        public static final String MIN_HEIGHT = "minHeight";
    }

    /**
     * The top level android package as a prefix, "android.".
     */
    public static final String ANDROID_SUPPORT_PKG_PREFIX = ANDROID_PKG_PREFIX + "support."; //$NON-NLS-1$

    /** The android.view. package prefix */
    public static final String ANDROID_VIEW_PKG = ANDROID_PKG_PREFIX + "view."; //$NON-NLS-1$

    /** The android.widget. package prefix */
    public static final String ANDROID_WIDGET_PREFIX = ANDROID_PKG_PREFIX + "widget."; //$NON-NLS-1$

    /** The android.webkit. package prefix */
    public static final String ANDROID_WEBKIT_PKG = ANDROID_PKG_PREFIX + "webkit."; //$NON-NLS-1$

    /** The android.app. package prefix */
    public static final String ANDROID_APP_PKG = ANDROID_PKG_PREFIX + "app."; //$NON-NLS-1$

    /** The android.support.v4. package prefix */
    public static final String ANDROID_SUPPORT_V4_PKG = ANDROID_SUPPORT_PKG_PREFIX + "v4."; //$NON-NLS-1$

    /** The android.support.v7. package prefix */
    public static final String ANDROID_SUPPORT_V7_PKG = ANDROID_SUPPORT_PKG_PREFIX + "v7."; //$NON-NLS-1$

    /** The android.support.design. package prefix */
    public static final String ANDROID_SUPPORT_DESIGN_PKG = ANDROID_SUPPORT_PKG_PREFIX + "design."; //$NON-NLS-1$

    /** The android.support.v17.leanback. package prefix */
    public static final String ANDROID_SUPPORT_LEANBACK_V17_PKG = ANDROID_SUPPORT_PKG_PREFIX + "v17.leanback."; //$NON-NLS-1$

    /** The com.google.android.gms. package prefix */
    public static final String GOOGLE_PLAY_SERVICES_PKG = "com.google.android.gms."; //$NON-NLS-1$

    /** The com.google.android.gms.ads. package prefix */
    public static final String GOOGLE_PLAY_SERVICES_ADS_PKG = GOOGLE_PLAY_SERVICES_PKG + "ads."; //$NON-NLS-1$

    /** The com.google.android.gms.ads. package prefix */
    public static final String GOOGLE_PLAY_SERVICES_MAPS_PKG = GOOGLE_PLAY_SERVICES_PKG + "maps."; //$NON-NLS-1$

    /** The LayoutParams inner-class name suffix, .LayoutParams */
    public static final String DOT_LAYOUT_PARAMS = ".LayoutParams"; //$NON-NLS-1$

    /** The fully qualified class name of an EditText view */
    public static final String FQCN_EDIT_TEXT = "android.widget.EditText"; //$NON-NLS-1$

    /** The fully qualified class name of a LinearLayout view */
    public static final String FQCN_LINEAR_LAYOUT = "android.widget.LinearLayout"; //$NON-NLS-1$

    /** The fully qualified class name of a RelativeLayout view */
    public static final String FQCN_RELATIVE_LAYOUT = "android.widget.RelativeLayout"; //$NON-NLS-1$

    /** The fully qualified class name of a GridLayout view */
    public static final String FQCN_GRID_LAYOUT = "android.widget.GridLayout"; //$NON-NLS-1$
    public static final String FQCN_GRID_LAYOUT_V7 = "android.support.v7.widget.GridLayout"; //$NON-NLS-1$

    /** The fully qualified class name of a FrameLayout view */
    public static final String FQCN_FRAME_LAYOUT = "android.widget.FrameLayout"; //$NON-NLS-1$

    /** The fully qualified class name of a TableRow view */
    public static final String FQCN_TABLE_ROW = "android.widget.TableRow"; //$NON-NLS-1$

    /** The fully qualified class name of a TableLayout view */
    public static final String FQCN_TABLE_LAYOUT = "android.widget.TableLayout"; //$NON-NLS-1$

    /** The fully qualified class name of a GridView view */
    public static final String FQCN_GRID_VIEW = "android.widget.GridView"; //$NON-NLS-1$

    /** The fully qualified class name of a TabWidget view */
    public static final String FQCN_TAB_WIDGET = "android.widget.TabWidget"; //$NON-NLS-1$

    /** The fully qualified class name of a Button view */
    public static final String FQCN_BUTTON = "android.widget.Button"; //$NON-NLS-1$

    /** The fully qualified class name of a RadioButton view */
    public static final String FQCN_RADIO_BUTTON = "android.widget.RadioButton"; //$NON-NLS-1$

    /** The fully qualified class name of a ToggleButton view */
    public static final String FQCN_TOGGLE_BUTTON = "android.widget.ToggleButton"; //$NON-NLS-1$

    /** The fully qualified class name of a Spinner view */
    public static final String FQCN_SPINNER = "android.widget.Spinner"; //$NON-NLS-1$

    /** The fully qualified class name of an AdapterView */
    public static final String FQCN_ADAPTER_VIEW = "android.widget.AdapterView"; //$NON-NLS-1$

    /** The fully qualified class name of a ListView */
    public static final String FQCN_LIST_VIEW = "android.widget.ListView"; //$NON-NLS-1$

    /** The fully qualified class name of an ExpandableListView */
    public static final String FQCN_EXPANDABLE_LIST_VIEW = "android.widget.ExpandableListView"; //$NON-NLS-1$

    /** The fully qualified class name of a GestureOverlayView */
    public static final String FQCN_GESTURE_OVERLAY_VIEW = "android.gesture.GestureOverlayView"; //$NON-NLS-1$

    /** The fully qualified class name of a DatePicker */
    public static final String FQCN_DATE_PICKER = "android.widget.DatePicker"; //$NON-NLS-1$

    /** The fully qualified class name of a TimePicker */
    public static final String FQCN_TIME_PICKER = "android.widget.TimePicker"; //$NON-NLS-1$

    /** The fully qualified class name of a RadioGroup */
    public static final String FQCN_RADIO_GROUP = "android.widgets.RadioGroup";  //$NON-NLS-1$

    /** The fully qualified class name of a Space */
    public static final String FQCN_SPACE = "android.widget.Space"; //$NON-NLS-1$
    public static final String FQCN_SPACE_V7 = "android.support.v7.widget.Space"; //$NON-NLS-1$

    /** The fully qualified class name of a TextView view */
    public static final String FQCN_TEXT_VIEW = "android.widget.TextView"; //$NON-NLS-1$

    /** The fully qualified class name of an ImageView view */
    public static final String FQCN_IMAGE_VIEW = "android.widget.ImageView"; //$NON-NLS-1$

    public static final String ATTR_SRC = "src"; //$NON-NLS-1$
    public static final String ATTR_SRC_COMPAT = "srcCompat"; //$NON-NLS-1$

    public static final String ATTR_GRAVITY = "gravity";        //$NON-NLS-1$

    public static final String ATTR_WEIGHT_SUM = "weightSum"; //$NON-NLS-1$
    public static final String ATTR_EMS = "ems"; //$NON-NLS-1$

    public static final String VALUE_HORIZONTAL = "horizontal"; //$NON-NLS-1$

    public static final String GRADLE_PLUGIN_NAME = "com.android.tools.build:gradle:";
    public static final String GRADLE_EXPERIMENTAL_PLUGIN_NAME = "com.android.tools.build:gradle-experimental:";
    public static final String GRADLE_MINIMUM_VERSION = "2.14.1";
    public static final String GRADLE_LATEST_VERSION = GRADLE_MINIMUM_VERSION;
    public static final String GRADLE_PLUGIN_MINIMUM_VERSION = "1.0.0";
    // Always update GRADLE_PLUGIN_RECOMMENDED_VERSION and GRADLE_EXPERIMENTAL_PLUGIN_RECOMMENDED_VERSION together.
    public static final String GRADLE_PLUGIN_RECOMMENDED_VERSION = "2.2.0";
    public static final String GRADLE_EXPERIMENTAL_PLUGIN_RECOMMENDED_VERSION = "0.8.0";
    public static final String GRADLE_PLUGIN_LATEST_VERSION = GRADLE_PLUGIN_RECOMMENDED_VERSION;
    public static final String GRADLE_EXPERIMENTAL_PLUGIN_LATEST_VERSION = GRADLE_EXPERIMENTAL_PLUGIN_RECOMMENDED_VERSION;
    public static final String GRADLE_COMPILE_CONFIGURATION = "compile";
    public static final String GRADLE_TEST_COMPILE_CONFIGURATION = "testCompile";
    public static final String GRADLE_ANDROID_TEST_COMPILE_CONFIGURATION = "androidTestCompile";
    public static final String MIN_BUILD_TOOLS_VERSION = "19.1.0";
    public static final String SUPPORT_LIB_ARTIFACT = "com.android.support:support-v4";
    public static final String DESIGN_LIB_ARTIFACT = "com.android.support:design";
    public static final String APPCOMPAT_LIB_ARTIFACT = "com.android.support:appcompat-v7";
    public static final String CARD_VIEW_LIB_ARTIFACT = "com.android.support:cardview-v7";
    public static final String GRID_LAYOUT_LIB_ARTIFACT = "com.android.support:gridlayout-v7";
    public static final String RECYCLER_VIEW_LIB_ARTIFACT = "com.android.support:recyclerview-v7";
    public static final String MAPS_ARTIFACT = "com.google.android.gms:play-services-maps";
    public static final String ADS_ARTIFACT = "com.google.android.gms:play-services-ads";
    public static final String LEANBACK_V17_ARTIFACT = "com.android.support:leanback-v17";
    public static final String ANNOTATIONS_LIB_ARTIFACT = "com.android.support:support-annotations";

    // Annotations
    public static final String SUPPORT_ANNOTATIONS_PREFIX = "android.support.annotation.";
    public static final String INT_DEF_ANNOTATION = SUPPORT_ANNOTATIONS_PREFIX + "IntDef";
    public static final String STRING_DEF_ANNOTATION = SUPPORT_ANNOTATIONS_PREFIX + "StringDef";
    public static final String TYPE_DEF_VALUE_ATTRIBUTE = "value";
    public static final String TYPE_DEF_FLAG_ATTRIBUTE = "flag";
    public static final String FN_ANNOTATIONS_ZIP = "annotations.zip";
    public static final String BINDING_ADAPTER_ANNOTATION = "android.databinding.BindingAdapter";

    // Data Binding MISC
    public static final String DATA_BINDING_LIB_ARTIFACT = "com.android.databinding:library";
    public static final String DATA_BINDING_BASELIB_ARTIFACT = "com.android.databinding:baseLibrary";
    public static final String DATA_BINDING_ANNOTATION_PROCESSOR_ARTIFACT =
            "com.android.databinding:compiler";
    public static final String DATA_BINDING_ADAPTER_LIB_ARTIFACT =
            "com.android.databinding:adapters";
    public static final String[] TAGS_DATA_BINDING = new String[]{TAG_VARIABLE,
        TAG_IMPORT, TAG_LAYOUT, TAG_DATA};
    public static final String[] ATTRS_DATA_BINDING = new String[]{ATTR_NAME,
        ATTR_TYPE, ATTR_CLASS, ATTR_ALIAS};
    public static final String DATA_BINDING_VIEW_STUB_PROXY = "android.databinding.ViewStubProxy";

    /** Name of keep attribute in XML */
    public static final String ATTR_KEEP = "keep";
    /** Name of discard attribute in XML (to mark resources as not referenced, despite guesses) */
    public static final String ATTR_DISCARD = "discard";
    /** Name of attribute in XML to control whether we should guess resources to keep */
    public static final String ATTR_SHRINK_MODE = "shrinkMode";
    /** {@linkplain #ATTR_SHRINK_MODE} value to only shrink explicitly encountered resources */
    public static final String VALUE_STRICT = "strict";
    /** {@linkplain #ATTR_SHRINK_MODE} value to keep possibly referenced resources */
    public static final String VALUE_SAFE = "safe";

    /** Prefix of the Android Support Repository path */
    public static final String ANDROID_SUPPORT_ARTIFACT_PREFIX = "com.android.";
    /** Prefix of the Google Repository path */
    public static final String GOOGLE_SUPPORT_ARTIFACT_PREFIX = "com.google.android.";
}
