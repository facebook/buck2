# Getting Started

**Note:** If you are a member of an organization that is using Buck, please consult with your colleagues to see if your organization has *site-specific documentation* for Buck. Buck is flexible and configurable, and many organizations have created their own Buck documentation, which is specific to their use cases—in addition to the documentation here.

## Quick Starts for various target platforms



|**Platform:**   |AndroidiOSJavaOther   |
|---   |---   |
|**Development OS:**   |macOSLinuxWindows   |

>While not a prerequisite for installing Buck itself, to build Android applications, you will also need at least the [Android SDK](https://developer.android.com/studio/index.html) and the [Android NDK](https://developer.android.com/ndk/index.html), which can be installed via [Homebrew](http://brewformulas.org/Android-sdk) or manually downloaded and installed.

The commands in this guide are designed to be copy-pasteable, idempotent, and usable on its representative operating system (macOS, Linux, Windows). Sometimes this results in some unusual constructions (such as using `echo` instead of `vi` or `Emacs` to create a file). Bear in mind that this is a *quick* start guide, and few things are quicker than copy-and-paste!

## Install with Homebrew

Buck is available as a bottle on [Homebrew](http://brew.sh/).

### Prerequisites

* [Command Line Tools](https://developer.apple.com/xcode/features/)
* [Java Runtime Environment version 11](https://java.com/en/download/) (support for future versions is in the works)
    If you have multiple installations of Java on your development computer, you might get warnings from Buck that you are using an unsupported version of Java. To resolve this issue, set the `JAVA_HOME` environment variable to the directory for **version 8** of the Java Development Kit (JDK). Note that the directory that `JAVA_HOME` points to should contain a `bin` subdirectory which in turn contains binaries for the Java compiler (`javac`) and Java runtime (`java`).

```
# Install command line tools. NOTE: If you have Xcode installed, these may
# already be installed.
xcode-select --install
# Download and Install Java SE 8 from:
+# https://www.oracle.com/technetwork/java/javase/downloads/index.html.
+# This installs the JDK 8, a superset of the JRE.
+# Alternatively, install AdoptOpenJDK 8 with Homebrew:
+brew tap AdoptOpenJDK/openjdk
+brew install --cask adoptopenjdk8
```

### Brew install

You have two choices when using Homebrew. You can choose to get the latest binary [release](https://github.com/facebook/buck/releases/latest):

```
brew tap facebook/fb
brew install buck
```

Or, you can get the latest code and build it locally:

```
brew update
brew tap facebook/fb
brew install --HEAD buck
```

## Build from Source

### Prerequisites

To manually build Buck, download and install the following prerequisites:

* [Command Line Tools](https://developer.apple.com/xcode/features/)
* [Oracle Java Development Kit version 8](http://www.oracle.com/technetwork/java/javase/downloads/index.html) (support for future versions is in the works)
* [Apache Ant 1.9 (or newer)](http://ant.apache.org/)
* [Python 2.7](https://www.python.org/downloads/)
* [Git](http://git-scm.com/download)
* [Watchman](https://facebook.github.io/watchman/docs/install)

>We strongly recommended that you install Watchman. With watchman, Buck uses a daemon ([buckd](https://buck.build/command/buckd.html)) which prevents Buck from needing to parse all of your [build files](https://buck.build/concept/build_file.html) every time you build—and it caches some other components of your build as well.

You can use [Homebrew](http://homebrew.sh/) to install many of the prerequisites on a Mac.

```
# Install Command Line tools first. NOTE: If you have Xcode installed, these may
# already be installed.
xcode-select --install
# Then the JDK (superset of the JRE)
brew update
brew tap caskroom/cask
brew tap caskroom/versions
brew cask install java8
# Then...
brew install ant python git watchman
```

### Build

Once you have the above tools installed, you can build Buck as follows:

```
git clone https://github.com/facebook/buck.git
cd buck
ant
./bin/buck build --show-output buck
buck-out/gen/programs/buck.pex --help
```

If everything worked correctly, you should see something like:

```
buck build tool
usage:
  buck [options]
  buck command --help
  buck command [command-options]
available commands:
  audit       lists the inputs for the specified target
  build       builds the specified target
  cache       makes calls to the artifact cache
  clean       deletes any generated files
  fetch       downloads remote resources to your local machine
  install     builds and installs an application
  kill        kill buckd for the current project
  killall     kill all buckd processes
  project     generates project configuration files for an IDE
  query       provides facilities to query information about the configured target nodes graph
  root        prints the absolute path to the root of the current buck project
  run         runs a target as a command
  server      query and control the http server
  targets     prints the list of buildable targets
  test        builds and runs the tests for the specified target
  uninstall   uninstalls an APK
  uquery      provides facilities to query information about the unconfigured target nodes graph
options:
 --help         : Shows this screen and exits.
 --version (-V) : Show version number.
```

Because you will likely be running `./bin/buck` often, you should add it to your path so that you can simply run `buck` from the command line.

### Set Location of Android SDK and NDK

You will need to tell Buck where to find the Android SDK and NDK.
To find the location of the **Android SDK**, Buck looks at the following values *in the following order*:

* `ANDROID_SDK` environment variable
* `ANDROID_HOME` environment variable
* `ANDROID_SDK_ROOT` environment variable
* The value of the [`[android].sdk_path`](https://buck.build/files-and-dirs/buckconfig.html#android.sdk_path) property in `.buckconfig`.

To find the location of a specific **NDK**, Buck looks at the following values *in the following order*:

* `ANDROID_NDK` environment variable.
* `NDK_HOME` environment variable.
* The value of the [`[ndk].ndk_path`](https://buck.build/files-and-dirs/buckconfig.html#ndk.ndk_path) property in `.buckconfig`.

If you have **multiple NDKs** installed into a single enclosing directory, you can specify this directory to Buck using either of the following values:

* `ANDROID_NDK_REPOSITORY` environment variable.
* The [`[ndk].ndk_repository_path`](https://buck.build/files-and-dirs/buckconfig.html#ndk.ndk_repository_path) property in `.buckconfig`.

If you specify *both* the environment variable and the `.buckconfig` setting, the environment variable takes precedence.
If you specify an NDK repository, Buck selects the NDK based on the version that you specify in the [`[ndk].ndk_version`](https://buck.build/files-and-dirs/buckconfig.html#ndk.ndk_version) property of `.buckconfig`.

## Trying Buck

Now that Buck is installed, it is time to use Buck in a sample project.

### Clone [Buck samples repo](https://github.com/fbsamples/bucksamples/)

```
git clone https://github.com/fbsamples/bucksamples.git
cd bucksamples/cross-platform-scale-2015-demo/
```

### Key Android Files

This sample app has all the files necessary to use Buck to build an Android project. From the root directory, you will find:

* `android/java/com/facebook/buck/demo/Hello.java`: The main Java file supported by other associated resources.
* `android/BUCK`: The [build file](https://buck.build/concept/build_file.html) is what makes Buck work. It defines all the [build rule](https://buck.build/concept/build_rule.html)s for your source code. A [build rule](https://buck.build/concept/build_rule.html) can also include dependencies (generally via `deps`), which may be from other [build file](https://buck.build/concept/build_file.html)s, as in the case of this app.
* `.buckconfig`: A [`.buckconfig`](https://buck.build/files-and-dirs/buckconfig.html) file allows for various flag and alias settings for any project (even beyond Android) within the root directory.

### Configure the environment

Before building an app you need to configure environment variables to let Buck know the locations of Android SDK and Android NDK.
First of all, check for existing variables:

```
$ env | grep ANDROID_
ANDROID_HOME=<path-to-sdk>
ANDROID_NDK_REPOSITORY=<path-to-ndk>
ANDROID_SDK=<path-to-sdk>
ANDROID_SDK_ROOT=<path-to-sdk>
```

Set the missing variables to the locations of Android SDK and Android NDK or set the paths in your [`.buckconfig`](https://buck.build/files-and-dirs/buckconfig.html) file.
Before building make sure you installed correct build tools and a target in Android SDK and correct version of Android NDK. You can find the required versions of these tools in [`.buckconfig`](https://buck.build/files-and-dirs/buckconfig.html):

* See [`[android].build_tools_version`](https://buck.build/files-and-dirs/buckconfig.html#android.build_tools_version) to get the version of build tools in Android SDK.
* [`[android].compile_sdk_version`](https://buck.build/files-and-dirs/buckconfig.html#android.compile_sdk_version) points to the Android SDK to build against.
* [`[ndk].ndk_version`](https://buck.build/files-and-dirs/buckconfig.html#ndk.ndk_version) points to the version of Android NDK.

Optionally:

* [`[android].sdk_path`](https://buck.build/files-and-dirs/buckconfig.html#android.sdk_path) is an absolute path to the Android SDK.
* [`[ndk].ndk_path`](https://buck.build/files-and-dirs/buckconfig.html#ndk.ndk_path) is an absolute path to the Android NDK.
* [`[ndk].ndk_repository_path`](https://buck.build/files-and-dirs/buckconfig.html#ndk.ndk_repository_path) is an absolute path to a directory that contains multiple Android NDKs in subdirectories. Buck selects which NDK to use based on the value of the [`[ndk].ndk_version`](https://buck.build/files-and-dirs/buckconfig.html#ndk.ndk_version) property in `.buckconfig`.

### Build the Android sample

In order to build the app, you use the [`buck build`](https://buck.build/command/build.html)command, specifying your app as the target. The target may be defined in the [`[alias]`](https://buck.build/files-and-dirs/buckconfig.html#alias) section in the [`.buckconfig`](https://buck.build/files-and-dirs/buckconfig.html) file or it would be the name of your Android project prepended by `//[the directory where your project is located]:` (e.g., `//android:demo-app`).

```
# From the root `cross-platform-scale-2015-demo/` directory
# demo_app_android is an alias in .buckconfig for //android:demo-app. Either works.
buck build demo_app_android
```

You should see output similar to:

```
export ANDROID_NDK=$HOME/android-sdk
buck build demo_app_android
[-] PROCESSING BUCK FILES...FINISHED 0.0s [100%]
[-] DOWNLOADING... (0.00 B/S AVG, TOTAL: 0.00 B, 0 Artifacts)
[-] BUILDING...FINISHED 0.7s [100%] (1/1 JOBS, 0 UPDATED, 0 [0.0%] CACHE MISS)
```

>The first time you build, you will most likely see a longer time and cache misses. Subsequent builds should be much faster, with minimal cache misses.

Buck outputs its results in the `buck-out/` directory.

### Run the built Android App

Now that you know your app has built successfully, you can install and run the app with [`buck install`](https://buck.build/command/install.html). This command both compiles and installs the application on the Android emulator. Using the `--run` flag will launch the emulator as well.

```
buck install --run demo_app_android
Installing apk on emulator-5554 (android-emulator).
[-] PROCESSING BUCK FILES...FINISHED 0.1s [100%]
[-] DOWNLOADING... (0.00 B/S AVG, TOTAL: 0.00 B, 0 Artifacts)
[-] BUILDING...FINISHED 0.8s [100%] (1/1 JOBS, 0 UPDATED, 0 [0.0%] CACHE MISS)
[+] INSTALLING...0.9s
Successfully ran install apk //android:demo-app on 1 device(s)
Starting activity com.facebook.buck.demo/.App...
Successfully ran start activity on 1 device(s)
```

>If you get an error either that you do not have certain Android add-ons (e.g., Google APIs) or that there is no emulator to run, you should launch the Android SDK Manager (e.g., `android sdk`) and install the appropriate packages and/or run your emulator (usually found under `Tools | Manage AVDs`).

### Success!

If all goes well, you should see something similar to:
