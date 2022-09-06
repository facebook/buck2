# Tutorials

This expanded tutorial shows extended concepts about using Buck to build a project after you have installed Buck, including creating a project, building a project, packaging a project, etc.

>**Currently this tutorial is Android specific for either Mac or Linux. We will be adding iOS, Java and Windows specific tutorial information in the near future.**



|**Platform:**    |AndroidMacOSLinux    |
|---    |---    |
|**Development OS:**    |macOSLinux    |
|**Language:**    |JavaKotlinRust    |

## Path Setup

Add Buck to your `$PATH` and set up [`buckd`](https://buck.build/concept/buckd.html):

```
sudo ln -s ${PWD}/bin/buck /usr/bin/buck
sudo ln -s ${PWD}/bin/buckd /usr/bin/buckd
```

## Create Project

We are going to build a sample application. We should start our project in an empty directory, so create a new one and navigate to it:

```
mkdir -p ~/my-first-buck-project/
cd ~/my-first-buck-project/
```

>**Note: the following instructions will now assume that all commands are run from your `~/my-first-buck-project` directory.**

## Compile Your Code

Android applications are typically written in Java and kotlin, so the first thing we will do is to configure Buck to compile code against the Android API. To do so, Buck needs to know where your Android SDK is. Assuming that your Android SDK is installed in `~/android-sdk`, run the following command to set a `ANDROID_SDK` environment variable that tells Buck where to find your Android SDK:

```
export ANDROID_SDK=$HOME/android-sdk
```

Now that Buck can locate your Android SDK, it is time to compile some Java code. First, we create a simple `Activity` at `java/com/example/activity/MyFirstActivity.java`:

```
mkdir -p java/com/example/activity/
echo "package com.example.activity;

import android.app.Activity;
import android.os.Bundle;

public class MyFirstActivity extends Activity {

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
  }
}" > java/com/example/activity/MyFirstActivity.java
```

Now we need a build file that defines a build rule to compile this Java code, so we create an [`android_library()`](https://buck.build/learning/rule/android_library.html) rule in `java/com/example/activity/BUCK`:

```
echo "android_library(
  name = 'activity',
  srcs = glob(['*.java']),
  visibility = [ 'PUBLIC' ],
)" > java/com/example/activity/BUCK
```

Now we can compile our Java code using Buck:

```
buck build //java/com/example/activity:activity
```

>Buck generates its output in the `buck-out` directory, so this is a good time to specify `buck-out` as something that should be ignored by your version control system.

## Package Resources

Android applications frequently contain resources, such as strings and images. For this example, we will create a trivial Android resource bundle that contains a single string:

```
mkdir -p res/com/example/activity/res/values/
echo "<?xml version='1.0' encoding='utf-8' ?>
<resources>
  <string name='app_name'>Hello World</string>
</resources>" > res/com/example/activity/res/values/strings.xml
```

Buck needs a way to reference this collection of resources, so we need to create a build file that defines an [`android_resource`](https://buck.build/rule/android_resource.html) rule:

```
echo "android_resource(
  name = 'res',
  res = subdir_glob([('res', '**')]),
  package = 'com.example',
  visibility = [
    '//apps/myapp:',
  ],
)" > res/com/example/activity/BUCK
```

## Create a Keystore

In practice, you will want to be able to test your Android app on a physical Android device, which means that it needs to be signed. We will create app-specific information, such as the key and manifest, in its own directory to keep things tidy:

```
mkdir -p apps/myapp/
```

To keep things simple, we will create a self-signed certificate for debugging.

>Unfortunately, this is not a one-liner because there is a number of prompts from the `keytool` command.

```
keytool -genkey -keystore apps/myapp/debug.keystore -alias my_alias \
    -keyalg RSA -keysize 2048 -validity 10000
```

When prompted for a keystore password, just use `android` (and then type it again to confirm it), and hit `Enter` to accept the default values for name, organizational unit, etc.
Then create a `.properties` file that stores all of this information:

```
echo "key.alias=my_alias
key.store.password=android
key.alias.password=android" > apps/myapp/debug.keystore.properties
```

## Build an APK

An Android application needs a manifest named `AndroidManifest.xml`, so we must create such a file:

```
echo "<?xml version='1.0' encoding='utf-8'?>
<manifest xmlns:android='http://schemas.android.com/apk/res/android'
          package='com.example'
          >

  <application
      android:label='@string/app_name'
      android:hardwareAccelerated='true'>
    <activity android:name='.activity.MyFirstActivity'>
      <intent-filter>
        <action android:name='android.intent.action.MAIN' />
        <category android:name='android.intent.category.LAUNCHER' />
      </intent-filter>
    </activity>
  </application>

</manifest>" > apps/myapp/AndroidManifest.xml
```

Now we define an [`android_binary`](https://buck.build/rule/android_binary.html) and [`keystore`](https://buck.build/rule/keystore.html) rule in our build file:

```
echo "android_binary(
  name = 'app',
  manifest = 'AndroidManifest.xml',
  manifest_entries = {
    'version_code': 1,
    'version_name': '1.0',
    'min_sdk_version': 26,
    'target_sdk_version': 29
  },
  keystore = ':debug_keystore',
  deps = [
    '//java/com/example/activity:activity',
    '//res/com/example/activity:res',
  ],
)

keystore(
  name = 'debug_keystore',
  store = 'debug.keystore',
  properties = 'debug.keystore.properties',
)" > apps/myapp/BUCK
```

Building an [`android_binary`](https://buck.build/rule/android_binary.html) rule will produce an APK:

```
buck build //apps/myapp:app
```

Alternatively, if you have an Android device connected to your computer, you can build and install the APK in one step with [`buck install`](https://buck.build/command/install.html):

```
buck install //apps/myapp:app
```

## Create an Alias

Typing `buck build //apps/myapp:app` every time you want to rebuild your APK can be tedious. Fortunately, Buck makes it possible to define an *alias* for a build target. An alias can always be used in place of a build target when using Buck's command-line interface.
Aliases must be defined in the [`[alias]`](https://buck.build/files-and-dirs/buckconfig.html#alias) a config file in the root of the project:

```
echo "[alias]
    app = //apps/myapp:app" > .buckconfig
```

With this alias in place, the command to build and install the APK is much shorter and easier to remember:

```
buck install app
```

## Create an IntelliJ Project

You likely want to develop your Android app using an IDE. Fortunately, Buck can generate an IntelliJ project from the build rules you defined in your build files.
In order to ensure that IntelliJ recognizes where your Java folders are, you need to specify the [`[java].src_roots`](https://buck.build/files-and-dirs/buckconfig.html#java.src_roots) in your [`.buckconfig`](https://buck.build/files-and-dirs/buckconfig.html) file:

```
echo "[java]
    src_roots = /java/" >> .buckconfig
```

Now you can create the IntelliJ project by running [`buck project`](https://buck.build/command/project.html):

```
buck project --ide intellij
```

Note that you will likely want to exclude these generated files from version control, so add the following to your `.gitignore` file (or `.hgignore` if you are using Mercurial) along with the files generated by [`buckd`](https://buck.build/concept/buckd.html):

```
echo "/.buckd
/buck-out
*.iml
/.idea/compiler.xml
/.idea/libraries/*.xml
/.idea/modules.xml
/.idea/runConfigurations/Debug_Buck_test.xml" > .gitignore
```

Now you can build your Android application from either IntelliJ or the command line.
