package com.example;

import com.sun.source.util.JavacTask;
import com.sun.source.util.Plugin;

public class JavacPlugin implements Plugin {

  @Override
  public String getName() {
    return "MyPlugin";
  }

  @Override
  public void init(JavacTask task, String... args) {
    String helloFromUtilString = new Util().getHelloString();
    // you will see this as an error output of javac
    System.err.println(
        "Sys err from "
            + this.getClass().getSimpleName()
            + "#init method. Util method: "
            + helloFromUtilString);
  }
}
