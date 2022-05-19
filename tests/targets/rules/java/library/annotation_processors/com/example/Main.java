package com.example;

@TestAnnotation
public class Main {
  public static void main(String[] args) {
    System.out.println(GeneratedMain.foo());
    System.out.println("Hello from " + Main.class.getSimpleName());
  }
}
