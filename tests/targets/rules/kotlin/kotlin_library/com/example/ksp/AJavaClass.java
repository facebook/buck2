package com.example.ksp;

@com.example.TestAnnotation
class AJavaClass {
  public void foo() {
    new JavaClassWithJavaAnnotation_kspgen_java_class();
    new KotlinClassWithJavaAnnotation_kspgen_java_class();
    new JavaClassWithKotlinAnnotation_kspgen_kotlin_class();
    new KotlinClassWithKotlinAnnotation_kspgen_kotlin_class();
    GeneratedAJavaClass.foo();
  }
}
