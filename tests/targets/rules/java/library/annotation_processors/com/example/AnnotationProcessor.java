package com.example;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Set;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.FileObject;
import javax.tools.JavaFileObject;
import javax.tools.StandardLocation;

@SupportedAnnotationTypes("com.example.TestAnnotation")
public final class AnnotationProcessor extends AbstractProcessor {

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    try {
      for (TypeElement typeElement : annotations) {
        for (Element element : roundEnv.getElementsAnnotatedWith(typeElement)) {
          String className = ((TypeElement) element).getQualifiedName().toString();
          int lastDot = className.lastIndexOf('.');
          String packageName = className.substring(0, lastDot);
          String simpleName = className.substring(lastDot + 1);

          String generatedSimpleName = "Generated" + simpleName;
          String generatedName = packageName + "." + generatedSimpleName;

          JavaFileObject sourceFile = processingEnv.getFiler().createSourceFile(generatedName);
          String propertyValue = processingEnv.getOptions().getOrDefault("propKey", "");
          String methodName1 = processingEnv.getOptions().getOrDefault("methodName1", null);
          String methodName2 = processingEnv.getOptions().getOrDefault("methodName2", null);

          try (PrintWriter out = new PrintWriter(sourceFile.openWriter())) {
            out.println("package " + packageName + ";\n");
            out.println("public class " + generatedSimpleName + " {");
            out.println(
                "  public static String foo() { com.example.Util.doStuff(); return \"Hello from annotation processor. \" + \""
                    + propertyValue
                    + "\" ; }");
            if (methodName1 != null) {
              out.println("  public static void " + methodName1 + "() {}");
            }
            if (methodName2 != null) {
              out.println("  public static void " + methodName2 + "() {}");
            }
            out.println("}");
          }

          FileObject resourceFile =
              processingEnv
                  .getFiler()
                  .createResource(
                      StandardLocation.CLASS_OUTPUT, "", "_RESOURCES.test." + generatedName);
          try (Writer writer = resourceFile.openWriter()) {
            writer.write("TestResource");
          }
        }
      }
      return false;
    } catch (IOException ex) {
      throw new RuntimeException(ex);
    }
  }
}
