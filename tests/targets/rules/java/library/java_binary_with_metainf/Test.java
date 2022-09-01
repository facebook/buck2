import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class Test {

  public static void main(String[] args) throws IOException {
    InputStream inputStream =
        Test.class.getClassLoader().getResourceAsStream("META-INF/bar/baz.txt");
    String text = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
    System.out.println(text);
  }
}
