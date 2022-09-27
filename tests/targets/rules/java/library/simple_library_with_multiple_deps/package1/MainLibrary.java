package package1;

import package2.Util1;
import package2.Util2;
import package2.Util3;
import package2.Util4;
import package2.Util5;

public class MainLibrary {

  public static void main(String[] args) {
    System.out.println("Hello from MainLibrary !!!");
    System.out.printf(
        "Hello from Utils deps : %s, %s, %s, %s, %s%n",
        Util1.getString(),
        Util2.getString(),
        Util3.getString(),
        Util4.getString(),
        Util5.getString());
    System.out.println("That's all folks!");
  }
}
