public class Main {

  public static void main(String[] args) {
    System.out.printf("Hello from Main %s !!!%n", new StringBuilder().appendCodePoint(0x1F4A3));
    System.out.println("Hello ⊗ from Utils dep: " + Utils.getString());
  }
}
