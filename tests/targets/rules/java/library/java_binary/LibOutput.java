public class LibOutput {

  private LibOutput() {}

  public static void main(String[] args) {
    System.out.println("output");
    System.err.println("error");
    System.out.println(LibUtil.getMessage());
  }
}
