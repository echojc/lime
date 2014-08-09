package lime;

public class Nil implements List {
  private Nil() {}
  private static List self = new Nil();
  public static List get() { return self; }
  public Object head() { throw new RuntimeException("head of nil"); }
  public List tail() { throw new RuntimeException("tail of nil"); }
  public String toString() { return "nil"; }
  public boolean equals(Object o) { return o == self; }
  public int hashCode() { return self.hashCode(); }
}
