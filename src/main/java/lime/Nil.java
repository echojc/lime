package lime;
public class Nil extends List {
  private Nil() {}
  private static List instance = new Nil();
  public static List get() {
    return instance;
  }
  public Object car() throws Exception { throw new Exception("car of nil"); }
  public Object cdr() throws Exception { throw new Exception("cdr of nil"); }
  public String toString() { return "()"; }
  public Double len() throws Exception { return 0.0; }
}
