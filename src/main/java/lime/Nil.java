package lime;
import java.io.Serializable;
public class Nil extends List implements Serializable {
  private Nil() {}
  private static List instance = new Nil();
  public static List get() {
    return instance;
  }
  public Object car() throws Exception { throw new Exception("car of nil"); }
  public Object cdr() throws Exception { throw new Exception("cdr of nil"); }
  public String toString() { return "()"; }
  public Object len() throws Exception { return Double.valueOf("0"); }
}
