package lime;
import java.io.Serializable;
public class Cons extends List implements Serializable {
  private Object _car;
  private Object _cdr;
  private Double _len;
  public Cons(Object car, Object cdr) {
    _car = car;
    _cdr = cdr;

    try {
      if (cdr instanceof List)
        _len = (Double)((List)cdr).len() + 1;
      else
        _len = null;
    } catch (Exception e) { _len = null; }
  }
  public Object car() throws Exception { return _car; }
  public Object cdr() throws Exception { return _cdr; }
  public String toString() { return "(" + _car.toString() + " " + _cdr.toString() + ")"; }
  public Object len() throws Exception {
    if (_len == null)
      throw new Exception("not a list");
    return _len;
  }
}
