package lime;

public class Cons implements List {
  private Object _head;
  private List _tail;

  public Object head() { return _head; }
  public List tail() { return _tail; }

  public Cons(Object head, List tail) {
    _head = head;
    _tail = tail;
  }

  public String toString() {
    String out = "'(" + _head.toString();
    List l = _tail;
    while (!(l instanceof Nil)) {
      out += " " + l.head();
      l = l.tail();
    }
    return out + ")";
  }
}
