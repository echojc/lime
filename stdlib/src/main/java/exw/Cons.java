package exw;

public class Cons implements List {
  private Object _head;
  private List _tail;

  public Object head() { return _head; }
  public List tail() { return _tail; }

  public Cons(Object head, List tail) {
    _head = head;
    _tail = tail;
  }
}
