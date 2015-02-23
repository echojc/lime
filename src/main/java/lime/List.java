package lime;
import java.io.Serializable;
public abstract class List implements Serializable {
  public abstract Object car() throws Exception;
  public abstract Object cdr() throws Exception;
  public abstract Object len() throws Exception;
}
