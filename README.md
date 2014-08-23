# lime

This is a work in progress: my own Lisp that runs on the JVM!

## Example

Put this in a file called `example.lime` at `/some/location`:

```lisp
(def fold (lst acc fun)
  (if (empty lst)
    acc
    (fold (cdr lst) (fun acc (car lst)) fun)))

(def sum (acc nxt)
  (+ acc (int nxt)))

(put (fold args 0 sum))
```

Copy and compile the stdlib sources (for now, this is a manual step):

```
cp -r stdlib/src/main/java/* /some/location
javac /some/location/**/*.java
```

Compile with sbt:

```
sbt 'compiler/run /some/location/example.lime'
```

Execute with java:

```
cd /some/location
java example 42 23
```

Output:

```
65
```
