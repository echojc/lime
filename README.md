# lime

This is a work in progress: my own Lisp that runs on the JVM!

## Example

Put this in a file called `example.lime` at `/some/location`:

```lisp
(def foo (a)
  (+ (int a) 42))

(put (car (cdr args)))
(put (foo (car args)))
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
java example 23 bar
```

Output:

```
bar
65
```
