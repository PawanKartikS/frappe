# frappe
A JVM written in Scala with basic optimisations such as inlining, escape analysis, loop unrolling, and JIT.

```shell
sbt:frappe> run -cp=/path/to/your/class/files/ -main=/path/to/your/class/files/main
```

Note:
1. Invocation arguments and process is subject to change.
2. Optimizations such as JIT and loop unrolling are yet to be checked-in.
3. There's a lot of work in progress. The purpose of frappe isn't to match existing mature JVMs but rather to act as a fun project.
