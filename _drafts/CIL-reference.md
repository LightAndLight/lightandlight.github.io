# CIL Reference

Minimal CIL program

```
.assembly extern mscorlib {}
.assembly test {}
.method public static void main() cil managed {
  .entrypoint
  ldstr "test"
  call void [mscorlib]System.Console::WriteLine(class System.String)
  ret
}
```

`.assembly extern` references an external assembly
`.assembly` specifies this assembly name
`.method` declares a method
