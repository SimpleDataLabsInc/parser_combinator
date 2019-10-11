# Scala Packrat Parser Combinators for DSLs

This is an example packrat parser combinator for a small DSL that parses these two functions:

```scala
def foo(a: Int, b: Int, c: Int) : Int = {
  let d = a + b * c;
  let e = a + b + d;
  return e;
}
```
and
```scala
def bar(a: Int, b: Int, c: Int) : Int = {
  let d = a > b and c > a + b;
  return d;
}
```

It shows

* Example of full lexer and parser using packrat parsers
* Writing lexer using RegexParsers
* Parsing that correctly handles
  * operator precedence (4 example levels)
  * operator associativity
  * generating abstract syntax tree (AST)
* Debugging support for parsing rules

# Run

```bash
./gradlew run
```
