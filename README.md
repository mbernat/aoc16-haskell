These are my hacky Haskell solutions for the Advent of Code 2016.

The solutions are presented in the form of library modules, each of them providing `solve` methods and either embedding `input` directly as data when that's reasonable, or else providing a function to `load` it.

Example usage.

```
stack repl

> Day6.solve . Day6.load <$> Day6.input
```