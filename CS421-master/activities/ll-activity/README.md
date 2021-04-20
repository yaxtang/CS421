# LL Parser Activity

Your work is to generate an LL parser for the following grammar:

```
E -> + E E
   | int
   | var
   | ( E )
   | let var = E in E end
```

The code is in `src/Lib.hs`. We have given you code to parse `+` and integers.
The top-level parsing function is called `parse`, and has type `String -> Exp`.

You can run the program using `stack repl`. 

## Some parsing notes

You can assume the tests will only give you valid programs.

For the variable names, allow any combination of lowercase letters.

## Test Suite

A test suite is in the works.  Expect it on Wednesday.
