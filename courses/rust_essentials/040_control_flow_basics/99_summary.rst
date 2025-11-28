=========
Summary
=========

-----------------
What We Covered
-----------------

- **Expressions** and **Blocks** 

  - Blocks have a value and type defined by its last expression
  - All branches of an :rust:`if`/:rust:`match` expression must return the same type
  - :rust:`match` must be exhaustive

- **Loops** 

  - Only the :rust:`loop` construct is both a loop and an expression
  - Skip the current iteration with :rust:`continue`
  - Exit any loop type early with :rust:`break`
  - Use labels with :rust:`break` and :rust:`continue`

- **Functions** and **Macros** 

    - The last expression, semicolon-less, is the function return value
    - No overloading, no default value for parameters
    - Macros: compile-time expanded code taking variable arguments
      - always suffixed with :rust:`!`
