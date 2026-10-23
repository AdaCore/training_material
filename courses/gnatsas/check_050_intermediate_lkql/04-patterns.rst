==========
Patterns
==========

------------------
Review: Patterns
------------------

* Previous module covered basic patterns

* Node patterns

  * Select node(s) that match some criteria

* Value patterns

  * Compare two values

* Filtered patterns

  * Filter selection based on some criteria

------------------
Binding Patterns
------------------

* Sometimes we want to refer to the values in the pattern we matched

  .. code:: lkql

    select Identifier # return all identifiers

  * How do we refer to each identifier returned?

* :dfn:`Binding pattern` allows us to map the returned items to a name

  .. code:: lkql

    select id @ Identifier # return all identifiers

  * We can now pass :lkql:`id` as a reference to the identifier

---------------
Node Patterns
---------------

* Node patterns have optional parenthesized expressions

  * Used to refine the query

* **Selector Predicate**

  * Match results of a sub-query

  .. code:: lkql

    select Body (any children: ForLoopStmt)

  *Select all bodies where any child has a* :ada:`for` *statement*

  * :lkql:`any` - at least one child matches
  * :lkql:`all` - all children match

* **Field Predicate**

  * Matches if the specified field has the specified pattern

  .. code:: lkql

    select ObjectDecl(f_default_expr: IntLiteral)

  *Select all object declarations where default expression is integer literal*

* **Property Call Predicate**

  * Similar to field predicate
  * Matches a property call's result against a pattern

  .. code:: lkql

    select Identifier(p_name_matches("some_string"): true)

  *Select identifiers whose name match the identifier* :lkql:`"some_string"`

------------------------
Regular Value Patterns
------------------------

* Pattern matching can extend to any values

  * Even for composite objects

* **Tuple Pattern**

  .. code:: lkql

    match i
    # match exactly 1, 2, 3
    | (1, 2, 3) => print("One, Two, Three")
    # match where first element is 1, third element is 3
    #   then print the second element
    | (1, a@*, 3) => { print(a) }

* **List Pattern**

  .. code:: lkql

    match lst
    # match exactly 1, 2, 3
    | [1, 2, 3]   => { print ("[1, 2, 3]") }
    # match where first element is 1, third element is 3
    #   then print the second element
    | [1, a@*, 3] => { print ("[1, a@*, 3], with a = " & img(a)) }

* **Object Pattern**

  .. code:: lkql

    match obj
    | {a: 12}  => "{a: 12}"
    | {a: a@*} => "Any object with an a key. Bind the result to a"

-----------------------
Special Case Patterns
-----------------------

* **Null Pattern**

  * Match all null nodes with this pattern

  .. code:: lkql

    match node
    | BasicDecl => "A BasicDecl node"
    | null      => "Node is null!"


* **Not Pattern**

  * Use this pattern to negate another one

  .. code:: lkql

    match v
    | not BasicDecl => "Everything except a BasicDecl node"
    | *             => "A BasicDecl node"

* **Or Pattern**

  * Use this pattern to match one of a set of patterns

    * Match any value matching one of those

  .. code:: lkql

    match v
    | (BasicDecl | 1) => "A BasicDecl node or 1"
    | *               => "Any other value"

---------------------------
Flexible Pattern Matching
---------------------------

* **Wildcard Pattern**

  * Match all values with this pattern

    * Will always return true

  .. code:: lkql
    :font-size: footnotesize

    match any_val
    | BasicDecl => "A BasicDecl node"
    | *         => "Any other value"

* **Splat Pattern**

  * Used inside *List Pattern* and *Object Pattern*

    * Match all remaining values
    * Collect into list or object as appropriate

  .. code:: lkql
    :font-size: footnotesize

    match v
    | [1, rem@...]    => "A list with 1 as first element followed by " & img(rem)
    | {a: 1, rem@...} => "An object with a=1 and " & img(rem)

-------------------
Filtered Patterns
-------------------

* Pattern using an arbitrary boolean condition

  * Uses :lkql:`when` clause

  .. code:: lkql

    select BasicDecl when some_condition

* Results of query need to be named

  * If passing to another query or function

  .. code:: lkql

    select b @ Identifier when b.p_is_defining()
