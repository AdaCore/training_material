============
Unit Rules
============

----------------------
What Is a Unit Rule?
----------------------

* Previously our rules worked on nodes

  .. code:: lkql

    @check
    fun abort_statements(node) =
        node is AbortStmt
  
  * Return :lkql:`true` if the node is an :ada:`abort` statement

* But some rules need more context

  * Check if number of lines in file exceeds some maximum
  * Flag any file containing end-of-line comments

* Both of these rules could be done as simple rules

  * But would generate messages for **every** line that matches

* We need rules to check if the **unit** fails, not the **node**
  
-----------------
Basic Unit Rule
-----------------

* Unit rules start with :lkql:`@unit_check` rather than :lkql:`@check`

* Major differences between unit rules and boolean rules

  * Unit Rules operate on analysis units instead of token nodes
  * Unit Rules return a message list rather than a boolean value

    * List can flag multiple issues in a single unit

* Simple rules trigger once per unit

  * Unit length cannot exceed some maximum number of lines
  * Unit must start with some predefined text

* Other rules trigger multiple times per unit

  * Subprogram length cannot exceed some maximum number of lines
  * Use of disallowed aspects

----------------------------------
Expanded Messages for Unit Rules
----------------------------------

* In a boolean rule, the default message could be replaced with a text explanation

  .. code:: lkql

    @check(message="Integer object found")
    fun integer_object(node) =

* In a unit rule, messages must be in a list

  * So they are defined in the body of the rule

  .. code:: lkql

    {message: "mark BEGIN with -- " & n.parent.p_defining_name().text,
     loc: n.token_start()}

  * **message** is what is displayed to the user

    * Can use concatenation to add specific information to the message

  * **loc** is the token that triggered the rule

    * Used to display where in the code that rule was triggered

  .. code:: error
    :font-size: footnotesize

    main.adb:6:07: rule violation: mark BEGIN with -- Proc_One
    main.adb:17:07: rule violation: mark BEGIN with -- Proc_Three

------------------
Simple Unit Rule
------------------

* Some rules need to flag a unit only one time

  * E.g. unit length rule

    * The violation should appear for the unit
    * Not for every node past the end of the line

* The rule still needs to return a **list** of messages

  .. code:: lkql

    @unit_check
    fun maximum_lines(unit, n: int = 10000) =
        {
            val tokens = unit.tokens.to_list;
            val tok    = tokens[tokens.length];

            if tok.end_line > n
            # Return a message as the only element of a list
            then [{message: "too many lines: " & img(tok.end_line), loc: tok}]
            # Return an empty list
            else []
        }

  * Flag unit if last token ends on a line greater than passed-in value

-----------------------------------
Rules That Trigger Multiple Times
-----------------------------------

* LKQL does not have *for* loops

* Rules can trigger multiple times (e.g. subprogram length exceeded)

  * **List comprehension** allows rule to build list of multiple messages

  .. code:: lkql

    import stdlib
    @unit_check
    fun end_of_line_comments(unit) =
        [
            {message: "end of line comment", loc: tok}
            for tok in unit.tokens
            if tok.kind == "comment" and
               stdlib.previous_non_blank_token_line(tok) == tok.start_line
        ]

* Build a list of messages such by looping through the unit tokens

  * If a token is a comment and there is code on the same line

    * Add message "end of line comment" for the current token location

.. code:: ada
  :number-lines: 6

   -- Regular comment
   One   : Integer_T := 11;     -- end of line comment
   Two   : Number_T  := 222;
   Three : Float_T   := 3.3e-3; -- end of line comment
   -- Regular comment

.. code:: error

  main.adb:7:33: rule violation: end of line comment
  main.adb:9:33: rule violation: end of line comment

-------------------
Support Functions
-------------------

* Good coding practice means using "helper" functions

  * Simplify complex expressions
  * Replace duplicated code

* LKQL allows support functions

  * Can return anything (boolean, integer, list, etc.)
  * No decorators needed

.. code:: lkql

  fun count_lines(node) =
      node.token_end().end_line - node.token_start().start_line + 1

  @unit_check(help="maximum number of lines in a subprogram",
              category="Style", subcategory="Program Structure")
  fun maximum_subprogram_lines(unit, n: int = 1000) =
      [
          {message: "too many lines in subprogram body: " & img(count_lines(n)),
           loc: n.token_start().previous(exclude_trivia=true)}
          for n in from unit.root
          select node@HandledStmts(parent: SubpBody) when count_lines(node) > n
      ]

* Could rewrite the query as

  .. code:: lkql

    select node@HandledStmts(parent: SubpBody)
      when n <
           node.token_end().end_line - node.token_start().start_line + 1

  * But it would need to be repeated in the message
  * And the query reads better with a function call

----------------------
"Memoized" Functions
----------------------

.. code:: lkql

    @memoized
    fun num_primitives(t) = t.p_get_primitives().length

    @unit_check
    fun too_many_primitives(unit, n : int = 5) =
        [
            {message: "tagged type has too many primitives (" &
                      img(num_primitives(n)) & ")",
             loc: n.p_defining_name()}
            for n in from unit.root through follow_generics
            select node@TypeDecl
            when node.p_is_tagged_type() and node.parent.parent is PublicPart
             and num_primitives(node) > n
        ]

* :lkql:`num_primitives` is called multiple times

  * Display information for the message
  * Part of the query for determining too many primitives

* Performing its own query (:lkql:`p_get_primitives`) can get expensive

* But functions cannot have side-effects (no global data)

  * So if called with the same parameter, results are the same

* :lkql:`@memoized` tells analyzer to cache the result

  * And return the value from the cache when called after the first time
