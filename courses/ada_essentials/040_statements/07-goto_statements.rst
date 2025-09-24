=================
GOTO Statements
=================

-----------------
GOTO Statements
-----------------

**Syntax**

.. container:: source_include 040_statements/syntax.bnf :start-after:goto_statements_begin :end-before:goto_statements_end :code:bnf

* Rationale

   - Historic usage
   - Arguably cleaner for some situations

* Restrictions

   - Based on common sense
   - Example: cannot jump into a `case` statement

--------
GOTO Use
--------

* Mostly discouraged
* May simplify control flow
* For example in-loop `continue` construct

.. code:: Ada

   loop
      -- lots of code
      ...
      goto continue;
      -- lots more code
      ...
      <<continue>>
   end loop;

* As always maintainability beats hard set rules

