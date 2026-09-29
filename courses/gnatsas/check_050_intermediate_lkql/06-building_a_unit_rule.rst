======================
Building a Unit Rule
======================

-------------------------------
Example of a Simple Unit Rule
-------------------------------

* We want a rule to find complex expressions

  * As defined by "expressions with more than N subexpressions"

    * In our case, **N** will be 2

.. code:: Ada
  :number-lines: 5

  Value := 1 + 2 + 3;
  Value := Value * 4;
  Do_Something (5 + 6, 7, Value * 8 / 9);

.. code:: error
  :font-size: scriptsize

  main.adb:5:14: rule violation: expression has too many sub-expressions (3)
  main.adb:7:05: rule violation: expression has too many sub-expressions (6)
  main.adb:7:25: rule violation: expression has too many sub-expressions (3)

* Line 5 has 3 subexpressions (:ada:`1`, :ada:`2`, :ada:`3`)
* Line 6 has 2 subexpressions (:ada:`Value`, :ada:`4`)
* Line 7 has 3 subexpressions (:ada:`5 + 6`, :ada:`7`, :ada:`Value * 8 / 9`)

  * And one subexpression also has 3 subexpressions (:ada:`Value`, :ada:`8`, :ada:`9`)

.. note::

  This rule already exists in :toolname:`GNATcheck` as :lkql:`maximum_subprogram_lines`

----------------------------
Actual Rule Implementation
----------------------------

.. code:: lkql

  fun num_expr(node) =
      (from node select ((e@SingleTokNode when e is not Op) | CondExpr |
                         QuantifiedExpr | BaseAggregate | TargetName)).length

  @unit_check(help="maximum complexity of an expression",
              category="Style", subcategory="Program Structure")
  fun maximum_expression_complexity(unit, n: int = 10) =
      [
          {message: "expression has too many sub-expressions (" &
                    img(num_expr(node)) & ")", loc: node}
          for node in from unit.root select
          expr@Expr(parent: not Expr)
          when expr is not (Identifier | DefiningName | EndName)
           and num_expr(expr) > n
      ]

.. note::

  The actual rule file contains **docstrings** for describing the functions

----------------------------
Examining the Rule - Query
----------------------------

.. code:: lkql

  for node in from unit.root select

*Loop over all nodes in the unit*

.. code:: lkql

  expr@Expr(parent: not Expr)

*Find expressions who are not part of another expression and name it* :lkql:`expr`

.. code:: lkql

  when expr is not (Identifier | DefiningName | EndName)
            and num_expr(expr) > n

*Trigger the rule when* :lkql:`expr` *is not a name and number of expressions is greater than* **N**

---------------------------------------
Examining the Rule - Support Function
---------------------------------------

.. code:: lkql

  fun num_expr(node) =
      (from node select (
          (e@SingleTokNode when e is not Op) |
          CondExpr |
          QuantifiedExpr |
          BaseAggregate |
          TargetName)).length

* Build a list of nodes such that each element is one of

  * Single token (unless the token is an operator)
  * Conditional expression
  * Quantified expression
  * Aggregate
  * :ada:`@` (Ada 2022 "target" symbol)

* Return the length of the list

--------------------------------
Examining the Rule - Decorator
--------------------------------

.. code:: lkql
  :font-size: small

  @unit_check(help="maximum number of lines in a subprogram",
              category="Style", subcategory="Program Structure")

* **help** - text used for getting help on a rule
* **category** - text used to distinguish what the rule is used for
* **subcategory** - text used to further classify the rule usage

