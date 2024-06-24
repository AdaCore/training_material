************************
Writing Your Own Rules
************************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

------------
Libadalang
------------

* Libadalang (LAL) - library for parsing and semantic analysis of Ada code

  * Meant as building block for integration into other tools (IDE, static analyzers, etc.)

* Provides mainly

  * Complete syntactic analysis with error recovery

    * Precise syntax tree when source is correct, OR
    * Best effort tree when the source is incorrect

  * Semantic queries on top of the syntactic tree such as

    * Resolution of references (what a reference corresponds to)
    * Resolution of types (what is the type of an expression)
    * General cross references queries (find all references to this entity)

------------------------
LangKit Query Language
------------------------

* LKQL (LangKit Query Language) - query language enabling users to run queries on top of source code

  * Based on `langkit <https://github.com/AdaCore/langkit>`_ technology
  * Currently hardwired for Ada (and LAL)

* Purely functional, high level, dynamically typed language with general purpose and tree query subsets
* Designed to be simple and concise
* Has a `reference manual <https://docs.adacore.com/live/wave/lkql/html/gnatcheck_rm/gnatcheck_rm/lkql_language_reference.html>`_

Having :toolname:`GNATcheck` rules expressed with a high level interpreted language such as LKQL allows users to write their own rules and test them quickly

======
LKQL
======

----------------------------------------
LKQL features (general purpose subset)
----------------------------------------

* You can easily define a function in LKQL

  * All functions are first class citizens

    .. code:: graphql

       fun add(x, y) = x * y
       fun sub(x, y) = x - y
       fun apply(f, x, y) = f(x, y)

       print(apply(add, 40, 2))
       print(apply((x, y) => x * y), 40, 2)

  * You can also define anonymous functions

* LKQL supports list comprehensions with the same syntax as Python

  .. code:: graphql

     val odds = [num for num in [1, 2, 3, 4, 5] if is_odd(num)]
     val ids = [node for node in nodes if node is Identifier]

* You can use LKQL block expressions to declare local values and add some sequentiality

  .. code:: graphql

     val complex = {
        val part = 40;
        val other_part = 2;
        print("LOGGING");
        part * other_part
     }

------------------------------
LKQL features (query subset)
------------------------------

* LKQL allows you to write queries to fetch all nodes which satisfy a given pattern

  * LKQL also provides selector operations (e.g. **any children**)

  .. code:: graphql

     val ids = from nodes select Identifier
     val if_id_child = select IfStmt(any children is Identifier)

* You can define a selector to express a tree traversal logic and use it later as a function or in a pattern

  * This will yield every child but will not recurse for the **if statement** children:

    .. code:: graphql

       selector children_until_if
       | IfStmt  => this
       | AdaNode => rec *this.children
       | *       => ()

* LKQL patterns use the LAL API to express any filtering logic in a simple and expressive way

  * For more information see the LKQL reference manual

  .. code:: graphql

     val test = select b@BinOp(f_op is OpEq)
                when b.f_left.text == "0" and
                     b.f_right is Identifier

---------------------
LKQL API References
---------------------

* LKQL API can be found at :url:`https://docs.adacore.com/live/wave/lkql/html/gnatcheck_rm/gnatcheck_rm/lkql_language_reference.html#lkql-api`

* Contains sections on

  * Libadalang API (found at :url:`https://docs.adacore.com/live/wave/libadalang/html/libadalang_ug/python_api_ref.html`)

    * Includes definitions of all functions like ``IfStmt`` that we saw above

  * Standard library

    * Includes definitions of typical functions like ``print`` and ``children``

----------------------------
Testing LKQL with its REPL
----------------------------

* LKQL has an interactive REPL *(Read-Eval-Print-Loop)*

  * Test your ideas and explore available properties and node kinds with auto-completion

* Start the LKQL REPL on a project named :filename:`example.gpr` by running the Python script :command:`lkql_repl.py`

  :command:`lkql_repl.py -P example.gpr`

* Then you can run any LKQL expression or declaration and immediately see the result

  .. container:: latex_environment scriptsize

    ::

      > select AdaNode  # Get the list of all Ada nodes in your project
      [...]
      > val ids = select Identifiers  # Assign "ids" value
      ()
      > fun test(nodes) = [n for n in nodes if n.text = "Hello"]  # Define a function
      ()
      > test(ids)  # Call previously defined function with the previously assigned value
      [...]

--------------------------------
Mapping Python API to LKQL API
--------------------------------

* Can also refer to `Libadalang Python API Reference <https://docs.adacore.com/live/wave/libadalang/html/libadalang_ug/python_api_ref.html>`_

* For example, we can find in the Python API documentation:

   | *class* **libadalang.Expr**:
   |    subclass of AdaNode
   |    Base class for expressions
   |
   | ...
   |   
   |    property  **p_expression_type**:
   |       Return the declaration corresponding to the type
   |       of this expression after name resolution.

* Thus we know that LKQL has a :python:`Expr` node kind and we can call the :python:`p_expression_type` on this kind of node

  * So we can do

    .. container:: latex_environment small

       .. code:: graphql

          val expr_types = [node.p_expression_type() for node in select Expr]

In the future LKQL will have its own LAL API documentation.

-------------------------------
Integrating LKQL in GNATcheck
-------------------------------

* :toolname:`GNATcheck` embeds an LKQL engine to execute rules semantics
* All :toolname:`GNATcheck` rules are expressed using LKQL
* You can make a custom rule written in :filename:`my_rules/custom_rule.lkql` available to :toolname:`GNATcheck` with a command line option

  :command:`--rules-dir=my_rules`

  * Option will trigger the loading of all :filename:`.lkql` files in the provided directory
  * Makes their associated rules available

* Example of a :toolname:`GNATcheck` call to load rules inside the :filename:`my_rules` folder and apply the :lkql:`custom_rule` rule

  .. container:: latex_environment small

    :command:`gnatcheck -P prj.gpr --rules-dir=my_rules/ -rules +Rcustom_rule`

=======
Rules
=======

---------------
Boolean Rules
---------------

* Defined by function which takes a :dfn:`node` as first parameter
* Returns a *boolean* indicating if given node should be flagged by :toolname:`GNATcheck`
* Called on every node of LAL AST

* To define custom *boolean* rule

  * Create an LKQL function annotated with :lkql:`@check`
  * Function name should be same as LKQL file name
  * Custom boolean rule which flags every :lkql:`BodyNode` in Ada sources

    * Function should be in :filename:`bodies.lkql`

    .. code:: graphql

       @check
       fun bodies(node) = node is BodyNode


--------------------------
Example of Boolean Rules
--------------------------

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      * Flag every :ada:`goto` and :ada:`if` statemnt

        .. code:: graphql

           @check
           fun goto_and_if(node) =
              match node
              | GotoStmt => true
              | IfStmt   => true
              | *        => false

      * Flag every :lkql:`Identifier` called :ada:`dummy` (case-insensitive)

        .. code:: graphql

           @check
           fun dummy_id(node) =
              node is id@Identifier
              when id.p_name_is("dummy")

      * Flags every *Binary Operator* with any child a *Numeric Literal*

        .. code:: graphql

           @check
           fun op_with_num(node) =
              node is BinOp(any children
                            is NumLiteral)

  .. container:: column

    .. container:: latex_environment tiny

       .. code:: Ada
          :number-lines: 1

          procedure Test is
             My_Int : Integer := 10 * 5;
             Dummy  : String  := "Hello World!";
          begin
             if My_Int = 15 then
                Put_Line (Dummy);
             else
                Goto label;
             end if;
             <<label>>
          end Test;

       Running :toolname:`GNATcheck` with these rules on this Ada source will produce:

          ::

             test.adb:02:24: op_with_num
             test.adb:03:04: dummy_id
             test.adb:05:04: goto_and_if
             test.adb:05:07: op_with_num
             test.adb:06:17: dummy_id
             test.adb:08:07: goto_and_if

------------
Unit Rules
------------

* Defined by function which takes an :dfn:`analysis unit` as its first parameter
* Return list of LKQL objects containing message and location
* Called on every LAL analysis unit
* Meant to be more flexible than boolean rules

  * Fulfill needs that the latter cannot express
  * Example: emitting multiple messages for the same node

* To create custom *unit* rule 

  * Create an LKQL function annotated with **@unit_check**
  * Function name should be the same as the LKQL file name (same as **@check**)

-----------------------
Example of Unit Rules
-----------------------

*Flag every* :ada:`goto` *statement and give target label line in associated message*

   .. code:: graphql

      @unit_check
      fun goto_line(unit) = [
         {message: "go to line " &
                   img(node.f_label_name
                           .p_referenced_decl()
                           .token_start()
                           .start_line),
          loc: node}
         for node in (from unit.root select GotoStmt)
      ]

.. code:: Ada
   :number-lines: 1

   procedure Test is
   begin
      <<start>>
      goto label;
      <<label>>

      goto start;
   end Test;

*Running* :toolname:`GNATcheck` *with this rule will produce:*

  ::

    test.adb:04:04: go to line 5 [goto_line]
    test.adb:07:04: go to line 3 [goto_line]

----------------
Rule Arguments
----------------

You configure an LKQL rule behavior with annotation arguments

  .. list-table::

    * - **message**
      
      - Message of the rule

    * -

      - *(boolean rules only)*

    * - **help**
      
      - Help message for the rule usage

    * - **follow_generic_instantiations**
      
      - Whether to follow generic instantiations in Ada sources

    * -

      - *(boolean rules only)*

    * - **category / subcategory**
      
      - Category and subcategory of a rule

    * - **remediation**
      
      - Mediation complexity for technical debt computation

*Example of an LKQL rule with rule arguments*

   .. code:: graphql

      @check(
         message: "There is a body node",
         help: "This rule flags all body nodes",
         follow_generic_instantiations: false,
         remediation: "EASY"
      )
      fun bodies(node) = node is BodyNode

--------------------------
Rule Function Parameters
--------------------------

* LKQL rule (*boolean* or *unit*) is defined by a function
* Rule function can have more than one parameter

  * Allows :toolname:`GNATcheck` rule arguments being forwarded

* Rule function parameter must have a default value

  * In case none is provided

* You can configure the rule below with the **threshold** argument when running it with :toolname:`GNATcheck`:

   * Flag all *Identifier* nodes with too many characters according to given threshold

   .. code:: graphql

      @check
      fun too_long_id(node, threshold=15) =
         node is Identifier
         when node.text.length >= threshold

* Flag all *Identifier* nodes with more than 42 characters using :lkql:`too_long_id` rule

   :command:`gnatcheck -P prj.gpr --rules-dir=. -rules +Rtoo_long_id:42`

---------------------------------------
Configuring a GNATcheck Run with LKQL
---------------------------------------

* You can configure :toolname:`GNATcheck` run with an LKQL file

  * Chooses rules you want to run (with arguments)
  * Possible alias 
  * Whether to run them on Ada code, SPARK code or both

* Example LKQL configuration file

  .. code:: graphql

     val rules = @{
        identifier_suffixes: [
           {access_suffix: "_PTR",
            type_suffix: "_T",
            constant_suffix: "_C",
            interrupt_suffix: "_Hdl"},
           {access_suffix: "_A",
            alias_name: "other_convention"}
        ]
     }
     val ada_rules = @{ goto_statements }
     val spark_rules = @{ recursive_subprograms }

* Example :toolname:`GNATcheck` call configured via :filename:`config.lkql`

   :command:`gnatcheck -P prj.gpr -rules -from-lkql=config.lkql`

=====
Lab
=====

.. include:: labs/check_040_lkql.lab.rst

=========
Summary
=========

---------------------------
Future evolutions of LKQL
---------------------------

* Adding a custom LAL API documentation for LKQL (for now user can rely on the LAL Python API documentation )
* Support of the LAL rewriting API to express code transformation
* Adding a static type system to improve performance and debugging processes
* Making LKQL available for all Langkit defined languages
