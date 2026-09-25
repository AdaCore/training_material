==========
Overview
==========

------------
Libadalang
------------

* Libadalang (LAL) - for parsing and semantic analysis of Ada code

  * Building block for integration into other tools

    * IDE, static analyzers, etc.

* Provides

  * Complete syntactic analysis with error recovery

    * Precise syntax tree when source is correct, OR
    * Best effort tree when source is incorrect

  * Semantic queries on top of syntactic tree such as

    * Resolution of references (what a reference corresponds to)
    * Resolution of types (what is the type of an expression)
    * General cross references queries (find all references to this entity)

-------------------------------
LangKit Query Language (LKQL)
-------------------------------

* LKQL is a functional language enabling source code queries

  * Based on `langkit <https://github.com/AdaCore/langkit>`_ technology
  * Currently hardwired for Ada (and LAL)

* Purely functional, high level, dynamically typed language 

  * General purpose and tree query subsets

* Designed to be simple and concise
* Has a `reference manual <https://docs.adacore.com/live/wave/lkql/html/gnatcheck_rm/gnatcheck_rm/lkql_language_reference.html>`_

-----------
Why LKQL?
-----------

* :toolname:`GNATcheck` uses LKQL to specify the rules it verifies

  * "Predefined" rules are just a text library of LKQL rules

  :command:`gnatcheck -P default.gpr --rule renamings`

  :filename:`<SAS installation>/share/lkql/renamings.lkql`

  .. code:: lkql

    @check(message="renaming declaration", category="Feature")
    fun renamings(node) =
        |" Flag renaming declarations.
        |"
        |" .. rubric:: Example
        |"
        |" .. code-block:: ada
        |"    :emphasize-lines: 2
        |"
        |"    I : Integer;
        |"    J : Integer renames I;     --  FLAG
        node is RenamingClause

-------------------
LKQL Construction
-------------------

* LKQL made up of two language subsets

* General Purpose Subset

  * Dynamically typed
  * Functional
  * Made up of

    * Function definitions
    * Common expressions
    * Basic support for numeric types, list comprehension, etc.

* Tree Query Language

  * Allows expression of concise predicates over tree nodes and their relatives
  * Contains tree traversal logistics
