=============
Expressions
=============

----------------------------
Review: Simple Expressions
----------------------------

* Previous module covered some simple LKQL expressions

  * Including blocks, comparisons, etc.

* This chapter covers the "next level" of expressions

  * Non-numeric literals
  * Control flow (:lkql:`if` and :lkql:`match`)
  * List comprehension
  * Dealing with null/empty values

-----------------
Simple Literals
-----------------

* LKQL supports the following basic *types*

  * Boolean
  * Integer
  * String
  * Unit

* Literals are available for these types

  .. code:: lkql

    val flag = true   # Boolean
    val count = 3     # Integer
    val name = "Fred" # String
    val empty = ()    # Unit

.. note::

  *Unit* is an **empty** result - use :lkql:`null` when value doesn't exist

--------------------
Composite Literals
--------------------

* Object literal

  * Object with named fields

  .. code:: lkql

    val object = {lower: "Hello", UPPER: "World"}
    print (object.lower & " " & object.upper)

  * Field names are case-insensitive

* List literal

  * Contiguous list of values

  .. code:: lkql

    val list = [1, 2, 3, 4]

* Tuple literal

  * Heterogeneous group of values

  .. code:: lkql

    val tuple = (1, "Hello", true)

------------------
"if" and "match"
------------------

* :lkql:`if` expression follows one of two paths

  * Based on a boolean condition

  .. code:: lkql

    val expr_index = if is_return then 1 else 2;

  * Boolean :lkql:`if` expressions do not require an :lkql:`else`

    * If condition is false and no :lkql:`else` expression, :lkql:`true` is the value

* :lkql:`match` expression allows pattern-matching to determine the path

  .. code:: lkql

    match n.f_prefix.p_referenced_decl()
    | BasicSubpDecl => true
    | SubpBody      => true
    | *             => false

  .. code:: lkql

    match node
    | ParenExpr => node.f_expr
    | QualExpr  => node.f_suffix

  * Arms are evaluated in order

    * First matching arm is taken
    * If no matching arm, run-time exception is raised
    * :lkql:`*` is an optional catch-all pattern; place it last

--------------------
List Comprehension
--------------------

* LKQL is functional, so no :ada:`for` loops

* List comprehension allows construction by embedding the :ada:`for`

  .. code::

    [<element_expression> for <identifier> in <query_expression>]

  * Build a sequence of *element_expression* elements such that

    * *Query_expression* returns results named *identifier*
    * *Identifier* can be used to build *element_expression*

  .. code:: lkql
    :font-size: scriptsize

    [item for item in subprograms if name == item.to_lower_case]

  * For every :lkql:`item` in the list of subprograms

    * If :lkql:`name` matches the item (the subprogram name)
    * Then add :lkql:`item` to the list being returned

.. note::

  * List comprehension actually builds a :lkql:`Stream`, not a list
  * Elements of a :lkql:`Stream` are not computed until referenced

----------------
"Safe" Variant
----------------

* Queries in LKQL can return mutliple literals indicating no result

  * **()** - no meaningful value ("Unit")
  * **[]** - list that has no values (empty list)
  * **null** - no node present

* :lkql:`?.` is used to simplify accessing past a :lkql:`null` value

  * Allows chaining of empty values

  .. code:: lkql
    :font-size: small

    val params = node?.f_subp_params?.f_params;

  * :lkql:`params` will contain a :lkql:`ParamSpecList` if

    * :lkql:`node` is not null
    * :lkql:`node` is a subprogram specification
    * :lkql:`node` has at least one parameter

.. note::

  :lkql:`?.` is also called *safe field access*

-------------------
Unwrap Expression
-------------------

* Safe field access returns a *nullable value*

  * Either :lkql:`null` or an object

* Use the *unwrap expression* :lkql:`!!` to require a non-null value

  * Typically used at the end of a chain of safe field accesses

  .. code:: lkql
    :font-size: small

    object_decl?.p_type_expression()?.p_designated_type_decl()!!

.. warning::

  If the chain returns :lkql:`null`, the unwrap will raise an error
