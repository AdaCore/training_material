=============
Expressions
=============

----------------------------
Review: Simple Expressions
----------------------------

* Previous chapter covered some simple LKQL expressions

  * Typical programming techniques like blocks, comparisons, etc.

* This chapter covers the "next level" of expressions

  * Non-numeric literals
  * Conditionals (:lkql:`if` and :lkql:`match`)
  * List comprehension
  * Dealing with null/empty values

* This chapter will **not** cover everything!

  * Refer to the *LKQL Language Reference* section of the **GNATcheck Reference Manual** for more

-----------------
Simple Literals
-----------------

* LKQL supports the following basic "types"

  * Boolean
  * Integer
  * String
  * Unit
  * Null

* Literals for these types are (mostly) as expected

  .. code:: lkql

    val flag = true   # Boolean
    val count = 3     # Integer
    val name = "Fred" # String
    val empty = ()    # Unit
    val absent = null # Null

.. note::

  *Unit* is an **empty** result, while *null* indicates absence

------------------
Complex Literals
------------------

* Object literal

  * Defines an object and it's fields

  .. code:: lkql

    val object = {lower: "Hello", UPPER: "World"}
    print (object.lower & " " & object.upper}

  * Field names always treated as lowercase

* List literal

  * Simple representation of a list of objects

  .. code:: lkql

    val list = [1, 2, 3, 4]

* Tuple literal

  * Collection of values

  .. code:: lkql

    val tuple = (1, "Hello", true)

------------------
"if" and "match"
------------------

* :lkql:`if` expression follows one of two paths

  * Based on a boolean condition

  .. code:: lkql

    val expr_index = if is_return then 1 else 2;

  * :lkql:`if` expressions that return a boolean value do not need an :lkql:`else`

    * :lkql:`true` will be returned for the implied :lkql:`else`

* :lkql:`match` expression allows pattern-matching to determine the path

  .. code:: lkql

    match n.f_prefix.p_referenced_decl()
    | BasicSubpDecl => true
    | SubpBody      => true
    | *             => false

  .. code:: lkql

    match node
    | ParenExpr => strip(node.f_expr)
    | QualExpr  => strip(node.f_suffix)

  * Arms are evaluated in order

    * First matching arm is taken
    * If no matching arm, run-time exception is raised
    * :lkql:`*` will match anything

--------------------
List Comprehension
--------------------

* LKQL is functional, so no :ada:`for` loops

  * List comprehension allows list construction by embedding the :ada:`for`

  .. code::

    [<expression> for <identifier> in <query>]

  * Build a list of *expression* elements such that

    * *Query* returns results named *identifier*
    * *Identifier* can be used to build *expression*

  .. code:: lkql
    :font-size: scriptsize

    [item for item in subprograms if name == item.to_lower_case]

  * Build a list of subprogram nodes where the name matches some parameter

.. note::

  List comprehension actually builds a :lkql:`Stream`, not a list

  Elements of a :lkql:`Stream` are not computed until referenced

----------------
"Safe" Variant
----------------

* Many queries in LKQL can return :lkql:`null` or :lkql:`()` (called :dfn:`Unit`)

  * Items not found, empty lists, etc.
  * Similar to returning a pointer

* :lkql:`?` is used to simplify downstream dereferencing

  * Allows chaining of empty values

  .. code:: lkql
    :font-size: small

    val params = node?.f_subp_spec?.f_subp_params?.f_params;

  * :lkql:`params` will contain a list of parameters for :lkql:`node` if

    * :lkql:`node` is not null
    * :lkql:`node` is a subprogram specification
    * :lkql:`node` list of parameters is not null

.. note::

  This is also referred to as an **unwrap** expression
