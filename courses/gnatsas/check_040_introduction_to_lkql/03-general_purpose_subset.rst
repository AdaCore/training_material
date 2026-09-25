========================
General Purpose Subset
========================

--------------------------------------
General Purpose Programming Language
--------------------------------------

* Composed of reduced set of declarations and expressions
* Minimal (but Turing-complete)
* Currently no side effects

  * Because LKQL is designed to just express queries

------------------
Basic Data Types
------------------

* ``Unit``

  * Used to represents empty values

* ``Int``

  * Basic integer type
  * Supports arbitrary sized values

* ``Str``

  * Built-in string type
  * Supports concatenation

* ``Bool``

  * Built-in boolean type
  * Supports expected boolean relational operators

* ``Node``

  * Correspond to syntax nodes of queried source files

* ``Token``

  * Correspond to lexical units of queried source files

* ``Pattern``

  * Compiled regular expressions
  * Can be used to match a string against

* ``Function``

  * LKQL functions are first class citizens
  * Represents values that can be called with a call expression

----------------------
Composite Data Types
----------------------

* ``Tuple``

  * Heterogeneous groups of values with a fixed size
  * Can be indexed to access inner values

* ``List``

  * Contiguous immutable sequences of items
  * Can be indexed; also support concatenation

* ``Object``

  * Heterogeneous records
  * Contain any number of key-to-value mappings

* ``Stream``

  * "Lazy" sequences of items

    * Element is not computed until it is observed

  * Can be indexed

    * Forces computation until indexed item

.. note::

  ``Tuple``, ``List``, and ``Stream`` start index at 1 (like Ada) rather than 0 (like C)

--------------
Declarations
--------------

* Functions are callable expressions

  * Simple function (no local variables)

    .. code:: lkql

      fun add(x, y) = x + y

  * Function with block expression

    .. code:: lkql

      fun add(x, y) = {
          |" Add two integers
          val ret = x + y;
          ret
      }

* Value declarations are used for named values

  .. code:: lkql

    val a = 12 + 15

  * Values are immutable

* Docstrings are used for comments

  .. code:: lkql

    |" Function that will add 12 to its first argument
    val adder = make_closure(12)

  * Part of the AST and are attached to the declaration

------------------------
Literals and Operators
------------------------

* Simple literals for booleans, integers, strings, unit, and null

  .. code:: lkql

    val a = true     # Boolean
    val b = 12       # Integer
    val c = "hello"  # String
    val d = ()       # Unit
    val e = null     # Null

* Built-in operators

  * Integer arithmetic

    .. code:: lkql

      val calc = a + 2 * 3 / 4
      val smaller_or_eq = a <= b

  * Boolean relational operators

    .. code:: lkql

      true and false or (a == b) and (not c)

  * Concatenation

    .. code:: lkql

      "Hello " & name  # Strings concatenation
      [1, 2] & [5, 6]  # Lists concatenation

* Multi-line string literals

  .. code:: lkql

    val a = |" Hello
            |" This is a multi line string
            |" Bue

  * First character after ``"`` should be whitespace

    * Not a parse error but will fail at run-time
    
-------------
Expressions
-------------

* *Block expressions* used to declare temporary values

  .. code:: lkql

    {
        val x = 40;
        val y = 2;
        print("DEBUG : " & (x + y).img);
        x + y
    }

* *Field access* returns contents of a field

  .. code:: lkql

    object_decl.f_type_expr

  * Get ``f_type_expr`` field from ``object_decl`` node

* *Call expressions* allow functions to call other functions

  .. code:: lkql

    fun add(a, b) = a + b

    val c = add(12, 15)
    val d = add(a=12, b=15)

  * Parameters use either positional or named notation

* *Indexing expressions* allow access to elements of composite data types

  .. code:: lkql

    (1, 2, 3)[1] # Indexing a tuple
    list[1]      # Indexing a list
    node[x]      # Indexing a node

  * List and tuple nodes use list-based indexing
  * Regular nodes use lexical ordering

------------------------
Comparison Expressions
------------------------

* Typical comparison operators are available

  .. code:: lkql

    12 < 15
    a == b
    b != c

  * Order-dependent operators (``<``, ``>=``, etc.) only work on integers


* *Membership expressions* verify value is in the collection

  * Works with ``List`` and ``Stream``

  .. code:: lkql

    12 in list

* *Is expressions* verify value matches a pattern

  .. code:: lkql

    val a = select AdaNode
    val b = a[1] is ObjectDecl






    

