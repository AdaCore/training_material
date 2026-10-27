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

  * Used to represent empty values

* ``Int``

  * Basic integer type
  * Supports arbitrary-sized values

* ``Str``

  * Built-in string type
  * Supports concatenation

* ``Bool``

  * Built-in boolean type
  * Supports expected boolean relational operators

* ``Node``

  * Corresponds to syntax nodes of queried source files

* ``Token``

  * Corresponds to lexical units of queried source files

* ``Pattern``

  * Compiled regular expressions
  * Can be used for string matching

* ``Function``

  * Designated by keyword :lkql:`fun`
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
  * Can be indexed
  * Supports concatenation

* ``Object``

  * Heterogeneous records
  * Contains any number of key-to-value mappings

* ``Stream``

  * Supports access by index
  * "Lazy" sequences of items

    * Forces computation until indexed item

.. note::

  ``Tuple``, ``List``, and ``Stream`` index starts at 1 (like Ada)

-------------------
Block Expressions
-------------------

* Group local declarations and expressions

  * Local values can be declared in the block

    * Designated by keyword :lkql:`val`

* Create a *local scope*

  .. code:: lkql

    fun maximum_lines(unit, n: int = 10000) =
        {
            val tokens = unit.tokens.to_list;
            val tok    = tokens[tokens.length];

            if tok.end_line > n
            then [{message: "too many lines: " & img(tok.end_line),
                   loc: tok}]
            else []
        }

* Value of the *block expression* comes from the :lkql:`if` branches

  * But :lkql:`tokens` and :lkql:`tok` can hold temporary values

--------------
Declarations
--------------

* Functions are callable values

  * Function with a single expression

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

  * Part of the syntax tree and are attached to the declaration

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

* Integer arithmetic

  .. code:: lkql

    val area = height * width
    val smaller_or_eq = perimeter <= 100

* Relational operators

  .. code:: lkql

    val flag = level > limit

* Boolean operators (:lkql:`and`, :lkql:`or`, :lkql:`not`)

  * :lkql:`and` and :lkql:`or` use short-circuit evaluation

  .. code:: lkql

    val test = power_on and count(something)
    # if "power_on" is false, "count" will not be called

* Concatenation

  .. code:: lkql

    "Hello " & name  # String concatenation
    [1, 2] & [5, 6]  # List concatenation

----------------------
A Word About Strings
----------------------

* Simple strings look just like strings in most languages

  .. code:: lkql

    val simple = "Hello"

* Multi-line strings are formatted differently

  * Require :lkql:`|"` and whitespace before text on each line

  .. code:: lkql

    val multi = |" Hello
                |" This is a multi-line string
                |" Goodbye!


* Multi-line string delimiters look just like *docstrings*

  * Differentiated by context

.. warning::

  Omitting the whitespace after :lkql:`|"` is
  a runtime error, **not** a parse error
    
-------------
Expressions
-------------

* *Block expressions* group computations into a single expression

  * May include local declarations
  * Final expression determines block's value

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

  * Work with ``List`` and ``Stream``

  .. code:: lkql

    12 in list

* *Is expressions* verify value matches a **pattern**

  .. code:: lkql

    val a = select AdaNode
    val b = a[1] is ObjectDecl

* :dfn:`Pattern` is a construct to match against a value

  * Can match basic or composite values
  * Can match literals (numeric, string, :lkql:`null`)
