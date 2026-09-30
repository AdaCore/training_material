======================
Expressions In-Depth
======================

-----------------------
Fields and Properties
-----------------------

* Objects created by users have fields

  * Referencing fields uses "dot-notation"

  .. code:: lkql

    val status = {msg: "Hello", flag: true}
    print (stauts.msg)

* Objects returned from the LKQL API have both fields and properties

  * Still referenced via "dot-notation"
  * Fields are prefaced with :lkql:`f_` and properties with :lkql:`p_`

    * Properties are **function calls** which may or may not take parameters

  .. code:: lkql

    node is ExprFunction
    when node.p_semantic_parent() is BasePackageDecl or
         (node.p_semantic_parent() is PrivatePart)

  .. code:: lkql

    node is AttributeRef
    when node.f_attribute?.p_name_is("Size")

------------------
Call Expressions
------------------

* Regular call expression

  .. code:: lkql

    fun add(a, b) = a + b

    val c = add(12, 15)
    val d = add(a=12, b=15)

* "Safe" variant

  * Returns :lkql:`()` (Unit) if what is being called is null

  .. code:: lkql

    fun add(a, b) = a + b
    val fn = if true then null else add
    fn?(1, 2) # Returns ()

  * Notice that :lkql:`fn` is basically a function pointer

------------------
Constructor Call
------------------

* Sometimes it is necessary to build a node

  * Typically to pass some construct to a function requiring a node

* Use the :lkql:`new` operator

  .. code:: lkql
    :font-size: small

    fun reduce_op(bin_op) =
        match bin_op.f_op
        | OpDiv                     => new IntLiteral("1")
        | (OpMinus | OpMod | OpRem) => new IntLiteral("0")
        | (OpEq | OpGte | OpLte)    => new Identifier("True")
        | (OpNeq | OpGt | OpLt)     => new Identifier("False")

  * Returns either an integer literal or an identifier

    * Different paths can return different node types
    * As long as caller expects it

-------------------
Importing Modules
-------------------

* Every rules file is considered a :dfn:`module`

* Importing modules allows programmer to keep files of common routines

  * Following modules are part of :toolname:`GNATcheck`

    * **control_flow** - perform control flow analysis
    * **metrics** - code complexity metrics
    * **parameters_aliasing** - determine parameter aliasing
    * **stdlib** - commonly used processing

* To import a module, include :lkql:`import <module>` at the top of the file

* Function calls use dot-notation

.. code:: lkql

  import stdlib

  @check
  fun exception_propagation_from_tasks(node) =
      node is TaskBody when stdlib.propagate_exceptions(node)
