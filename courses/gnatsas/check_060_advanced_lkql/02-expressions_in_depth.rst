======================
Expressions In-Depth
======================

-----------------------
Fields and Properties
-----------------------

* User-created objects have fields

  * Access fields using "dot-notation"

  .. code:: lkql

    val status = {msg: "Hello", flag: true}
    print (status.msg)

* LKQL API exposes fields and properties of syntax tree nodes

  * Still referenced via "dot-notation"
  * Fields are prefaced with :lkql:`f_`
  * Properties are prefaced with :lkql:`p_`

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

  .. code:: lkql

    fun add(a, b) = a + b
    val fn = if true then null else add
    fn?(1, 2) # Returns ()

  * Returns :lkql:`()` (Unit) if :lkql:`fn` is :lkql:`null`
  * Notice that :lkql:`fn` is just a nullable value

----------------------------
Traversing the Syntax Tree
----------------------------

* Default behavior for traversal is going through children

  .. code:: lkql

    for body in from unit.root select subp@BaseSubpBody
    # Return all children of root that are subprogram bodies

* Can use :lkql:`through` to indicate other traversals

  * Most common is :lkql:`parent`

    .. code:: lkql

      from node through parent select first TypeDecl 
      # traverse all ancestors of "node"; stop at first TypeDecl

  * Also can use

    * :lkql:`prev_siblings` - sibling nodes before current node
    * :lkql:`next_siblings` - sibling nodes after current node
    * :lkql:`super_types` - if current node is a type, then all parent types

.. note::

  :lkql:`follow_generics` is a special case indicating traversal should
  follow instantiated generic trees

-------------------
Importing Modules
-------------------

* Every rules file is considered a :dfn:`module`

* Import modules to use functions and values defined in other rules

* Modules that are part of :toolname:`GNATcheck`

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
