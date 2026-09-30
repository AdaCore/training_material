====================
Selectors In-Depth
====================

-----------------------------
Review: What Is a Selector?
-----------------------------

* Special form of functions

  * Recursive :lkql:`match` statement

    * Match expression treats current node as :lkql:`this`

  * Return a :lkql:`Stream` of values
  * Used in the query subset of LKQL

    * Allow easy expression of traversal blueprints

* Example: built-in selector :lkql:`children`

  *  Explores the tree node by node

* Selector declarations do not have parameters

  * But selector calls can specify, :lkql:`min_depth`, :lkql:`max_depth`, and :lkql:`depth`

.. code:: lkql

  # Calling a selectors directly
  val descendants = children(node, depth=3)
  # descendants has all children at dept of 3

  # Calling a selector in a nested sub-pattern
  select AdaNode(any children(min_depth=3): BasicDecl)
  # find all BasicDecl nodes at depth lower than 3

---------------------
Defining a Selector
---------------------

* Each arm of a selector returns a :lkql:`RecExpr` (recursive expression)

  * Builds two lists

    * :dfn:`Recursion list` is a list of items to be traversed next
    * :dfn:`Return list` is a list of items to be returned to the user
    * Can add an item or a list of items (prefixed by :lkql:`*`) to a list

  * Selector returns a :lkql:`Stream`

* :lkql:`rec` operation creates the recursive expression

  * First parameter represents what is added to recursion list
  * Second parameter represents what is added to return list

    * If not specified, first parameter is added to both lists

* Return all defining names of a :lkql:`DefiningName`

  .. code:: lkql

    selector defining_names
        | BasicDecl => rec(*[d for d in this.p_defining_names() if d != null])
        | *         => ()

  * If the node is a basic declaration

    * Build a list of all defining names for :lkql:`list` that are not null
    * Add each item in that list to the recursion and return lists

  * Otherwise, stop recursing (return an empty Unit)
