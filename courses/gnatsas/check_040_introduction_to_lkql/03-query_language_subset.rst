=======================
Query Language Subset
=======================

----------------
Query Language
----------------

* Composed of three language constructs

* **Patterns**

  * Allow user to express filtering logic on trees and graphs
  * Similar to regular expressions for strings

* **Queries**

  * Select object declarations based on certain criteria

* **Selectors**

  * Mechanism to perform traversal on the node graph
  * Tree traversal

    * E.g. find the parent of a node

  * Non-syntactic exploration

    * E.g. go from a reference to its declaration

    * Traverse tree

------------------
Query Expression
------------------

* Simplest part of the query subset

  * Complexity is added via *patterns* and *selectors*

* Simple query

  .. code:: graphql

    # Select all non-null nodes
    select AdaNode

  * Root of query is implicit

    * Set by context

* Specify query root via ``from``

  .. code:: graphql

    # Select all non-null nodes starting from node theNode
    from theNode select AdaNode

* Specifying a selector using ``through``

  .. code:: graphql

    # Selects parents of first basic declaration
    from (select first BasicDecl) through parent select *

---------
Pattern
---------

* Simply a construction to match against a value

  * But can complicated very quickly!

* LKQL checks that value matches the pattern

  * In a query, matches get added to result of query

--------------------
Pattern Categories
--------------------

* Node patterns

  * Match one (or many) node kinds

    * Node kind name - match all nodes of this kind
    * Pattern - match multiple node kinds
    * Wildcard pattern - matchin all node kinds

  .. code:: graphql
    :font-size: small

    select *                           # Will select every node
    select BasicDecl                   # Will select every basic declaration
    select (ObjectDecl | BaseTypeDecl) # Will select every object and type declaration

* Regular values patterns

  * Match a value based on a pattern

  .. code:: graphql

    v is 12              # Integer pattern
    v is "hello.*?world" # Regex pattern

* Filtered patterns

  * Use ``when`` for an arbitrary boolean condition

    .. code:: graphql

      select BasicDecl when some_condition

----------------------
Selector Declaration
----------------------

* A :dfn:`selector` is a function that returns a ``Stream`` of values

  * Query expression explores tree via built-in selector ``children``

* Selector does not have parameters

  * But can take optional arguments

    * ``min_depth`` - traversal depth is lower than some value
    * ``max_depth`` - traversal depth is higher than some value
    * ``depth`` - traversal depth is exactly some value

  .. code:: graphql

    # Calling selectors directly
    val kids = children(node, depth=3)

    # Calling a selector in a nested sub-pattern
    select AdaNode(any children(min_depth=3): BasicDecl)

--------------------
Built-in Selectors
--------------------

* ``parent``

  * Parent nodes

* ``children``

  * Child nodes

* ``prev_siblings``

  * Sibling nodes that are before the current node

* ``next_siblings``

  * Sibling nodes that are after the current node

* ``super_types``

  * If the current node is a type, then all its parent types
