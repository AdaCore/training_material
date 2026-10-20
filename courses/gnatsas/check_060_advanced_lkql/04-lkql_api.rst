==========
LKQL API
==========

---------------------
Libadalang and LKQL
---------------------

* Libadalang API can be called from LKQL

  * Basis for most :toolname:`GNATcheck` rules

* Broken down by category

  * **Node types** - syntactic and semantic constructs
  * **Symbol types** - identifiers and lexical tokens
  * **List types** - strongly-typed sequences of nodes
  * **Object types** - core session and lifecycle structures

* *Origin* parameter

  * Many Libadalang properties accept optional :lkql:`origin` parameter
  * Specifies from where property is evaluated

    * Default is the current node
    
  * For example, :lkql:`p_most_visible_part` returns different data for a
    private type depending on the location of the node

------------------
Standard Library
------------------

* Builtin functions used throught LKQL functions

* Builtin methods for tree nodes

* :lkql:`stdlib` module defines supplied support functions

  * Not builtin so they need to be imported
