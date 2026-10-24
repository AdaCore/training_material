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

* Builtin functions used throughout LKQL functions

* Builtin methods for tree nodes

* :lkql:`stdlib` module defines supplied support functions

  * Not builtin so they need to be imported

----------------------------------
Accumulation Across a Collection
----------------------------------

* Somtimes we need a way to gather information about a collection

  * Count number of items
  * Total value of items

* :lkql:`reduce` function provides that capability

  .. code:: lkql

    reduce (collection, accumulating_function, initial_value)

* **Collection**

  * List or stream of items to traverse

* **Accumulating_Function**

  * Takes two parameters

    * Accumulator
    * Node

  * Can be either a function OR a parameter list and expression

* **Initial_Value** - initial value of accumulator

-------------------
"reduce" Examples
-------------------

* Counting number of children

  .. code:: lkql

    fn add(x, y) = x + y
    reduce(
        [1 for child in node.children],
        add,
        0)

  * Initialize accumulator to 0
  * Use list comprehension to add **1** to the list for every child
  * Call :lkql:`add` to add the item to the accumulator

* Calculating a total

  .. code:: lkql:

    val total = reduce([1, 2, 3], (acc, x) => acc + x, 0)

  * Initialize accumulator :lkql:`acc` to 0
  * For each item in the list, add it's value to :lkql:`acc`

    * :lkql:`total` = 6
