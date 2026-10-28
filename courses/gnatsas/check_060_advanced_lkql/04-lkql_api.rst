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
  * **Symbol types** - named values representing categories or kinds
  * **List types** - strongly-typed sequences of nodes
  * **Object types** - structured data (such as query results)

* *Origin* parameter

  * Some Libadalang properties accept optional :lkql:`origin` parameter
  * Specifies the node from which visibility is evaluated

    * Default is the current node
    
  * For example, :lkql:`p_most_visible_part` returns different data for a
    private type depending on the location of the node

------------------
Standard Library
------------------

* Built-in functions used throughout LKQL functions

* Built-in methods for tree nodes

* :lkql:`stdlib` module defines supplied support functions

  * Not built-in so they need to be imported

----------------------------------
Accumulation Across a Collection
----------------------------------

* Sometimes we need a way to gather information about a collection

  * Count number of items
  * Sum of integer items

* :lkql:`reduce` function provides that capability

  .. code:: lkql

    reduce (collection, accumulating_function, initial_value)

* **Collection**

  * List or stream of items to traverse

* **Accumulating_Function**

  * Takes two parameters

    * Accumulator
    * Item from the collection

  * Can be either a function OR a parameter list and expression

* **Initial_Value** - initial value of accumulator

-------------------
"reduce" Examples
-------------------

* Counting number of children

  .. code:: lkql

    fun add(x, y) = x + y
    reduce(
        [1 for child in node.children],
        add,
        0)

  * Initialize accumulator to 0
  * Use list comprehension to add **1** to the list for every child
  * Call :lkql:`add` to add the item to the accumulator

* Calculating a total

  .. code:: lkql

    val total = reduce([1, 2, 3], (acc, x) => acc + x, 0)

  * Initialize accumulator :lkql:`acc` to 0
  * For each item in the list, add its value to :lkql:`acc`

    * :lkql:`total` = 6
