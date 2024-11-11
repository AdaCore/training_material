************
Containers
************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==============
Introduction
==============

-------------------
Container Library
-------------------

* :ada:`Ada.Containers` parent package
* Packages (including generics)

    - Different types of data containers
    - Hold an :ada:`Element` type
    - Container types are :ada:`tagged`

* Types defined as a product of both

    - A data structure
    - An implementation

* Containers share sets of operations

    - Seen later

=================
Container Types
=================

-----------------------
Data Structures (1/2)
-----------------------

* Vector

   - Essentially an array
   - Dynamic length

* Doubly-linked list

    - Linked list
    - Iteration in both directions

* Map

    - Containers matching Key -> Element
    - Not a one-to-one relationship

        + Can have several keys for a single element

* Set

    - Collection of **unique** values

* Queue

    - No iterator
    - Only ordered access
    - For multi-tasking operations

..
  language_version 2012

----------------------
Data Structures (2/2)
----------------------

* Tree

    - Similar to list
    - A node can have several children

* Holder

    - Wraps around an indefinite (unconstrained, class-wide ...)
    - Resulting type is definite
    - Single element, no iteration or cursor

..
  language_version 2012

-----------------------
Implementations (1/2)
-----------------------

* :dfn:`Bounded`

    - Maximal storage is bounded
    - Constant :dfn:`capacity` and element size
    - Only static allocation
    - :ada:`Bounded_<Structure>`

* :dfn:`Unbounded`

    - Capacity can grow dynamically
    - Easiest to use
    - Default

* :dfn:`Ordered`

    - Elements are sorted in order
    - Must provide :ada:`<` and :ada:`=` operators
    - Not hashed
    - :ada:`XXX_Ordered_<Structure>`

* :dfn:`Hashed`

    - Elements are hashed
    - Must provide :ada:`Hash` function and :ada:`=` operator
    - Not ordered
    - Some hash functions are provided (e.g. :ada:`Ada.Strings.Hash`)
    - :ada:`XXX_Hashed_<Structure>`

-----------------------
Implementations (2/2)
-----------------------

* :dfn:`Indefinite`

    - Element can be indefinite
    - Size of element is unknown
    - :ada:`Indefinite_XXX_<Structure>`

..
  language_version 2012

-----------------------
Example of Containers
-----------------------

* Standard defines 25 different container variations
* :ada:`Indefinite_Vector`

    - Static capacity
    - Dynamically sized (indefinite elements)
    - Random access in ``O(1)``

* :ada:`Ordered_Set`

    - Unique elements
    - Differenciated by :ada:`<` and :ada:`=`
    - Manipulated in order

* :ada:`Bounded_Doubly_Linked_List`

    - Static size of container and elements
    - Insertions and deletions in ``O(1)``

-------------
Declaration
-------------

* Generic packages
* Always need at least the ``Element_Type``
* Examples chosen for the next slides:

.. include:: examples/containers/extracts/decl_vector.ads
    :code: Ada

.. include:: examples/containers/extracts/decl_set.ads
    :code: Ada

.. include:: examples/containers/extracts/decl_map.ads
    :code: Ada

---------------
Instantiation
---------------

* May require an initial :ada:`Empty_xxx` value

.. include:: examples/containers/extracts/decl_instances.adb
    :code: Ada

=======================
Containers Operations
=======================

-------------------
Common Operations
-------------------

* Lots of common operations

    - What is available depends greatly on the exact container type
    - ... so does syntax

* Insertion
* Iteration
* Comparison
* Sort
* Search
* Aggregates

-----------
Insertion
-----------

* May be in order :ada:`Append` or :ada:`Prepend`
* May be :ada:`Insert` (at random or at given index)
* May :ada:`Replace` an existing element

.. include:: examples/containers/extracts/insert.adb
    :code: Ada

-----------
Iteration
-----------

* Container has a :ada:`Cursor` type

    - Points to an element in a container
    - Can be used for advanced iterations

.. include:: examples/containers/extracts/iterate.adb
    :code: Ada

------------
Comparison
------------

.. include:: examples/containers/extracts/compare.adb
    :code: Ada

------
Sort
------

* Arrays

   - `Ada.Containers.Generic_Array_Sort`
   - `Ada.Containers.Generic_Constrained_Array_Sort`

* Any type that supports indexing

   - `Ada.Containers.Generic_Sort`

.. include:: examples/containers/extracts/sort.adb
    :code: Ada

--------
Search
--------

* Use :ada:`Find` for a :ada:`Cursor`

    - :ada:`<Pkg>.No_Element` returned if unsuccesful

        + :ada:`Has_Element (No_Element) = False`

* Use :ada:`Find_Index` for an :ada:`Index_Type` (vectors)

.. include:: examples/containers/extracts/search.adb
    :code: Ada

------------
Aggregates
------------

.. admonition:: Language Variant

    Ada 2022

* In Ada 2022, containers can be initialized with aggregates

   * Similar to arrays and records
   * Container aggregates use square brackets **[..]**

* Maps required named notation, all other containers use positional notation

.. code:: Ada

   package Int_Vectors is new Ada.Containers.Vectors
     (Positive, Integer);

   X : constant Int_Vectors.Vector := [1, 2, 3];

   type Key_T is (Height, Width, Depth);
   package Float_Maps is new Ada.Containers.Ordered_Maps
     (Key_T, Float);

   Y : constant Float_Maps.Map := [Height => 1.0,
                                   Width => 2.5,
                                   Depth => 5.51];

*Note that if you create your own container types, you will need to use the aspect* :ada:`Aggregate` *to enable this functionality.*

===========
Reference
===========

--------------------------------------
Standard ``Ada.Containers`` Packages
--------------------------------------

* Definite Types

  - Vectors
  - Doubly_Linked_Lists
  - Multiway_Trees
  - Hashed_Maps
  - Ordered_Maps
  - Hashed_Sets
  - Ordered_Sets

* Indefinite Types

  - Indefinite_Vectors
  - Indefinite_Doubly_Linked_Lists
  - Indefinite_Multiway_Trees
  - Indefinite_Hashed_Maps
  - Indefinite_Ordered_Maps
  - Indefinite_Hashed_Sets
  - Indefinite_Ordered_Sets
  - Indefinite_Holders

* Bounded Types

  - Bounded_Vectors
  - Bounded_Doubly_Linked_Lists
  - Bounded_Multiway_Trees
  - Bounded_Hashed_Maps
  - Bounded_Ordered_Maps
  - Bounded_Hashed_Sets
  - Bounded_Ordered_Sets

========
Lab
========

.. include:: labs/adv_250_containers.lab.rst

=========
Summary
=========

-------------------
Containers Review
-------------------

* Containers class is the ultimate "code re-use"

   - Solidifies most common containers used in coding
   - Full functionality

      + When writing your own, you may not create all the functions someone else neds

   - Part of the language, so reliability is much higher

* Availability depends on language-version and runtime

    - Typically not available on certified runtimes (e.g. Ravenscar)
