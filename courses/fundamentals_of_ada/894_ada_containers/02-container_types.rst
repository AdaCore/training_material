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
    - Differentiated by :ada:`<` and :ada:`=`
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

.. code:: Ada

   package Pkg_Vectors is new Ada.Containers.Bounded_Vectors
     (Index_Type => Index_Type,
      Element_Type => Integer);
      -- "=" (A, B : Integer) is directly visible

.. code:: Ada

   package Pkg_Sets is new Ada.Containers
     .Indefinite_Ordered_Sets
     (Element_Type => String);

.. code:: Ada

   package Pkg_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Float,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

---------------
Instantiation
---------------

* May require an initial :ada:`Empty_xxx` value

.. code:: Ada

   Student_Per_Day : Pkg_Vectors.Vector (5);
   -- Warning: initial size is 0, using an Empty_Vector as
   --          initial value would mean a *capacity* of 0!

   Received_Parcels : Pkg_Sets.Set := Pkg_Sets.Empty_Set;

   Math_Constants : Pkg_Maps.Map := Pkg_Maps.Empty_Map;
