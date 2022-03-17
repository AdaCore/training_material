************
Containers
************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

-------------------
Container Library
-------------------

* Packages (including generics) for different types of data containers

   - Vectors
   - Doubly-linked lists
   - Maps
   - Sets
   - Trees
   - Queues
   - Indefinite containers (holder/wrapper)

* Sorting mechanisms

-------------------
Library Structure
-------------------

* Groups of objects of

   - Definite types
   - Indefinite types
   - Bounded capacity (of definite types)

* Definite types

   - More efficient

* Indefinite types

   - More flexible

* Bounded capacity

   - Known size

-----------------------------
Common Container Processing
-----------------------------

* Every container has a type `Cursor` which points to an element in a container

   - Points to the container as well as the element!

* Container types are tagged allowing for prefixed notation

   - Prefixed: `My_List.Append ( Item );`
   - Untagged: `Append(My_List, Item);`

* Hashed vs Ordered

   - **Ordered** containers are sequential

      + Searching can be slow for large containers

   - **Hashed** containers require effort

      + Hash function needs to be carefully written

==========
Examples
==========

--------------------------------------
Actual containers - Ada.Containers.*
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

----------------------------
Doubly Linked List Example
----------------------------

.. code:: Ada

   type Space_Agencies_T is (Nasa, Esa, Rsa);
   Last_Position : constant integer :=
      Space_Agencies_T'pos ( Space_Agencies_T'last );

   type List_Element_T is
      record
         Agency : Space_Agencies_T;
         Values : Satellite.Data_T;
      end record;
   function Is_Equal ( Left, Right : List_Element_T )
         return boolean is
      ( Left.Agency = Right.Agency );

   package Database_Pkg is new
     Ada.Containers.Bounded_Doubly_Linked_Lists
       (Element_Type => List_Element_T,
        "="          => Is_Equal);

   Database : Database_Pkg.List(
      Ada.Containers.Count_Type(1 + Last_Position));

----------------
Vector Example
----------------

.. code:: Ada

   type Space_Agencies_T is (Nasa, Esa, Rsa);
   Last_Position : constant integer :=
      Space_Agencies_T'pos ( Space_Agencies_T'last );

   package Database_Pkg is new
      Ada.Containers.Bounded_Vectors
         (Index_Type => natural,
          Element_Type => Satellite.Data_T);

   Database : Database_Pkg.Vector(
      Ada.Containers.Count_Type(1 + Last_Position));

-------------
Map Example
-------------

.. code:: Ada

   type Space_Agencies_T is (Nasa, Esa, Rsa);
   Last_Position : constant integer :=
      Space_Agencies_T'pos ( Space_Agencies_T'last );

   package Database_Pkg is new
      Ada.Containers.Bounded_Ordered_Maps
         (Key_Type => Space_Agencies_T,
          Element_Type => Satellite.Data_T);

   Database : Database_Pkg.Map(Ada.Containers.Count_Type(
                               1 + Last_Position));

--------------------
Sorting Mechanisms
--------------------

* Arrays

   - `Ada.Containers.Generic_Array_Sort`
   - `Ada.Containers.Generic_Constrained_Array_Sort`

* Any object

   - `Ada.Containers.Generic_Sort`

      + Allows you to define your own comparison and swap mechanisms!
      + This means you can extend sorting to container classes

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
