===================
Formal Containers
===================

-------------------------------
Formal Containers in SPARKlib
-------------------------------

* Available from SPARK Library

  - Distributed with SPARK Pro
  - Copy :filename:`sparklib.gpr` or :filename:`sparklib_light.gpr` locally
  - Set value of :code:`Object_Dir` in the copied project file
  - To use, add :code:`with "sparklib[_light]";` in your project file

* Reminder: four kinds of formal containers

  - vectors
  - doubly linked lists
  - sets (hashed and ordered)
  - maps (hashed and ordered)

* All available in bounded and unbounded versions

* All generics that need to be instantiated

  - Only their spec is in SPARK
  - Their implementation is not proved

---------------------------
Bounded Formal Containers
---------------------------

* Bounded version for light and embedded runtimes

* Under :ada:`SPARK.Containers.Formal.<name>`

* Use discriminated record

  - Discriminant :ada:`Capacity` fixes maximum size

* Component type must have known size (:dfn:`definite` type)

* Container type itself is definite

  - Bounded container can be component of another formal container

-----------------------------
Unbounded Formal Containers
-----------------------------

* Unbounded version for full runtimes

* Under :ada:`SPARK.Containers.Formal.Unbounded_<name>`

* Use dynamic memory allocation

  - For each component in the container
  - For growing the container

* Use controlled types for dynamic memory reclamation

* Component type may have unknown size (:dfn:`indefinite` type)

* Container type itself is definite

  - Unbounded container can be component of another formal container

------------------------------
Loops Over Formal Containers
------------------------------

* Same as for quantified expressions

* Range-based iteration (only for vectors)

  .. code:: ada

     for J in V.First_Index .. V.Last_Index loop
        V.Replace_Element (J, 0);
     end loop;

* Iteration over positions

  .. code:: ada

     for J in V loop
        V.Replace_Element (J, 0);
     end loop;

* Iteration over components (no update!)

  .. code:: ada

     for E of V loop
        pragma Assert (E = 0);
     end loop;

----------------------------------------
Loop Invariants Over Formal Containers
----------------------------------------

* Range-based iteration (only for vectors)

  - Use scalar index :ada:`J` to access vector at :ada:`V.Element (J)`

|

* Iteration over positions

  - For vectors, same as range-based iteration (cursor is index)
  - Otherwise, need to reason about formal model

    + Functional model of the container
    + Mapping from cursors to positions
    + Sequence of components/keys of the container

|

* Iteration over components

  - Impossible to access previous components
  - Use iteration over positions instead

-----------------------------------
Formal Model of Formal Containers
-----------------------------------

* Defined in local package :ada:`Formal_Model`

  - Based on functional containers (also part of SPARKlib)

    + Immutable containers to represent mathematical one

  - Used in contracts of formal containers API

* Functional model of the container

  - Given by function :ada:`Model`
  - Returns a different type

    + A sequence of components for formal lists
    + A set of components for formal sets
    + A map from keys to components for maps

* Mapping from cursors to positions

  - Given by function :ada:`Positions`
  - Positions in the iteration sequence

* Sequence of components/keys of the container

  - Corresponds to the iteration sequence
  - Given by different functions

    + :ada:`Model` for lists
    + :ada:`Elements` for sets
    + :ada:`Keys` for maps

------------------------------------------------
Difficulties with Loops Over Formal Containers
------------------------------------------------

* :toolname:`GNATprove` does not unroll such loops

|

* :toolname:`GNATprove` does not generate a frame condition

  - Contrary to loops over arrays
  - Need to explicitly state the frame condition using attribute
    :ada:`Loop_Entry`

|

* Container structure may be modified in the loop

  - When inserting or deleting components
  - In general, need to know position of corresponding cursor

    + Relative to current cursor: e.g. previous/next cursor
    + Otherwise difficult with hashed sets/maps

----------------------
Functional Containers
----------------------

* Available from SPARK Library

* Five kinds of functional containers

  - infinite sequences
  - vectors
  - sets
  - multisets
  - maps

* Simple containers close to mathematical structures

  - No bounds on cardinality
  - No cursors for iteration
  - No order of components in sets and maps
  - Functional: cannot modify them, rather create a new one

* They are easy to handle for proof

  - Often used as models for more complex structures

* They are executable but might be inefficient

