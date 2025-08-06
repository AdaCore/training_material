==============================
Looping Over Array Components
==============================

------------------------------------------------
Note on Default Initialization for Array Types
------------------------------------------------

* In Ada, objects are not initialized by default
* To initialize an array, you can initialize each component

   * But if the array type is used in multiple places, it would be better to initialize at the type level
   * No matter how many dimensions, there is only one component type

* Uses aspect `Default_Component_Value`

   .. code:: Ada

      type Vector is array (Positive range <>) of Float
         with Default_Component_Value => 0.0;

   - Note that creating a large object of type :ada:`Vector` might incur a run-time cost during initialization

..
  language_version 2012

-------------------------------
Two High-Level For-Loop Kinds
-------------------------------

* For arrays and containers

   - Arrays of any type and form
   - Iterable containers

      + Those that define iteration (most do)
      + Not all containers are iterable (e.g., priority queues)!

* For iterator objects

   - Known as "generalized iterators"
   - Language-defined, e.g., most container data structures

* User-defined iterators too
* We focus on the arrays/containers form for now

..
  language_version 2012

---------------------------
Array/Container For-Loops
---------------------------

* Work in terms of components within an object
* Syntax hides indexing/iterator controls

   .. code:: Ada

      for name of [reverse] array_or_container_object loop
      ...
      end loop;

* Starts with "first" component unless you reverse it
* Loop parameter name is a constant if iterating over a constant, a variable otherwise

..
  language_version 2012

----------------------------------
Array Component For-Loop Example
----------------------------------

* Given an array

   .. code:: Ada

        type T is array (Positive range <>) of Integer;
        Primes : T := (2, 3, 5, 7, 11);

* Component-based looping would look like

   .. code:: Ada

      for P of Primes loop
         Put_Line (Integer'Image (P));
      end loop;

* While index-based looping would look like

   .. code:: Ada

      for P in Primes'Range loop
         Put_Line (Integer'Image (Primes (P)));
      end loop;

..
  language_version 2012

------
Quiz
------

.. code:: Ada

   declare
      type Array_T is array (1..5) of Integer
         with Default_Component_Value => 1;
      A : Array_T;
   begin
      for I in A'First + 1 .. A'Last - 1 loop
         A (I) := I * A'Length;
      end loop;
      for I of reverse A loop
         Put (I'Image);
      end loop;
   end;

.. container:: columns

 .. container:: column

   Which output is correct?

      A. 1 10 15 20 1
      B. :answer:`1 20 15 10 1`
      C. 0 10 15 20 0
      D. 25 20 15 10 5

 .. container:: column

  .. container:: animate

     Explanations

     A. There is a :ada:`reverse`
     B. Yes
     C. Default value is 1
     D. No

NB: Without :ada:`Default_Component_Value`, init. values are random

