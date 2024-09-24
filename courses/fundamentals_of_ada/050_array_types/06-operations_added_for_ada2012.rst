==============================
Operations Added for Ada2012
==============================

----------------------------------------
Default Initialization for Array Types
----------------------------------------

* Supports constrained and unconstrained array types
* Supports arrays of any dimensionality

   - No matter how many dimensions, there is only one component type

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

* Work in terms of elements within an object
* Syntax hides indexing/iterator controls

   .. code:: Ada

      for name of [reverse] array_or_container_object loop
      ...
      end loop;

* Starts with "first" element unless you reverse it
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

----------------------------------------
For-Loops with Multidimensional Arrays
----------------------------------------

.. container:: columns

 .. container:: column

    * Same syntax, regardless of number of dimensions
    * As if a set of nested loops, one per dimension

       - Last dimension is in innermost loop, so changes fastest

    * In low-level format looks like

    .. code::

       for each row loop
          for each column loop
             print Identity (
                row, column)
          end loop
       end loop

 .. container:: column

   .. container:: latex_environment small

    .. code:: Ada

       declare
         subtype Rows is Positive;
         subtype Columns is Positive;
         type Matrix is array
            (Rows range <>,
             Columns range <>) of Float;
           Identity : constant Matrix
              (1..3, 1..3) :=
                ((1.0, 0.0, 0.0),
                 (0.0, 1.0, 0.0),
                 (0.0, 0.0, 1.0));
       begin
         for C of Identity loop
           Put_Line (Float'Image (C));
         end loop;

..
  language_version 2012

------
Quiz
------

.. code:: Ada

   declare
      type Array_T is array (1..3, 1..3) of Integer
         with Default_Component_Value => 1;
      A : Array_T;
   begin
      for I in 2 .. 3 loop
         for J in 2 .. 3 loop
            A (I, J) := I * 10 + J;
         end loop;
      end loop;
      for I of reverse A loop
         Put (I'Image);
      end loop;
   end;

.. container:: columns

 .. container:: column

   Which output is correct?

      A. 1 1 1 1 22 23 1 32 33
      B. :answer:`33 32 1 23 22 1 1 1 1`
      C. 0 0 0 0 22 23 0 32 33
      D. 33 32 0 23 22 0 0 0 0

 .. container:: column

  .. container:: animate

     Explanations

     A. There is a :ada:`reverse`
     B. Yes
     C. Default value is 1
     D. No

NB: Without :ada:`Default_Component_Value`, init. values are random

