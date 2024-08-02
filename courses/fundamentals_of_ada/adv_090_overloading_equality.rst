**********************
Overloading Equality
**********************

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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

--------------
Introduction
--------------

* Overloading equality is tricky

    - Is it bit-wise comparison or semantic comparison?
    - Result allowed to be non-boolean

* Complexity with user-defined composed types

    - Composition of equality

=======================
User-Defined Equality
=======================

----------
Examples
----------

.. include:: examples/090_overloading/user_defined_equality.rst

-----------------------
User-Defined Equality
-----------------------

* Allowed like any other operator

   - Must remain a binary operator

* May have any parameter and result types

   - Typically declared to return type Boolean

* Non-Boolean result example:

   .. code:: Ada

      type Fuzzy_Result is (Unknown, False, True);
      function "=" (Left : Foo;  Right : Bar)
          return Fuzzy_Result;

------------------------------------
User-Defined `=` Returning Boolean
------------------------------------

* Implicitly declares ``/=``
* Thus negation has consistent meaning

   .. code:: Ada

      if X /= Y then
      if not (X = Y) then
      if X not = Y then

* No explicit declaration of ``/=`` returning Boolean

   - Returning values of other types is allowed

      .. code:: Ada

         function "/=" (Left : Foo;  Right : Bar)
             return Fuzzy_Result;

-------------------------------
User-Defined Equality Example
-------------------------------

* Especially useful for composite types
* Predefined ``=`` is bit-wise comparison over entire structure so may be inappropriate semantics
* Given the following types:

   .. code:: Ada

      Max : constant := 100;
      type Index is range 0 .. Max;
      type Vector is array (Index range 1 .. Max) of Integer;
      type Stack is record
        Values : Vector;
        Top : Index := 0;
      end record;

* Equality function might look like:

   .. code:: Ada

      function "=" (Left, Right : Stack) return Boolean is
      begin
        if Left.Top /= Right.Top then -- not same size
          return False;
        else -- compare values
          for K in 1 .. Left.Top loop
            if Left.Values (K) /= Right.Values (K) then
              return False;
            end if;
          end loop;
        end if;
        return True;
      end "=";

=========================
Composition of Equality
=========================

----------
Examples
----------

.. include:: examples/090_overloading/composition_of_equality.rst

----------------------------
 "Composition of Equality"
----------------------------

* Whether user-defined equality functions are called automatically as part of equality for composite types containing types having such functions
* Only composes when user-defined equality is defined

   * Assume you defined "=" for a scalar type
   * If you define "=" for a composite containing the scalar type, your scalar "=" will be used
   * If you rely on the implicit "=" for the composite, then the scalar's implicit "=" will also be used

      * **Not** the one you just defined

--------------------------------
Composition Vs Non-Composition
--------------------------------

.. code:: Ada
   :number-lines: 1

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Main is

      type Array1_T is array (1 .. 3) of Integer;
      type Array2_T is array (1 .. 3) of Integer;

      X_A, Y_A : Array1_T := (others => 123);
      X_B, Y_B : Array2_T := (others => 123);

      -- When comparing Integers, this forces False result
      function "=" (L, R : Integer) return Boolean is
      begin
         return False;
      end "=";
      -- Array equality operator will use "=" on line 11
      function "=" (L, R : Array2_T) return Boolean is
      begin
         return (for all I in 1 .. 3 => L (I) = R (I));
      end "=";

   begin
      -- Use "=" on line 11 for Integer comparison
      Put_Line (Boolean'Image (X_A (2) = Y_A (2)));
      -- Comparison using predefined operator: "=" on line 11 is ignored
      Put_Line (Boolean'Image (X_A = Y_A));
      -- Comparison using "=" on line 16: "=" on line 11 is "composed"
      Put_Line (Boolean'Image (X_B = Y_B));
   end Main;

.. container:: speakernote

   Equality for Array1_T doesn't compose because Integer is not a record type.

-------------------------------------
Enclosing Equality Function Example
-------------------------------------

* Explicitly declared for the enclosing type
* Calls user-defined ``=`` for components

.. code:: Ada

   type Bar is record
     Value : Foo; -- assuming Foo is not a record type
     Id : Integer;
   end record;

   function "=" (Left, Right : Bar) return Boolean is
   begin
     -- User-defined "=" for Foo
     return Left.Value = Right.Value
        -- predefined "=" for Integer
        and Left.Id = Right.Id;
   end "=";

----------------------------------------
`=` for Predefined Composites Composes
----------------------------------------

* Per RM 4.5.2(32/1)
* For all non-limited types declared in language-defined packages
* Thus you can safely ignore the issue for composite types defined by the language

-----------------------------------
User-Defined Equality Composition
-----------------------------------

* No issue for all language-defined types in all versions of Ada
* An issue for user-defined types
* Only automatic for :ada:`record` types in Ada 2012
* Only automatic for :ada:`tagged record` types in Ada 2005

   - Otherwise need explicit equality function for enclosing type

* Not automatic for other user-defined types in any Ada version

   - Need explicit equality function for enclosing type

------
Quiz
------

.. code:: Ada

   type Range_T is range -1_000 .. 1_000;
   function "=" (L, R : Range_T) return Boolean is
      (Integer (abs (L)) = Integer (abs (R)));
   type Coord_T is record
      X : Range_T;
      Y : Range_T;
   end record;
   type Coord_3D_T is record
      XY : Coord_T;
      Z  : Range_T;
   end record;
   A : Coord_3D_T := (XY => (1, -1), Z => 2);
   B : Coord_3D_T := (XY => (-1, 1), Z => -2);

Which function will return True when comparing A and B?

A. | Implicit equality operator
B. | :answermono:`function "=" (L, R : Coord_3D_T) return Boolean is`
   |    :answermono:`(L.Z = R.Z and`
   |     :answermono:`L.XY.X = R.XY.X and L.XY.Y = R.XY.Y);`
C. | ``function "=" (L, R : Coord_3D_T) return Boolean is``
   |    ``(L.Z = R.Z and L.XY = R.XY);``
D. ``function "=" (L, R : Coord_3D_T) return Boolean is (L = R);``

.. container:: animate

   We are looking to use our own equality operator (that compares absolute
   values) so the only time that happens is when we examine each
   :ada:`Range_T` component individually

=========
Summary
=========

---------
Summary
---------

* Bit-wise comparison for standard types
* Remember :ada:`function "="` for record types does compose, otherwise not
