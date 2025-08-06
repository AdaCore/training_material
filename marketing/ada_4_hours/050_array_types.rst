*************
Array Types
*************

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

-------------
Terminology
-------------

* :dfn:`Index type`

   - Specifies the values to be used to access the array components

* :dfn:`Component type`

   - Specifies the type of values contained by objects of the array type
   - All components are of this same type

.. code:: Ada

   type Array_T is array (Index_T) of Component_T;

------------------------------
Array Type Index Constraints
------------------------------

* Must be of an integer or enumeration type
* Default to predefined `Integer`
* Allowed to be null range

   - Defines an empty array

.. code:: Ada

   type Schedule is array (Days range Mon .. Fri) of Float;
   type Flags_T is array (-10 .. 10) of Boolean;
   type Dynamic is array (1 .. N) of Integer; -- can be null range

   subtype Line is String (1 .. 80);
   subtype Translation is Matrix (1..3, 1..3);

-------------------------
Run-Time Index Checking
-------------------------

* Array indexes are checked at run-time as needed
* Invalid index values result in :ada:`Constraint_Error`

.. code:: Ada

   procedure Test is
     type Int_Arr is array (1..10) of Integer;
     A : Int_Arr;
     K : Integer;
   begin
     A := (others => 0);
     K := Foo;
     A (K) := 42; -- runtime error if Foo returns < 1 or > 10
     Put_Line (A(K)'Image);
   end Test;

===========================
Unconstrained Array Types
===========================

---------------------------------------
Unconstrained Array Type Declarations
---------------------------------------

* Do not specify bounds for objects

  -  Thus different objects of the same type may have different bounds

* Bounds cannot change once set

* Example

   .. code:: Ada

      type Index is range 1 .. Integer'Last;
      type Char_Arr is array (Index range <>) of Character;
      S1 : Char_Arr(1..10);
      S2 : Char_Arr := ('A', 'B', 'C');

----------------
"String" Types
----------------

* Language-defined unconstrained array types

   - Always have a character component type
   - Always one-dimensional

* Language defines various types

   - `String`, with `Character` as component

      .. code:: Ada

         subtype Positive is Integer range 1 .. Integer'Last;
         type String is array (Positive range <>) of Character;

   - `Wide_String`, with `Wide_Character` as component
   - `Wide_Wide_String`, with `Wide_Wide_Character` as component

* Can create your own

============
Aggregates
============

-----------------------------
Aggregate "Positional" Form
-----------------------------

* Specifies array component values explicitly
* Uses implicit ascending index values

.. code:: Ada

   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Working is array (Days) of Boolean;
   Week : Working;
   ...
   -- Saturday and Sunday are False, everything else true
   Week := (True, True, True, True, True, False, False);

------------------------
Aggregate "Named" Form
------------------------

* Explicitly specifies both index and corresponding component values
* Allows any order to be specified
* Ranges and choice lists are allowed (like case choices)

.. code:: Ada

   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Working is array (Days) of Boolean;
   Week : Working;
   ...
   Week := (Sat => False, Sun => False, Mon..Fri => True);
   Week := (Sat | Sun => False, Mon..Fri => True);

-----------
 "Others"
-----------

* Indicates all components not yet assigned a value
* All remaining components get this single value
* Similar to case statement's :ada:`others`
* Can be used to apply defaults too

.. code:: Ada

   type Schedule is array (Days) of Float;
   Work : Schedule;
   Normal : constant Schedule := (8.0, 8.0, 8.0, 8.0, 8.0,
                                  others => 0.0);
