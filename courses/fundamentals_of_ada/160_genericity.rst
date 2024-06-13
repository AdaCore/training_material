************
Genericity
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

-------------------------
The Notion of a Pattern
-------------------------

* Sometimes algorithms can be abstracted from types and subprograms

   .. code:: Ada

      procedure Swap_Int (Left, Right : in out Integer) is
        V : Integer := Left;
      begin
         Left := Right;
         Right := V;
      end Swap_Int;

      procedure Swap_Bool (Left, Right : in out Boolean) is
         V : Boolean := Left;
      begin
         Left := Right;
         Right := V;
      end Swap_Bool;

* It would be nice to extract these properties in some common pattern, and then just replace the parts that need to be replaced

   .. code:: Ada

      procedure Swap (Left, Right : in out (Integer | Boolean)) is
        V : (Integer | Boolean) := Left;
      begin
         Left := Right;
         Right := V;
      end Swap;

--------------------
Solution: Generics
--------------------

* A :dfn:`generic unit` is a unit that does not exist
* It is a pattern based on properties
* The instantiation applies the pattern to certain parameters

--------------------------------------
Ada Generic Compared to C++ Template
--------------------------------------

.. container:: columns

 .. container:: column

  Ada Generic

  .. container:: latex_environment scriptsize

    .. code:: Ada

      -- specification
      generic
        type T is private;
      procedure Swap (L, R : in out T);

      -- implementation
      procedure Swap (L, R : in out T) is
         Tmp : T := L;
      begin
         L := R;
         R := Tmp;
      end Swap;

      -- instance
      procedure Swap_F is new Swap (Float);

 .. container:: column

  C++ Template

  .. container:: latex_environment scriptsize

    .. code:: C++

      // prototype
      template <class T>
      void Swap (T & L, T & R);

      // implementation
      template <class T>
      void Swap (T & L, T & R) {
         T Tmp = L;
         L = R;
         R = Tmp;
      }

      // instance
      int x, y;
      Swap<int>(x,y);

===================
Creating Generics
===================

---------------------------
What Can Be Made Generic?
---------------------------

* Subprograms and packages can be made generic

   .. code:: Ada

      generic
         type T is private;
      procedure Swap (L, R : in out T)
      generic
         type T is private;
      package Stack is
         procedure Push (Item : T);
         ...

* Children of generic units have to be generic themselves

   .. code:: Ada

      generic
      package Stack.Utilities is
         procedure Print (S : Stack_T);

---------------------------
How Do You Use A Generic?
---------------------------

* Generic instantiation is creating new set of data where a generic package contains library-level variables:

.. code:: Ada

   package Integer_Stack is new Stack (Integer);
   package Integer_Stack_Utils is
       new Integer_Stack.Utilities;
   ...
   Integer_Stack.Push (S, 1);
   Integer_Stack_Utils.Print (S);

==============
Generic Data
==============

--------------------------------
Generic Types Parameters (1/2)
--------------------------------

* A generic parameter is a template
* It specifies the properties the generic body can rely on

   .. code:: Ada

      generic
         type T1 is private;
         type T2 (<>) is private;
         type T3 is limited private;
      package Parent is

* The actual parameter must be no more restrictive then the :dfn:`generic contract`

---------------------------------------
Generic Types Parameters (2/3)
---------------------------------------

* Generic formal parameter tells generic what it is allowed to do with the type

.. container:: latex_environment tiny

  .. list-table::

    * - :ada:`type T1 is (<>);`

      - Discrete type; :ada:`'First`, :ada:`'Succ`, etc available

    * - :ada:`type T2 is range <>;`

      - Signed Integer type; appropriate mathematic operations allowed

    * - :ada:`type T3 is digits <>;`

      - Floating point type; appropriate mathematic operations allowed

    * - :ada:`type T4 (<>);`

      - Indefinite type; can only be used as target of :ada:`access`

    * - :ada:`type T5 is tagged;`

      - :ada:`tagged` type; can extend the type

    * - :ada:`type T6 is private;`

      - No knowledge about the type other than assignment, comparison, object creation allowed

    * - :ada:`type T7 (<>) is private;`

      - :ada:`(<>)` indicates type can be unconstrained, so any object has to be initialized

--------------------------------
Generic Types Parameters (3/3)
--------------------------------

* The usage in the generic has to follow the contract

  * Generic Subprogram

    .. code:: Ada

       generic
          type T (<>) is private;
       procedure P (V : T);
       procedure P (V : T) is
          X1 : T := V; -- OK, can constrain by initialization
          X2 : T;      -- Compilation error, no constraint to this
       begin

  * Instantiations

    .. code:: Ada

       type Limited_T is limited null record;

       -- unconstrained types are accepted
       procedure P1 is new P (String);

       -- type is already constrained
       -- (but generic will still always initialize objects)
       procedure P2 is new P (Integer);

       -- Illegal: the type can't be limited because the generic
       -- thinks it can make copies
       procedure P3 is new P (Limited_T);

------------------------------------
Generic Parameters Can Be Combined
------------------------------------

* Consistency is checked at compile-time

.. code:: Ada

   generic
      type T (<>) is private;
      type Acc is access all T;
      type Index is (<>);
      type Arr is array (Index range <>) of Acc;
   function Element (Source   : Arr;
                     Position : Index)
                     return T;

   type String_Ptr is access all String;
   type String_Array is array (Integer range <>)
       of String_Ptr;

   function String_Element is new Element
      (T     => String,
       Acc   => String_Ptr,
       Index => Integer,
       Arr   => String_Array);

------
Quiz
------

.. code:: Ada

   generic
      type T1 is (<>);
      type T2 (<>) is private;
   procedure G
     (A : T1;
      B : T2);

Which is **not** a legal instantiation?

   A. :answermono:`procedure A is new G (String, Character);`
   B. ``procedure B is new G (Character, Integer);``
   C. ``procedure C is new G (Integer, Boolean);``
   D. ``procedure D is new G (Boolean, String);``

.. container:: animate

   :ada:`T1` must be discrete - so an integer or an enumeration. :ada:`T2` can be any type

=====================
Generic Formal Data
=====================

--------------------------------------------
Generic Constants/Variables as Parameters
--------------------------------------------

.. container:: columns

 .. container:: column

    * Variables can be specified on the generic contract
    * The mode specifies the way the variable can be used:

       - :ada:`in` |rightarrow| read only
       - :ada:`in out` |rightarrow| read write

    * Generic variables can be defined after generic types

 .. container:: column

   .. container:: latex_environment tiny

     * Generic package

       .. code:: Ada

          generic
            type Element_T is private;
            Array_Size     : Positive;
            High_Watermark : in out Element_T;
          package Repository is

     * Generic instance

       .. code:: Ada

         V   : Float;
         Max : Float;

         procedure My_Repository is new Repository
           (Element_T      => Float,
            Array_size     => 10,
            High_Watermark => Max);

-------------------------------
Generic Subprogram Parameters
-------------------------------

* Subprograms can be defined in the generic contract
* Must be introduced by :ada:`with` to differ from the generic unit

   .. code:: Ada

      generic
         type T is private;
         with function Less_Than (L, R : T) return Boolean;
      function Max (L, R : T) return T;

      function Max (L, R : T) return T is
      begin
         if Less_Than (L, R) then
            return R;
         else
            return L;
         end if;
      end Max;

      type Something_T is null record;
      function Less_Than (L, R : Something_T) return Boolean;
      procedure My_Max is new Max (Something_T, Less_Than);

----------------------------------------
Generic Subprogram Parameters Defaults
----------------------------------------

* :ada:`is <>` - matching subprogram is taken by default
* :ada:`is null` - null subprogram is taken by default

   - Only available in Ada 2005 and later

   .. code:: Ada

      generic
        type T is private;
        with function Is_Valid (P : T) return Boolean is <>;
        with procedure Error_Message (P : T) is null;
      procedure Validate (P : T);

      function Is_Valid_Record (P : Record_T) return Boolean;

      procedure My_Validate is new Validate (Record_T,
                                             Is_Valid_Record);
      -- Is_Valid maps to Is_Valid_Record
      -- Error_Message maps to a null subprogram

..
  language_version 2005

------
Quiz
------

.. include:: quiz/genericity_type_and_variable/quiz.rst

------
Quiz
------

.. container:: columns

 .. container:: column


   .. code:: Ada
      :number-lines: 1

      procedure Double (X : in out Integer);
      procedure Square (X : in out Integer);
      procedure Half (X : in out Integer);
      generic
         with procedure Double (X : in out Integer) is <>;
         with procedure Square (X : in out Integer) is null;
      procedure Math (P : in out Integer);
      procedure Math (P : in out Integer) is
      begin
         Double (P);
         Square (P);
      end Math;
      procedure Instance is new Math (Double => Half);
      Number : Integer := 10;

 .. container:: column

 .. container:: column

   What is the value of Number after calling :ada:`Instance (Number)`

   A. 20
   B. 400
   C. :answer:`5`
   D. 10

.. container:: animate

  A. Would be correct for :ada:`procedure Instance is new Math;`

  B. Would be correct for either :ada:`procedure Instance is new Math (Double, Square);` *or* :ada:`procedure Instance is new Math (Square => Square);`

  C. Correct

    * We call formal parameter :ada:`Double`, which has been assigned to actual subprogram :ada:`Half`, so :ada:`P`, which is 10, is halved.

    * Then we call formal parameter :ada:`Square`, which has no actual subprogram, so it defaults to :ada:`null`, so nothing happens to :ada:`P`

  D. Would be correct for either :ada:`procedure Instance is new Math (Double, Half);` *or* :ada:`procedure Instance is new Math (Square => Half);`

..
  language_version 2005

----------------------
Quiz Answer In Depth
----------------------

      A. Wrong - result for :ada:`procedure Instance is new Math;`
      B. Wrong - result for :ada:`procedure Instance is new Math (Double, Square);`
      C. :ada:`Double` at line 10 is mapped to :ada:`Half` at line 3, and :ada:`Square` at line 11 wasn't specified so it defaults to :ada:`null`
      D. Wrong - result for :ada:`procedure Instance is new Math (Square => Half);`

.. container:: animate

  .. container:: latex_environment tiny

    :ada:`Math` is going to call two subprograms in order, :ada:`Double` and :ada:`Square`, but both of those come from the formal data.

    Whatever is used for :ada:`Double`, will be called by the :ada:`Math` instance. If nothing is passed in, the compiler tries to find a subprogram named :ada:`Double` and use that. If it doesn't, that's a compile error.

    Whatever is used for :ada:`Square`, will be called by the :ada:`Math` instance. If nothing is passed in, the compiler will treat this as a null call.

    In our case, :ada:`Half` is passed in for the first subprogram, but nothing is passed in for the second, so that call will just be null.

    So the final answer should be 5 (hence letter C).

====================
Generic Completion
====================

------------------------------
Implications at Compile-Time
------------------------------

* The body needs to be visible when compiling the user code
* Therefore, when distributing a component with generics to be instantiated, the code of the generic must come along

-----------------------------
Generic and Freezing Points
-----------------------------

* A generic type **freezes** the type and needs the **full view**
* May force separation between its declaration (in spec) and instantiations (in private or body)

.. code:: Ada

   generic
      type X is private;
   package Base is
      V : access X;
   end Base;

   package P is
      type X is private;
      -- illegal
      package B is new Base (X);
   private
      type X is null record;
   end P;

-------------------------------
Generic Incomplete Parameters
-------------------------------

* A generic type can be incomplete
* Allows generic instantiations before full type definition
* Restricts the possible usages (only :ada:`access`)

.. code:: Ada

   generic
      type X; -- incomplete
   package Base is
      V : access X;
   end Base;

   package P is
      type X is private;
      -- legal
      package B is new Base (X);
   private
      type X is null record;
   end P;

------
Quiz
------

.. include:: quiz/genericity_private_type/quiz.rst

========
Lab
========

.. include:: labs/160_genericity.lab.rst

=========
Summary
=========

-------------------------------------
Generic Routines vs Common Routines
-------------------------------------

.. code:: Ada

   package Helper is
      type Float_T is digits 6;
      generic
         type Type_T is digits <>;
         Min : Type_T;
         Max : Type_T;
      function In_Range_Generic (X : Type_T) return Boolean;
      function In_Range_Common (X   : Float_T;
                                Min : Float_T;
                                Max : Float_T)
                                return Boolean;
   end Helper;

   procedure User is
     type Speed_T is new Float_T range 0.0 .. 100.0;
     B : Boolean;
     function Valid_Speed is new In_Range_Generic
        (Speed_T, Speed_T'First, Speed_T'Last);
   begin
     B := Valid_Speed (12.3);
     B := In_Range_Common (12.3, Speed_T'First, Speed_T'Last);

.. container:: speakernote

   Generics increase code size and readability
   Common functions reduce size, but increase error possibilities

---------
Summary
---------

* Generics are useful for copying code that works the same just for different types

   - Sorting, containers, etc

* Properly written generics only need to be tested once

   - But testing / debugging can be more difficult

* Generic instantiations are best done at compile time

   - At the package level
   - Can be run-time expensive when done in subprogram scope
