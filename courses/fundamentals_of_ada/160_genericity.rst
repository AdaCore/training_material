************
Genericity
************

.. |rightarrow| replace:: :math:`\rightarrow`

.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

-------------------------
The Notion of a Pattern
-------------------------

* Some algorithms can be **abstracted** from **types** and **subprograms**
    
   .. code:: Ada
    
      procedure Swap_Int ( Left, Right : in out Integer) is
        V : Integer;
      begin
         V := Left;
         Left := Right;
         Right := V;
      end Swap_Int;
          
      procedure Swap_Bool (Left, Right : in out Boolean) is
         V : Boolean;
      begin
         V := Left;
         Left := Right;
         Right := V;
      end Swap_Bool;
     
---------
Generics
---------

* A generic unit is **declared** but **not** part of the program

    - It needs to be **instanciated**

* Generics use a **pattern**, and replace only parts of it
    
   .. code:: Ada
    
      procedure Swap (Left, Right : in out (Integer | Boolean)) is
        V : (Integer | Boolean);
      begin
         V := Left;
         Left := Right;
         Right := V;
      end Swap;

* An **instantiation** of the generic provides the needed values

--------------------------------------
Ada Generic Compared to C++ Template
--------------------------------------

* Ada :ada:`generic`

   .. code:: Ada
    
      -- specification
      generic
        type T is private;
      procedure Swap (L, R : in out T);
      -- implementation
      procedure Swap (L, R : in out T) is
        Tmp : T := L
      begin
        L := R;
        R := Tmp;
      end Swap;
      -- instance
      procedure Swap_F is new Swap (Float);
      F1, F2 : Float;
      procedure Main is
      begin
         Swap_F (F1, F2);
      end Main;
     
* C++ Template

   .. code:: C++
    
      template <class T>
      void Swap (T & L, T & R);
      template <class T>
      void Swap (T & L, T & R) {
         T Tmp = L;
         L = R;
         R = Tmp;
      }
      float F1, F2;
      void Main (void) {
         Swap <float> (F1, F2);
      }

===================
Creating Generics
===================

---------------------------
What Can Be Made Generic?
---------------------------

* **Subprograms** and **packages** can be generic

   .. code:: Ada

      generic
         type T is private;
      procedure Swap (L, R : in out T)
      generic
         type T is private;
      package Stack is
         procedure Push (Item : T);
         ...
 
* Children of generic package **must be** generic

   .. code:: Ada

      generic
      package Stack.Utilities is
         procedure Print is 

-------------
Instanciation
-------------

* A generic is **instanciated** by providing **data** for its parameters
* Instanciation uses the :ada:`new` keyword

.. code:: Ada

   package Integer_stack is new Stack (Integer);
   package Integer_Stack_Utils is
       new Integer_Stack.Utilities;
   ...
   Integer_Stack.Push (1);
   Integer_Stack_Utils.Print;

==============
Generic Data
==============

----------
Examples
----------

.. include:: examples/160_genericity/generic_data.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/160_genericity.html#generic-data`

--------------------------------
Generic Types Parameters (1/2)
--------------------------------

* A **generic** parameter specifies **properties** and **constraints**

    - **Generic contract** the generic body can rely on

   .. code:: Ada
      
      generic
         type T1 is private; -- should have properties
                             -- of private type (assignment,
                             -- comparison, able to declare
                             -- variables on the stack...)
         type T2 (<>) is private;    -- can be unconstrained
         type T3 is limited private; -- can be limited
      package Parent is [...]
 
* The **actual** parameter **must** provide the specified properties and constraints

--------------------------------
Generic Types Parameters (2/2)
--------------------------------

* The **usage** in the generic has to follow the contract

.. code:: Ada

   generic
      type T (<>) is private;
   procedure P (V : T);
   procedure P (V : T) is
      X1 : T := V; -- OK, can constrain by initialization
      X2 : T;      -- Compilation error, no constraint to this
   begin
   ...
   type L_T is limited null record;
   ...
   -- unconstrained types are accepted
   procedure P1 is new P (String); 
   -- type is already constrained
   procedure P2 is new P (Integer); 
   -- Illegal: the type can't be limited because the generic
   -- is allowed to make copies
   procedure P3 is new P (L_T);
 
---------------------------------------
Possible Properties for Generic Types
---------------------------------------

.. code:: Ada

   type T1 is (<>); -- discrete
   type T2 is range <>; -- integer
   type T3 is digits <>; -- float
   type T4 (<>); -- indefinite
   type T5 is tagged;
   type T6 is array ( Boolean ) of Integer;
   type T7 is access integer;
   type T8 (<>) is [limited] private;
 
------------------------------------
Generic Parameters Can Be Combined
------------------------------------

* Consistency is **checked** at **compile-time**

.. code:: Ada

   generic
      type T (<>) is limited private;
      type Acc is access all T;
      type Index is (<>);
      type Arr is array (Index range <>) of Acc;
   procedure P;
   
   type String_Ptr is access all String;
   type String_Array is array (Integer range <>)
       of String_Ptr;
   
   procedure P_String is new P
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

Which is an illegal instantiation?

   A. :answermono:`procedure A is new G (String, Character);`
   B. ``procedure B is new G (Character, Integer);``
   C. ``procedure C is new G (Integer, Boolean);``
   D. ``procedure D is new G (Boolean, String);``

.. container:: animate

   :ada:`T1` must be discrete - so an integer or an enumeration. :ada:`T2` can be any type

=====================
Generic Formal Data
=====================

----------
Examples
----------

.. include:: examples/160_genericity/generic_formal_data.rst

:url:`https://learn.adacore.com/training_examples/fundamentals_of_ada/160_genericity.html#generic-formal-data`

--------------------------------------------
Generic Constants and Variables Parameters
--------------------------------------------

.. container:: columns

 .. container:: column
  
    * Variables can be specified on the generic contract
    * The mode specifies the way the variable can be used:

       - `in` |rightarrow| read only
       - `in out` |rightarrow| read write

    * Generic variables can be defined after generic types

 .. container:: column
  
    .. code:: Ada
    
       generic
          type T is private;
          X1 : Integer;  -- constant
          X2 : in out T; -- variable
       procedure P;
       
       V : Float;
       
       procedure P_I is new P
          (T  => Float,
           X1 => 42,
           X2 => V);
     
-------------------------------
Generic Subprogram Parameters
-------------------------------

* Subprograms may be defined as parameters
    
    - Using :ada:`with` to differ from the generic unit
    
   .. code:: Ada
          
      generic
         with procedure Callback;
      procedure P;
      procedure P is
      begin
         Callback;
      end P;
      procedure Something;
      procedure P_I is new P (Something);
     
----------------------------------------
Generic Subprogram Parameters Defaults
----------------------------------------

.. admonition:: Language Variant

   Ada 2005

* Default values can be specified

    - :ada:`is <>` - subprogram with **same name** in instanciation scope
    - :ada:`is null` - null subprogram

   .. code:: Ada
          
      generic
        with procedure Callback1 is <>;
        with procedure Callback2 is null;
      procedure P;
      procedure Callback1;
      procedure P_I is new P;
      -- takes Callback1 and null
     
----------------------------
Generic Package Parameters
----------------------------

* A generic unit can be using a generic package parameter

    - Package parameter can be **constrained** 
    - Partially or completely

.. code:: Ada

   generic
      type T1 is private;
      type T2 is private;
   package Base is [...]
   
   generic
      with package B is new Base (Integer, <>);
      V : B.T2;
   package Other [...]
   
   package Base_I is new Base (Integer, Float);
   
   package Other_I is new Other (Base_I, 56.7);
 
------
Quiz
------

.. code:: Ada

   package P is
      procedure P1 (X : in out Integer); -- add 100 to X
      procedure P2 (X : in out Integer); -- add 20 to X
      procedure P3 (X : in out Integer); -- add 3 to X
      generic
         Z : in out Integer;
         with procedure P1 (X : in out Integer) is <>;
         with procedure P2 (X : in out Integer) is null;
      procedure G;
   end P;

   package body P is
      -- bodies of P1/P2/P3 skipped for space
      procedure G is begin
         P1 (Z);
         P2 (Z);
      end G;
   end P;

Given an integer Z initialized to 100, what is the value of Z after calling I for each of the following instantiations?

.. list-table::

   * - :ada:`procedure I is new G (Z);`
     - :animate:`200 - Calls P1 and null`
   * - :ada:`procedure I is new G (Z, P1 => P3); `
     - :animate:`103 - Calls P3 and null`
   * - :ada:`procedure I is new G (Z, P2 => P3); `
     - :animate:`203 - Calls P1 and P3`
   * - :ada:`procedure I is new G (Z, P1 => P3, P2 => P3); `
     - :animate:`106 - Calls P3 twice`

====================
Generic Completion
====================

------------------------------
Implications for Distribution
------------------------------

* The body needs to be **visible** when compiling the user code
* Remember generics are **not** compiled or part of the program
* Distribution of a library with generics must include the generics source code

-----------------------------
Generic and Freezing Points
-----------------------------

.. container:: columns

 .. container:: column
  
    * A generic type instanciation "freezes" the type and needs to have access to its full view
    * May force **separation** between the generic type **declaration** and subsequent **instantiations** (e.g. with containers)

 .. container:: column
  
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

.. container:: columns

 .. container:: column
  
    * A generic type can be **incomplete**
    * Allows instantiations **before** full type definition
    * Usage of the type is then fairly restricted 
        
        - Can only be used through :ada:`access`


 .. container:: column
  
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

===================
Generic Subprograms
===================

-------------------
Generic Subprograms
-------------------

* Subprograms can also be defined as generics

.. code:: Ada
    
   package Helper is
      type Float_T is digits 6;
      generic
         type Type_T is digits <>;
         Min : Type_T;
         Max : Type_T;
      function In_Range_Generic (X : Type_T) return Boolean;
   end Helper;
     
   procedure User is
     type Speed_T is new Float_T range 0.0 .. 100.0;
     B : Boolean;
     function Valid_Speed is new In_Range_Generic
        (Speed_T, Speed_T'First, Speed_T'Last);
   begin
     B := Valid_Speed (12.3);
     
.. container:: speakernote

   Generics increase code size and readability
   Common functions reduce size, but increase error possibilities
     
========
Lab
========

.. include:: labs/160_genericity.lab.rst
 
=========
Summary
=========

---------
Summary
---------

* Generics are useful for copying **code patterns**

   - Sorting, containers, etc

* Properly written generics only need to be tested **once**

   - But testing / debugging can be more difficult

* Generic instantiations are best done at compile-time

   - At the package level
   - Can be **run-time expensive** when done in subprogram scope
