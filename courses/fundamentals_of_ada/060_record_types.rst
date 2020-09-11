
**************
Record Types
**************

==============
Introduction
==============

---------------------
Syntax (Simplified)
---------------------

.. code:: Ada

   record_definition ::= record component_list end record |
                         null record
   component_list ::= component_declaration
      {component_declaration }  |
      {component_declaration } variant_part |
      null;
   component_declaration ::=
      defining_identifier_list : component_definition
          [:= default_expression];
 
==================
Components Rules
==================

-------------------------------
Characteristics of Components
-------------------------------

* Heterogeneous types allowed
* Referenced by name
* May be no components, for empty records
* No anonymous types (e.g., arrays) allowed
* No constant components
* Multiple component declarations allowed
* No recursive definitions
* Default initial expressions allowed

-----------------------------------------
"Dot" Notation for Components Reference
-----------------------------------------

.. code:: Ada

      type Months_T is (January, February, ..., December);
      type Date is
         record
            Day : Integer range 1 .. 31;
            Month : Months_T;
            Year : Integer range 0 .. 2099;
         end record;
      Arrival : Date;
   begin
     Arrival.Day := 27;  -- components referenced by name
     Arrival.Month := November;
     Arrival.Year := 1990;
 
------------------------------
Anonymously-Typed Components
------------------------------

* Not allowed
* No type name so no compatibility check is possible

   .. code:: Ada
      
      type Illegal is
        record
          A : array (Foo) of Bar;
        end record;
      X, Y : Illegal;
 
   - Cannot perform `X.A := Y.A`

---------------------
Constant Components
---------------------

* Not allowed
* Assignment would allow altering constants
* Constant record objects (not components) are allowed

   .. code:: Ada

      type Illegal is
        record
          A : constant Foo := F(X);
        end record;
      X, Y : Illegal;
 
   - Cannot perform `X.A := Y.A;`

-------------------------
More Component Rules...
-------------------------

* Multiple declarations are allowed (like objects)

   .. code:: Ada

      type Several is 
        record
          A, B, C : Integer;
        end record;
 
* Recursive definitions are not allowed

   .. code:: Ada

      type Not_Legal is
        record
          A, B : Some_Type;
          C : Not_Legal;
        end record;
 
============
Operations
============

----------------------
Available Operations
----------------------

* Predefined

   - Equality (and thus inequality) 

      .. code:: Ada

         if A = B then
 
   - Assignment 

      .. code:: Ada

         A := B;
 
   - Component-level operations

      + Based on components' types

         .. code:: Ada

            if A.component < B.component then
 
* User-defined

   - Subprograms

---------------------
Assignment Examples
---------------------

.. code:: Ada

     type Complex is record
         Real : Float;
         Imaginary : Float;
       end record;
     ...
     Phase1 : Complex;
     Phase2 : Complex;
   begin
     ...
       -- object reference
      Phase1 := Phase2;  -- entire object reference
      -- component references
      Phase1.Real := 2.5;
      Phase1.Real := Phase2.Real;
   end;
 
-------------------------------
Referencing Nested Components
-------------------------------

.. code:: Ada

     type Date is ....  -- as before
     type Personal_Information is record
         Name : String(1..80);
         Birth : Date;
       end record;
     type Employee_Information is record
         Number : Employee_Number;
         Personal_Data : Personal_Information;
       end record;
     Employee : Employee_Information;
   begin
     ...
      Employee.Personal_Data.Birth.Month := March;
 
============
Aggregates
============

------------
Aggregates
------------

* Literal values for composite types

   - As for arrays

* Syntax (simplified):

   .. code:: Ada

      record_aggregate ::= (record_component_association_list)
      
      record_component_association_list ::= 
         record_component_association
         { , record_component_association}
      
      record_component_association ::= 
         [component_choice_list =>] expression
      
      component_choice_list ::= 
         component_selector_name { | component_selector_name}
         | others
 
---------------------------
Record Aggregate Examples
---------------------------

.. code:: Ada

   procedure Test is
     type Complex is
       record
         Real      : Float;
         Imaginary : Float;
       end record;
     Phase : Complex := (0.0, 0.0);
   begin
     Phase := (10.0, Imaginary => 2.5);
     Phase := (Imaginary => 12.5, Real => 0.212);
 
------------------------
Aggregate Completeness
------------------------

.. container:: columns

 .. container:: column
  
    * All component values must be accounted for

       - Including defaults via ``box``

    * Allows compiler to check for missed components
    * Type definition
    
       .. code:: Ada
    
          type Struct is record
              A : Integer;
              B : Integer;
              C : Integer;
              D : Integer;
            end record;
          S : Struct;
     
 .. container:: column
  
    * Compiler will not catch the missing component
    
       .. code:: Ada
    
          S.A := 10;
          S.B := 20;
          S.C := 12;
          Send (S);
     
    * Aggregate must be complete - compiler error
    
       .. code:: Ada
    
          S := (10, 20, 12);
          Send (S);
     
--------------------
Named Associations
--------------------

.. container:: columns

 .. container:: column
  
    * Allows any order of associations

       - Don't have to remember the order
       - Less likely to mix up associations of the same type

    * Provides more information to the reader

       - May be mixed with positional form

    * Restriction

       - Must stick with named associations once begun

 .. container:: column
    
    .. code:: Ada
    
       type Complex is record
           Real : Float;
           Imaginary : Float;
         end record;
       Phase : Complex := (0.0, 0.0);
       ...
       Phase := (10.0,
                 Imaginary => 2.5);
       Phase := (Imaginary => 12.5,
                 Real => 0.212);
       Phase := (Imaginary => 12.5,
                 0.212); -- illegal

.. container:: speakernote

   No positional notation after named notation

-------------------
Nested Aggregates
-------------------

* Result from composite component types

   .. code:: Ada
    
     type Months_T is ( January, February, ..., December);
     type Date is record
         Day   : Integer range 1 .. 31;
         Month : Months_T;
         Year  : Integer range 0 .. 2099;
       end record;
     type Person is record
         Born : Date;
         Hair : Color;
       end record;
     John : Person    := ( (21, November, 1990), Brown );
     Julius : Person  := ( (2, August, 1995), Blond );
     Heather : Person := ( (2, March, 1989), Hair => Blond );
     Megan : Person   := (Hair => Blond, 
                          Born => (16, December, 2001));
     
------------------------------------
Aggregates with Only One Component
------------------------------------

* Must use named form

   - Since syntax for expressions includes same tokens

* Same as array aggregates, for same reason

.. code:: Ada

   type Singular is
     record
       A : Integer;
     end record;
   S : Singular := (3);          -- illegal
   S : Singular := (3 + 1);      -- illegal
   S : Singular := (A => 3 + 1); -- required
 
--------------------------
Aggregates with `others`
--------------------------

* Indicates all components not yet specified (like arrays)
* Since all `others` get the same value, all such components must be the same type

.. code:: Ada

   type Poly is
     record
       A : Real;
       B, C, D : Integer;
     end record;
   P : Poly := (2.5, 3, others => 0);
   type Homogeneous is
     record
       A, B, C : Integer;
     end record;
   Q : Homogeneous := (others => 10);
 
================
Default Values
================

--------------------------
Component Default Values
--------------------------

.. code:: Ada

   type Complex is
     record
       Real : Real := 0.0;
       Imaginary : Real := 0.0;
     end record;
   -- all components use defaults
   Phasor : Complex;
   -- all components must be specified
   I : constant Complex := (0.0, 1.0);
 
------------------------------------
Default Component Value Evaluation
------------------------------------

* Occurs when object is elaborated

   - Not when the type is elaborated

* Not evaluated if explicitly overridden

.. code:: Ada

   type Structure is
     record
       A : Integer;
       R : Time := Clock;
     end record;
   -- Clock is called for S1
   S1 : Structure;
   -- Clock is not called for S2
   S2 : Structure := (A => 0, R => Yesterday);
 
-----------------------------------
Defaults Within Record Aggregates
-----------------------------------

.. admonition:: Language Variant

   Ada 2005

* Specified via the ``box`` notation
* Value for the component is thus taken as for a stand-alone object declaration

   - So there may or may not be a defined default!

* Can only be used with "named association" form

   - But can mix forms, unlike array aggregates

.. code:: Ada

   type Complex is
     record
       Real : Float := 0.0;
       Imaginary : Float := 0.0;
     end record;
   Phase := (42.0, Imaginary => <>);
 
------------------------------------------
Default Initialization Via Aspect Clause
------------------------------------------

.. admonition:: Language Variant

   Ada 2012

* Not definable for entire record type
* Components of scalar types take type's default if no explicit default value specified by record type

.. code:: Ada

   type Toggle_Switch is (Off, On)
       with Default_Value => Off;
   type Controller is record
       -- Off unless specified during object initialization
       Override : Toggle_Switch;
       -- default for this component
       Enable : Toggle_Switch := On;
     end record;
   C : Controller; -- Override => off, Enable => On
   D : Controller := (On, Off); -- All defaults replaced
 
==========
Examples
==========

--------------------
Game Board Example
--------------------

.. code:: Ada

   type Position is record
       Row : Natural := 0;
       Column : Natural := 0;
     end record;
   type Offset is record
       Row : Integer range -1 .. 1;
       Col : Integer range -1 .. 1;
     end record;
   type Deltas is (Left, Right, Up, Down);
   Offsets : constant array (Deltas)
     of Offset := ( Left  => (0,-1), 
                    Right => (0,+1),
                    Down  => (+1,0),
                    Up    => (-1,0) );

.. list-table::
   :header-rows: 1
   :stub-columns: 1

  * - 

    - *1*
    - *2*
    - *3*

  * - *1*

    - 
    - **-1, 0**
    - 

  * - *2*

    - **0, -1**
    - **Current Position**
    - **0, +1**

  * - *3*

    - 
    - **+1, 0**
    - 
  
    
--------------
Date Example
--------------

.. code:: Ada

     End_of_Month : array (Months) of Days :=
                   (Sep | Apr | Jun | Nov => 30,
                    Feb => 28,
                    others => 31);
   begin
      if (Today.Year mod 4 = 0 and Today.Year mod 100 /= 0)
         or else
         (Today.Year mod 400 = 0)
      then 
         End_of_Month (Feb) := 29; -- adjust for leap year
      end if;
      if Today.Day /= End_of_Month (Today.Month) then
         Today.Day := Today.Day + 1;
      else -- wrap around day of month
         Today.Day := 1;
         if Today.Month /= Dec then
            Today.Month := Months'Succ (Today.Month);
         else -- wrap around month
            Today.Month := Jan;
            Today.Year := Today.Year + 1;
         end if;
      end if;
 
.. container:: speakernote

   Better way to set EndOfMonth will be discussed elsewhere

========
Lab
========

.. include:: labs/060_record_types.lab.rst

=========
Summary
=========

---------
Summary
---------

* Heterogeneous types allowed for components
* Default initial values allowed for components

   - Evaluated when each object elaborated, not the type
   - Not evaluated if explicit initial value specified

* Aggregates express literals for composite types
* Aggregates can mix named and positional forms
