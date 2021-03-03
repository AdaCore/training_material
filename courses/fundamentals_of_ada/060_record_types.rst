
**************
Record Types
**************

.. |rightarrow| replace:: :math:`\rightarrow`

.. role:: ada(code)
   :language: ada

.. role:: C(code)
   :language: C

.. role:: cpp(code)
   :language: C++

==============
Introduction
==============

---------------------
Syntax and Examples
---------------------

* Syntax (simplified)

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

* Examples

   .. code:: Ada

      type Record1_T is record
         Field1 : integer;
         Field2 : boolean;
      end record;

      type Record2_T ( Size : natural := 0 ) is record
         Text : string(1..Size);
      end record;
      
 
==================
Components Rules
==================

----------
Examples
----------

.. include:: examples/060_record_types/components_rules.rst

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

   declare
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
 
------
Quiz
------

Which component definition is legal?

.. code:: Ada

   type Record_T is record

A. Component1 : array ( 1 .. 3 ) of boolean;
B. :answer:`Component2, Component3 : integer;`
C. Component4 : Record_T;
D. Component5 : constant integer := 123;

.. code:: Ada

   end record;

.. container:: animate

   Explanations

   A. Anonymous types not allowed
   B. Correct
   C. No recursive definitions
   D. No constant components

============
Operations
============

----------
Examples
----------

.. include:: examples/060_record_types/operations.rst

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

   declare
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

----------
Examples
----------

.. include:: examples/060_record_types/aggregates.rst

------------
Aggregates
------------

* Literal values for composite types

   - As for arrays
   - Default value / selector: :ada:`<>`, :ada:`others`

* Can use both **named** and **positional**

    - Unambiguous

* Syntax (simplified):

   .. code:: Ada

      component_init ::= expression | <>

      record_aggregate ::=
         {[component_choice_list =>] component_init ,}
         [others => component_init]

* Example

   .. code:: Ada

        V : Car_T := (
            Red,
            Plate_No => "AX672",
            others => <>
        );
 
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
 
------
Quiz
------

.. code:: Ada

   type Nested_T is record
      Field : Integer := 1_234;
   end record;
   type Record_T is record
      One   : Integer := 1;
      Two   : Character;
      Three : Boolean;
      Four  : Integer := -1;
      Five  : Nested_T;
   end record;
   X, Y : Record_T;
   Z    : constant Nested_T := (others => -1);

Which assignment is illegal?

A. :answer:`X := (1, '2', Three => True, Four => 4, Five => (6));`
B. X := (Two => '2', Three => False, Five => Z, others => 5);
C. X := Y;
D. X := (1, '2', True, 4, (others => 5));

.. container:: animate

   Explanations

   A. Component :ada:`Five` is a singleton record - aggregate requires named notation (:ada:`Five => ( Field => 6 )` )
   B. Correct - :ada:`others` clause covers components :ada:`One` and :ada:`Four` which are both integers`
   C. Correct - simple assignment. Note that components :ada:`Two` and :ada:`Three` are still not initialized
   D. Correct - positional notation for all components

================
Default Values
================

----------
Examples
----------

.. include:: examples/060_record_types/default_values.rst

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
 
------
Quiz
------

.. code:: Ada

   function Next return Natural; -- returns next number starting with 1

   declare
      type Record_T is record
         A, B : Integer := Next;
         C    : Integer := Next;
      end record;
      R : Record_T := (C => 100, others => <>);
   begin
      Put_Line (Integer'Image (R.A) & Integer'Image (R.B) & Integer'Image (R.C));
   end;

What is the output from this block?

A. 1 2 3
B. 1 1 2
C. :answer:`1 2 100`
D. 100, 101, 102

.. container:: animate

   Explanations

   A. Assignment of :ada:`C` to 100 takes precedence over the call to :ada:`Next`
   B. Declaration of multiple components is identical to a series of single declarations
   C. Correct
   D. Assignment of 100 to :ada:`C` has no effect on components :ada:`A` and :ada:`B`

=================
Variant Records
=================

----------
Examples
----------

.. include:: examples/060_record_types/variant_records.rst

----------------------
Variant Record Types
----------------------

* *Variant record type* is a record type where

   + Different objects may have different sets of components (i.e. different variants)
   + Given object itself may be *unconstrained*

      * Different variants at different times

* Supported in other languages

   + Variant records in Pascal
   + Unions in C

* Variant record offers a kind of storage overlaying

   + Same storage might be used for one variant at one time, and then for another variant later
   + Language issue: Ensure this does not provide loophole from type checking

      * Neither Pascal nor C avoids this loophole

-------------------------------------
Discriminant in Ada Variant Records
-------------------------------------

* Variant record type contains a special field (*discriminant*) whose value indicates which variant is present
* When a field in a variant is selected, run-time check ensures that discriminant value is consistent with the selection

   + If you could store into `Pubs` but read `GPA`, type safety would not be guaranteed

* Ada prevents this type of access

   + Discriminant (Tag) established when object of type Person created
   + Run-time check verifies that field selected from variant is consistent with discriminant value

      * Constraint_Error raised if the check fails

* Can only read discriminant (as any other field), not write

      * Aggregate assignment is allowed

-----------
Semantics
-----------

* Variable of type `Person` is constrained by value of discriminant supplied at object declaration

   + Determines minimal storage requirements
   + Limits object to corresponding variant

   .. code:: Ada

      Pat  : Person(Student); -- May select Pat.GPA, not Pat.Pubs
      Prof : Person(Faculty); -- May select Prof.Pubs, not Prof.GPA
      Soph : Person := ( Tag  => Student, 
                         Name => "John Jones", 
                         GPA  => 3.2, 
                         Year => 2);
      X    : Person;  -- Illegal; discriminant must be initialized

* Assignment between Person objects requires same discriminant values for LHS and RHS 

   .. code:: Ada

      Pat  := Soph; -- OK
      Soph := Prof; -- Constraint_Error at run time

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
