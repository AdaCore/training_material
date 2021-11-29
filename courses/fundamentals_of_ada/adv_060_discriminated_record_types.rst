
****************************
Discriminated Record Types
****************************

==============
Introduction
==============

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

------------------------------------
Example Variant Record Description
------------------------------------

* Record / structure type for a person

   + Person is either a student or a faculty member (discriminant)
   + Person has a name (string)
   + Each student has a GPA (floating point) and a graduation year (non-negative integer)
   + Each faculty has a count of publications (non-negative integer)

----------------------
Example Defined in C
----------------------

.. code:: C

   enum person_tag {Student, Faculty};

   struct Person {
      enum person_tag tag;
      char name [10];
      union {
         struct { float gpa; int year; } s;
         int pubs;
      };
   };

* Issue: maintaining consistency between tag and union fields is responsibility of the programmer

   + Source of potential vulnerabilities

------------------------
Example Defined in Ada
------------------------

.. code:: Ada

   type Person_Tag is (Student, Faculty);
   type Person (Tag : Person_Tag) is -- Tag is the discriminant
      record
         Name : String(1..10); -- Always present
         case Tag is
            when Student => -- 1st variant
               GPA  : Float range 0.0 .. 4.0;
               Year : Integer range 1..4;
            when Faculty => -- 2nd variant
               Pubs : Integer;
         end case;
      end record;

* `Tag` value enforces field availability

   + Can only access `GPA` and `Year` when `Tag` is `Student`
   + Can only access `Pubs` when `Tag` is `Faculty`

------------------------
Variant Part of Record
------------------------

* Variant part of record specifies alternate list of componenents

   .. code:: Ada

      type Variant_Record_T (Discriminant : Integer) is record
         Common_Component : String (1 .. 10);
         case Discriminant is
            when Integer'First .. -1 =>
               Negative_Component : Float;
            when 1 .. Integer'Last =>
               Positive_Component : Integer;
            when others =>
               Zero_Component : Boolean;
         end case;
      end record;

   * Choice is determined by discriminant value
   * Record can only contain one variant part

      - Variant must be last part of record definition

==========================
Variant Record Semantics
==========================

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

----------------
Implementation
----------------

* Typically type and operations would be treated as an ADT

   + Implemented in its own package

   .. code:: Ada

      package Person_Pkg is
         type Person_Tag is (Student, Faculty);
         type Person (Tag : Person_Tag) is
            record
               Name : String(1..10);
               case Tag is
                  when Student =>
                     GPA  : Float range 0.0 .. 4.0;
                     Year : Integer range 1..4;
                  when Faculty =>
                     Pubs : Integer;
               end case;
            end record;
         -- parameters can be unconstrained (constraint comes from caller)
         procedure Put ( Item : in Person );
         procedure Get ( Item : out Person );
      end Person_Pkg;

----------------
Primitives
----------------

* Output

   .. code:: Ada

      procedure Put ( Item : in Person ) is
      begin
        Put_Line("Tag:" & Person_Tag'Image(Item.Tag));
        Put_Line("Name: " & Item.Name );
        -- Tag specified by caller
        case Item.Tag is
          when Student =>
            Put_Line("GPA:" & Float'Image(Item.GPA));
            Put_Line("Year:" & Integer'Image(Item.Year) );
          when Faculty =>
            Put_Line("Pubs:" & Integer'Image(Item.Pubs) );
        end case;
      end Put;

* Input

   .. code:: Ada

      procedure Get ( Item : out Person ) is
      begin
        -- Tag specified by caller
        case Item.Tag is
          when Student =>
            Item.GPA := Get_GPA;
            Item.Year := Get_Year;
          when Faculty =>
            Item.Pubs := Get_Pubs;
        end case;
      end Get;

-------
Usage
-------

.. code:: Ada

   with Person_Pkg; use Person_Pkg;
   with Ada.Text_IO; use Ada.Text_IO;
   procedure Person_Test is
     Tag   : Person_Tag;
     Line  : String(1..80);
     Index : Natural;
   begin
     loop
       Put("Tag (Student or Faculty, empty line to quit): ");
       Get_Line(Line, Index);
       exit when Index=0;
       Tag := Person_Tag'Value(Line(1..Index));
       declare
         Someone : Person(Tag);
       begin
         Get(Someone);
         case Someone.Tag is
           when Student => Student_Do_Something ( Someone );
           when Faculty => Faculty_Do_Something ( Someone );
         end case;
         Put(Someone);
       end;
     end loop;
   end Person_Test;

===============================
Unconstrained Variant Records
===============================

---------------------------------------
Adding Flexibility to Variant Records
---------------------------------------

* Previously, declaration of `Person` implies that object, once created, is always constrained by initial value of `Tag`

   + Assigning `Person(Faculty)` to `Person(Student)` or vice versa, raises Constraint_Error

* Additional flexibility is sometimes desired

   + Allow declaration of unconstrained `Person`, to which either `Person(Faculty)` or `Person(Student)` can be assigned
   + To do this, *declare discriminant with default initialization*

* Type safety is not compromised

   + Modification of discriminant is only permitted when entire record is assigned

      * Either through copying an object or aggregate assignment

--------------------------------------
Unconstrained Variant Record Example
--------------------------------------

.. code:: Ada

   declare
      type Mutant( Tag : Person_Tag := Faculty ) is
         record
            Name : String(1..10);
            case Tag is
               when Student =>
                  GPA  : Float range 0.0 .. 4.0;
                  Year : Integer range 1..4;
               when Faculty =>
                  Pubs : Integer;
            end case;
         end record;

      Pat  : Mutant( Student ); -- Constrained
      Doc  : Mutant( Faculty ); -- Constrained
      Zork : Mutant; -- Unconstrained (Zork.Tag = Faculty)

   begin
      Zork     := Pat;     -- OK, Zork.Tag was Faculty, is now Student
      Zork.Tag := Faculty; -- Illegal to assign to discriminant
      Zork     := Doc;     -- OK, Zork.Tag is now Faculty
      Pat      := Zork;    -- Run-time error (Constraint_Error)
   end;

=======================
Varying Length Arrays
=======================

----------------------------------
Varying Lengths of Array Objects
----------------------------------

* In Ada, array objects have to be fixed length

   .. code:: Ada

      S : String(1..80);
      A : array ( M .. K*L ) of Integer;

* We would like an object with a maximum length, but current length is variable

   + Need two pieces of data

      * Array contents
      * Location of last valid element

* For common usage, we want this to be a type (probably a record)

   + Maximum size array for contents
   + Index for last valid element

-----------------------------
Simple Varying Length Array
-----------------------------

.. code:: Ada

   type Simple_VString is
      record
         Length : Natural range 0..Max_Length := 0;
         Data   : String(1..Max_Length) := (others => ' ');
      end record;

   function "&"(Left, Right : Simple_VString) return Simple_VString is
      Result : Simple_VString;
   begin
      if Left.Length + Right.Length > Max_Length then
         raise Constraint_Error;
      else
         Result.Length := Left.Length + Right.Length;
         Result.Data(1..Result.Length) :=
            Left.Data(1..Left.Length) & Right.Data(1..Right.Length);
         return Result;
      end if;
   end "&";

* Issues

   + Every object has same maximum length
   + `Length` needs to be maintained by program logic
   + Need to define "="

------------------------------------------
Varying Length Array via Variant Records
------------------------------------------

* Discriminant can serve as bound of array field

.. code:: Ada

   type VString ( Max_Length : Natural := 0 ) is
     record
       Data   : String(1..Max_Length) := (others => ' ');
     end record;

* Discriminant default value?

   * With default discriminant value, objects can be copied even if lengths are different
   * With no default discriminant value, objects of different lengths cannot be copied

========================
Variant Record Details
========================

------------------------------------
Semantics of Discriminated Records
------------------------------------

* A discriminant is a parameter to a record type

   - The value of a discriminant affects the presence, constraints, or initialization of other components

* A type may have more than one discriminant

   - Either all have default initializations, or none do

* Ada restricts the kinds of types that may be used to declare a discriminant

   - Discrete types (i.e., enumeration or integer type)
   - Access types (not covered here)

-------------------------------------------
Use of Discriminants in Record Definition
-------------------------------------------

* Within the record type definition, a discriminant may only be referenced in the following contexts

   - In "case" of variant part
   - As a bound of a record component that is an unconstrained array
   - As an initialization expression for a component
   - As the value of a discriminant for a component that itself a variant record

* A discriminant is not allowed as the bound of a range constraint

========
Lab
========

.. include:: labs/adv_060_discriminated_record_types.lab.rst

=========
Summary
=========

------------------------------------
Properties of Variant Record Types
------------------------------------

* Rules

   - Case choices for variants must partition possible values for discriminant
   - Field names must be unique across all variants

* Style

   - Typical processing is via a case statement that "dispatches" based on discriminant
   - This centralized functional processing is in contrast to decentralized object-oriented approach

* Flexibility

   - Variant parts may be nested, if some fields common to a set of variants

