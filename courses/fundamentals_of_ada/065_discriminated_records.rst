***********************
Discriminated Records
***********************

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

----------------------------
Discriminated Record Types
----------------------------

* :dfn:`Discriminated record` type

   + Different **objects** may have **different** components and/or different sizes
   + All objects **still** share the same type

* Similar to :C:`union` in C

   + But preserves **type checking**

      * Except in the case of an :ada:`Unchecked_Union` (seen later)

   + And object size **is related to** discriminant
    
* Aggregate assignment is allowed

   + Provided constraints are correct

---------------------------------
Defining a Discriminated Record
---------------------------------

* Record type with a :dfn:`discriminant` 

   * **Discriminant** controls behavior of the record
   * Part of record definition
   * Can be read as any other field

      * But can only be modified by object assignment (sometimes)

* Sample definitions (completions appear later in this module)

.. container:: latex_environment small

   .. code:: Ada

      type Employee_T (Kind : Category_T) is record ...
      type Mutable_T (Kind : Category_T := Employee) is record ...
      type Vstring (Last : Natural := 0) is record ...
      type C_Union_T (View : natural := 0) is record ...

=================
Variant Records
=================

---------------------------
What is a Variant Record?
---------------------------

* A :dfn:`variant record` uses the discriminant to determine which fields are currently accessible

   .. code:: Ada

      type Category_T is (Employee, Contractor);
      type Employee_T (Kind : Category_T) is record
         Name : String_T;
         DOB  : Date_T;
         case Kind is
            when Employee =>
               Pay_Rate  : Pay_T;
            when Contractor =>
               Hourly_Rate : Contractor_Rate_T;
         end case;
      end record;

      An_Employee     : Employee_T (Employee);
      Some_Contractor : Employee_T (Contractor);

* Note that the :ada:`case` block must be the last part of the record definition

   * Therefore only one per record

* Variant records are considered the same type

   * So you can have

      .. code:: Ada

         procedure Print (Item : Employee_T);

         Print (An_Employee);
         Print (Some_Contractor);

--------------------------
Immutable Variant Record
--------------------------

* In an :dfn:`immutable variant record` the discriminant has no default value

   * It is an :dfn:`indefinite type`, similar to an unconstrained array

     * So you must add a constraint (discriminant) when creating an object
     * But it can be unconstrained when used as a parameter

* For example

   .. code:: Ada
      :number-lines: 24

      Pat     : Employee_T (Employee);
      Sam     : Employee_T :=
         (Kind        => Contractor,
          Name        => From_String ("Sam"),
          DOB         => "2000/01/01",
          Hourly_Rate => 123.45);
      Illegal : Employee_T;  -- indefinite

--------------------------------
Immutable Variant Record Usage
--------------------------------

* Compiler can detect some problems

   .. code:: Ada

      begin
         Pat.Hourly_Rate := 12.3;
      end;

   ``warning: component not present in subtype of "Employee_T" defined at line 24``

* But more often clashes are run-time errors

   .. code:: Ada
     :number-lines: 32

     procedure Print (Item : Employee_T) is
     begin
       Print (Item.Pay_Rate);

   ``raised CONSTRAINT_ERROR : print.adb:34 discriminant check failed``
  
* :ada:`Pat := Sam;` would be a compiler warning because the constraints do not match

------------------------
Mutable Variant Record
------------------------

* To add flexibility, we can make the type :dfn:`mutable` by specifying a default value for the discriminant

   .. code:: Ada

      type Mutable_T (Kind : Category_T := Employee) is record
         Name : String_T;
         DOB  : Date_T;
         case Kind is
            when Employee =>
               Pay_Rate  : Pay_T;
            when Contractor =>
               Hourly_Rate : Contractor_Rate_T;
      end record;

      Pat : Mutable_T;
      Sam : Mutable_T (Contractor);

* Making the variant mutable creates a definite type

   * An object can be created without a constraint (:ada:`Pat`)
   * Or we can create in immutable object where the discriminant cannot change (:ada:`Sam`)
   * And we can create an array whose element is mutable

--------------------------------
Mutable Variant Record Example
--------------------------------

* We can change the discriminant of a mutable object

  * But only via a copy / aggregate assignment

  .. code:: Ada

    if Pat.Kind = Contractor then
      Pat := (Employee, Pat.Name, Pat.Age, 12.34);
    else
      Pat := Sam;
    end if;

* But you cannot change the discriminant like a regular field

  .. code:: Ada

    Pat.Kind := Contractor; -- compile error

  ``error: assignment to discriminant not allowed``
    
* And you cannot change the discriminant of :ada:`Sam`

  * :ada:`Sam := Pat;` will give you a runtime error if :ada:`Pat.Kind` is not :ada:`Contractor`

    * And the compiler will not warn about this!

------
Quiz
------

.. code:: Ada

    type Variant_T (Sign : Integer) is record
        case Sign is
        when Integer'First .. -1 =>
            I : Integer;
            B : Boolean;
        when others =>
            N : Natural;
        end case;
    end record;

    Variant_Object : Variant_T (1);

Which component(s) does :ada:`Variant_Object` contain?

A. :ada:`Variant_Object.I, Variant_Object.B`
B. :answermono:`Variant_Object.N`
C. None: Compilation error
D. None: Runtime error

------
Quiz
------

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada
         :number-lines: 2

         type Coord_T is record
            X, Y : Float;
         end record;

         type Kind_T is (Circle, Line);
         type Shape_T (Kind : Kind_T := Line) is record
            Origin : Coord_T;
            case Kind is
               when Line =>
                  End_Point : Coord_T;
               when Circle =>
                  End_Point : Coord_T;
            end case;
         end record;

         A_Circle : Shape_T       :=
           (Circle, (1.0, 2.0), (3.0, 4.0));
         A_Line   : Shape_T (Line) :=
           (Circle, (1.0, 2.0), (3.0, 4.0));

  .. container:: column

    .. container:: latex_environment small

      What happens when you try to build and run this code?

      A. Runtime error
      B. Compilation error on an object
      C. :answer:`Compilation error on a type`
      D. No problems

.. container:: animate

  .. container:: latex_environment footnotesize

   * If you fix the compilation error (by changing the name of one of the :ada:`End_Point` fields), then

      * You would get a warning on line 20 (because :ada:`A_Line` is constrained to be a :ada:`Line`

         ``incorrect value for discriminant "Kind"``

      * If you then ran the executable, you would get an exception 

         ``CONSTRAINT_ERROR : test.adb:20 discriminant check failed``
   
======================================
Discriminant Record Array Size Idiom
======================================

----------------------------
Vectors of Varying Lengths
----------------------------

* In Ada, array objects have to be fixed length

   .. code:: Ada

      S : String (1..80);
      A : array (M .. K*L) of Integer;

* We would like an object with a maximum length and a variable current length

   + Like a queue or a stack
   + Need two pieces of data

      * Array contents
      * Location of last valid element

* For common usage, we want this to be a type (probably a record)

   + Maximum size array for contents
   + Index for last valid element

---------------------------------
Simple Vector of Varying Length
---------------------------------

* Not unconstrained - we have to define a maximum length to make it a :dfn:`definite type`

.. code:: Ada

   type Simple_Vstring is
      record
         Last : Natural range 0 .. Max_Length := 0;
         Data : String (1 .. Max_Length) := (others => ' ');
      end record;

   Obj1 : Simple_Vstring := (0, (others => '-'));
   Obj2 : Simple_Vstring := (0, (others => '+'));
   Obj3 : Simple_Vstring;

* Issue - Operations need to consider :ada:`Last` field

   * :ada:`Obj1 = Obj2` will be false
   * Can redefine :ada:`=` to be something like

      .. code:: Ada

         if Obj1.Data (1..Obj1.Last) = Obj2.Data (1..Obj2.Last)

   * Same thing with concatentation

      .. code:: Ada

         Obj3.Last := Obj1.Last + Obj2.Last;
         Obj3.Data (1..Obj3.Last) := Obj1.Data (1..Obj1.Last) &
                                     Obj2.Data (1..Obj2.Last)
* Other Issues

   + Every object has same maximum length
   + ``Last`` needs to be maintained by program logic

------------------------------------------------
Varying Length Array via Discriminated Records
------------------------------------------------

* Discriminant can serve as bound of array component

   .. code:: Ada

      type Vstring (Last : Natural := 0) is
        record
          Data   : String (1..Last) := (others => ' ');
        end record;

* Mutable objects vs immutable objects

   + With default discriminant value (mutable), objects can be copied even if lengths are different
   + With no default discriminant value (immutable), objects of different lengths cannot be copied (and we can't change the length)

-----------------
Object Creation
-----------------

* When a mutable object is created, runtime assumes largest possible value

   + So this example is a problem

      .. code:: Ada

         type Vstring (Last : Natural := 0) is record
            Data   : String (1..Last) := (others => ' ');
         end record;

         Good : Vstring (10);
         Bad  : Vstring;

      + Compile warning: ``warning: creation of "Vstring" object may raise Storage_Error``

      + Runtime error: ``raised STORAGE_ERROR : EXCEPTION_STACK_OVERFLOW`` 

* Better implementation

   .. code:: Ada

      subtype Length_T is natural range 0 .. 1_000;
      type Vstring (Last : Length_T := 0) is record
         Data   : String (1..Last) := (others => ' ');
      end record;

      Good      : Vstring (10);
      Also_Good : Vstring;

------------------------
Simplifying Operations
------------------------

* With mutable discriminated records, operations are simpler

   .. code:: Ada

      Obj : Simple_Vstring;
      Obj1 : Simple_Vstring := (6, " World");

   * Creation

      .. code:: Ada

         function Make (S : String) return Vstring is (S'length, S);
         Obj2 : Simple_Vstring := Make ("Hello");

   * Equality: :ada:`Obj1 = Obj2`

      * :ada:`Data` is exactly the correct length
      * if :ada:`Data` or :ada:`Last` is different, equality fails

   * Concatentation

      .. code:: Ada

         Obj := (Obj1.Last + Obj2.Last,
                 Obj1.Data & Obj2.Data);

------
Quiz
------

.. include:: quiz/mutable_with_array/quiz.rst

====================
Interfacing with C
====================

-----------------------------------
Passing Records Between Ada and C
-----------------------------------

* Your Ada code needs to call C that looks like this:

   .. code:: C

      struct Struct_T {
         int   Field1;
         char  Field2;
         float Field3;
      };

      int DoSomething (struct Struct_T);

* Ada has mechanisms that will allow you to 

   * Call :C:`DoSomething`
   * Build a record that is binary-compatible to :C:`Struct_T`

--------------------------------
Building a C-Compatible Record
--------------------------------

* To build an Ada record for :C:`Struct_T`, start with a regular record:

   .. code:: Ada

      type Struct_T is record
         Field1 : Interfaces.C.int;
         Field2 : Interfaces.C.char;
         Field3 : Interfaces.C.C_Float;
      end record;

   * We use types from :ada:`Interfaces.C` to map directly to the C types

* But the Ada compiler needs to know that the record layout must match C

   * So we add an aspect to enforce it

   .. code:: Ada

      type Struct_T is record
         Field1 : Interfaces.C.int;
         Field2 : Interfaces.C.char;
         Field3 : Interfaces.C.C_Float;
      end record with Convention => C_Pass_By_Copy;

-------------------------
Mapping Ada to C Unions
-------------------------

* Discriminant records are similar to C's :c:`union`, but with a limitation

   * Only one part of the record is available at any time

* So, you create the equivalent of this C :c:`union`

   .. code:: C

      union Union_T {
         int Field1;
         char Field2;
         float Field3;
      };

* By using a discriminant record and adding aspect :ada:`Unchecked_Union`

   .. code:: Ada

      type C_Union_T (View : natural := 0) is record
         case View is
         when 0 => Field1 : Interfaces.C.int;
         when 1 => Field2 : Interfaces.C.char;
         when 2 => Field3 : Interfaces.C.C_Float;
         when others => null;
         end case;
      end record with Convention => C_Pass_By_Copy,
                      Unchecked_Union;

   * This tells the compiler not to reserve space in the record for the discriminant

------
Quiz
------

.. code:: C

   union Union_T {
      struct Record_T field1;
      char            field2[11];
      float           field3;
   };

.. code:: Ada

    type C_Union_T (Flag : Natural := 1) is record
        case Sign is
        when 1 =>
            One   : Record_T;
        when 2 =>
            Two   : String(1..11);
        when 3 =>
            Three : Float;
        end case;
    end record;

    C_Object : C_Union_T;

Which component does :ada:`C_Object` contain?

   A. :ada:`C_Object.One`
   B. :ada:`C_Object.Two`
   C. :answer:`None: Compilation error`
   D. None: Runtime error

.. container:: animate

    The variant :ada:`case` must cover all the possible values of :ada:`Natural`.

========
Lab
========

.. include:: labs/065_discriminated_records.lab.rst

=========
Summary
=========

------------------------------------------
Properties of Discriminated Record Types
------------------------------------------

* Rules

   - Case choices for variants must partition possible values for discriminant
   - Field names must be unique across all variants

* Style

   - Typical processing is via a case statement that "dispatches" based on discriminant
   - This centralized functional processing is in contrast to decentralized object-oriented approach
