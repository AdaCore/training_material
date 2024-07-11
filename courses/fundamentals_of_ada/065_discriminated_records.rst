****************************
Discriminated Record Types
****************************

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

   .. code:: Ada

      type Employee_T (Kind : Category_T) is record ...
      type Student_T (Grad : Boolean := False) is record ...
      type Stack_T (Max_Size : positive) is record ...
      type C_Union_T (View : integer) is record ...

=================
Variant Records
=================

---------------------------
What is a Variant Record?
---------------------------

* :dfn:`Variant record` uses the discriminant to determine which fields are currently *active*

.. code:: Ada

   type Category_T is (Applicant, Employee, Contractor);
   type Employee_T (Kind : Category_T) is record
      Name : String_T;
      DOB  : Date_T;
      case Kind is
         when Applicant =>
            null;
         when Employee =>
            Pay_Rate  : Pay_T;
         when Contractor =>
            Hourly_Rate : Contractor_Rate_T;
   end record;

   Pat : Employee_T (Applicant);
   Sam : Employee_T (Contractor);

* Each object is of the same type, but has access to different fields

   * :ada:`Pat` contains the fields :ada:`Name` and :ada:`DOB`
   * :ada:`Sam` contains the fields :ada:`Name`, :ada:`DOB`, and :ada:`Hourly_Rate`

* Note that the :ada:`case` block must be the last part of the record definition

   * Hence only one per record

---------------------------
Uses of a Variant Record
---------------------------

* By allowing the record content to be "flexible", it allows a parameter to have different behavior for the same record type

   .. code:: Ada

      procedure Print (Item : Employee_T) is
      begin
         Put_Line (To_String(Item.Name) & " " & To_String(Item.DOB));
         case Item.Kind is
         when Applicant =>
            null;
         when Employee =>
            Put_Line (To_String(Pay_Rate));
         when Contractor  =>
            Put_Line (To_String(Item.Hourly_Rate));
         end case;
      end Print;

* Which allows more flexible programming idioms

   .. code:: Ada

      Employees : array (1..2) of Employee_T := (Pat, Sam);

   .. Code:: Ada

      for Employee of Employees loop
         Print (Employee);
      end loop;

--------------------------
Immutable Variant Record
--------------------------

* In an :dfn:`immutable variant record` the discriminant is set at creation and cannot be modified

   .. code:: Ada

      Pat : Employee_T (Applicant);
      Sam : Employee_T := (Kind        => Contractor,
                           Name        => From_String("Sam"),
                           DOB         => "2000/01/01",
                           Hourly_Rate => 123.45);

* Discriminant is treated as any other field

  * But with :ada:`Employee_T` it cannot be changed

* :ada:`Employee_T` is an :ada:`indefinite type`, similar to an unconstrained array

  * So you must add a constraint (discriminant) when creating an object
  * But it can be unconstrained when used as a parameter or array element

----------------------------------
Immutable Variant Record Example
----------------------------------

* Compiler can detect some problems, but more often clashes are run-time errors

  * :ada:`Pat.Hourly_Rate := 12.3;` would generate a compile warning because compiler knows :ada:`Pat` is a :ada:`Applicant`

    * ``warning: Constraint_Error will be raised at run time``

  .. code:: Ada

    procedure Do_Something (Param : in out Employee_T) is
    begin
      Param.Hourly_Rate := Param.Hourly_Rate * 1.03;
    end Do_Something;

  * :ada:`Do_Something (Pat);` generates a run-time error, because only at run-time is the discriminant for :ada:`Param` known

    * ``raised CONSTRAINT_ERROR : discriminant check failed``

* :ada:`Pat := Sam;` would be a compiler warning because the constraints do not match

------------------------
Mutable Variant Record
------------------------

* To add flexibility, we can make the type :dfn:`mutable` by specifying a default value for the discriminant

   .. code:: Ada

      type Category_T is (Applicant, Employee, Contractor);
      type Employee_T (Kind : Category_T := Employee) is record
         Name : String_T;
         DOB  : Date_T;
         case Kind is
            when Applicant =>
               null;
            when Employee =>
               Pay_Rate  : Pay_T;
            when Contractor =>
               Hourly_Rate : Contractor_Rate_T;
      end record;

* :ada:`Pat : Employee_T;` is mutable, but :ada:`Sam : Employee_T(Contractor);` is not

  * Defining object with discriminant value enforces the *constraint*

* We can change the discriminant of a mutable object

  * But only via a copy / aggregate assignment

--------------------------------
Mutable Variant Record Example
--------------------------------

* Each object of :ada:`Employee_T` has some common fields and some unique fields

  .. code:: Ada

    Pat : Employee_T := (Employee, "2000/01/01", 12.34);
    Sam : Employee_T(Contractor);

* You can change the discriminant of :ada:`Pat` via an aggregate assignment or a copy

  .. code:: Ada

    if Pat.Kind = Applicant then
      Pat := (Employee, Pat.Name, Pat.Age, 12.34);
    else
      Pat := Sam;
    end if;

* But you cannot change the discriminant like a regular field

  .. code:: Ada

    Pat.Kind := Contractor; -- compile error
    
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

.. code:: Ada

    type Variant_T (Floating : Boolean := False) is record
        case Floating is
            when False =>
                I : Integer;
            when True =>
                F : Float;
        end case;
        Flag : Character;
    end record;

    Variant_Object : Variant_T (True);

Which component does :ada:`Variant_Object` contain?

A. :ada:`Variant_Object.F, Variant_Object.Flag`
B. :ada:`Variant_Object.F`
C. :answer:`None: Compilation error`
D. None: Runtime error

.. container:: animate

    The variant part cannot be followed by a component declaration (:ada:`Flag : Character` here)

======================================
Discriminant Record Array Size Idiom
======================================

----------------------------------
Varying Lengths of Array Objects
----------------------------------

* In Ada, array objects have to be fixed length

   .. code:: Ada

      S : String (1..80);
      A : array (M .. K*L) of Integer;

* We would like an object with a maximum length, but current length is variable

   + Need two pieces of data

      * Array contents
      * Location of last valid element

* For common usage, we want this to be a type (probably a record)

   + Maximum size array for contents
   + Index for last valid element

-----------------------------
Simple Unconstrained Array
-----------------------------

.. code:: Ada

   type Simple_VString is
      record
         Length : Natural range 0 .. Max_Length := 0;
         Data   : String (1 .. Max_Length) := (others => ' ');
      end record;

   Object1 : Simple_Vstring := ( 0, (others => ' '));
   Object2 : Simple_Vstring := ( 0, (others => '.'));
   Object3 : Simple_Vstring;

* Issue - Operations need to consider :ada:`Length` field

   * :ada:`Object1 = Object2` will be false
   * Can redefine :ada:`=` to be something like

      .. code:: Ada

         if Object1.Data(1..Object1.Length) =
            Object2.Data(1..Object2.Length)
         then

   * Same thing with concatentation

      .. code:: Ada

         Object3.Length := Object1.Length + Object2.Length;
         Object3.Data(1..Object3.Length) :=
               Object1.Data(1..Object1.Length) &
               Object2.Data(1..Object2.Length)
* Other Issues

   + Every object has same maximum length
   + ``Length`` needs to be maintained by program logic

------------------------------------------------
Varying Length Array via Discriminated Records
------------------------------------------------

* Discriminant can serve as bound of array component

   .. code:: Ada

      type VString (Length : Natural := 0) is
        record
          Data   : String (1..Length) := (others => ' ');
        end record;

* Discriminant default value?

   + With default discriminant value, objects can be copied even if lengths are different
   + With no default discriminant value, objects of different lengths cannot be copied

-----------------
Object Creation
-----------------

* When an object is created with the default value (mutable) runtime assumes largest possible value

   + So this is a problem

      .. code:: Ada

         type VString (Length : Natural := 0) is record
            Data   : String (1..Length) := (others => ' ');
         end record;

         Good : Simple_Vstring(10);
         Bad  : Simple_Vstring;

   + Compile warning: ``warning: creation of "Bad" object may raise Storage_Error``

   + Runtime error: ``raised STORAGE_ERROR : EXCEPTION_STACK_OVERFLOW`` 

* Better implementation

   .. code:: Ada

      subtype Length_T is natural range 0 .. 1_000;
      type VString (Length : Length_T := 0) is record
         Data   : String (1..Length) := (others => ' ');
      end record;

      Good      : Simple_Vstring(10);
      Also_Good : Simple_Vstring;

------------------------
Simplifying Operations
------------------------

* With mutable discriminated records, operations are simpler

   .. code:: Ada

      Object1 : Simple_Vstring := ( 5, "Hello");
      Object2 : Simple_Vstring := ( 6, " World");
      Object3 : Simple_Vstring;

   * Equality: :ada:`Object1 = Object2`

      * :ada:`Data` is exactly the correct length
      * if :ada:`Data` or :ada:`Length` is different, equality fails

   * Concatentation

      .. code:: Ada

         Object3 := (Object1.Length + Object2.Length,
                     Object1.Data & Object2.Data);

========
Lab
========

.. include:: labs/adv_060_discriminated_record_types.lab.rst

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

* Flexibility

   - Variant parts may be nested, if some components common to a set of variants

