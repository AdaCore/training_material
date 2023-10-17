***************
Private Types
***************

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

* Why does fixing bugs introduce new ones?
* Control over visibility is a primary factor

   - Changes to an abstraction's internals shouldn't break users
   - Including type representation

* Need tool-enforced rules to isolate dependencies

   - Between implementations of abstractions and their users
   - In other words, "information hiding"

--------------------
Information Hiding
--------------------

.. container:: columns

 .. container:: column

    * A design technique in which implementation artifacts are made inaccessible to users
    * Based on control of visibility to those artifacts

       - A product of "encapsulation"
       - Language support provides rigor

    * Concept is "software integrated circuits"

 .. container:: column

    .. image:: interface_vs_implementation.png
       :width: 70%

-------
Views
-------

* Specify legal manipulation for objects of a type

   - Types are characterized by permitted values and operations

* Some views are implicit in language

   - Mode :ada:`in` parameters have a view disallowing assignment

* Views may be explicitly specified

   - Disallowing access to representation
   - Disallowing assignment

* Purpose: control usage in accordance with design

   - Adherence to interface
   - Abstract Data Types

============================================
Implementing Abstract Data Types via Views
============================================

----------------------------------
Implementing Abstract Data Types
----------------------------------

* A combination of constructs in Ada
* Not based on single "class" construct, for example
* Constituent parts

   - Packages, with "private part" of package spec
   - "Private types" declared in packages
   - Subprograms declared within those packages

---------------------------------------------
Package Visible and Private Parts for Views
---------------------------------------------

* Declarations in visible part are exported to users
* Declarations in private part are hidden from users

   - No compilable references to type's actual representation

.. code:: Ada

   package name is
   ... exported declarations of types, variables, subprograms ...
   private
   ... hidden declarations of types, variables, subprograms ...
   end name;

-----------------------------------
Declaring Private Types for Views
-----------------------------------

* Partial syntax

   .. code:: Ada

      type defining_identifier is private;

* Private type declaration must occur in visible part

   - :dfn:`Partial view`
   - Only partial information on the type
   - Users can reference the type name

* Full type declaration must appear in private part

   - Completion is the :dfn:`Full view`
   - **Never** visible to users
   - **Not** visible to designer until reached

.. code:: Ada

   package Control is
     type Valve is private;
     procedure Open (V : in out Valve);
     procedure Close (V : in out Valve);
     ...
   private
     type Valve is ...
   end Control;

---------------------------------
Partial and Full Views of Types
---------------------------------

* Private type declaration defines a :dfn:`partial view`

   - The type name is visible
   - Only designer's operations and some predefined operations
   - No references to full type representation

* Full type declaration defines the :dfn:`full view`

   - Fully defined as a record type, scalar, imported type, etc...
   - Just an ordinary type within the package

* Operations available depend upon one's view

---------------------------------
Software Engineering Principles
---------------------------------

* Encapsulation and abstraction enforced by views

   - Compiler enforces view effects

* Same protection as hiding in a package body

   - Recall "Abstract Data Machines" idiom

* Additional flexibility of types

   - Unlimited number of objects possible
   - Passed as parameters
   - Components of array and record types
   - Dynamically allocated
   - et cetera

-----------------------------------
Users Declare Objects of the Type
-----------------------------------

* Unlike "abstract data machine" approach
* Hence must specify which stack to manipulate

   - Via parameter

.. code:: Ada

   X, Y, Z : Stack;
   ...
   Push (42, X);
   ...
   if Empty (Y) then
   ...
   Pop (Counter, Z);

------------------------------------
Compile-Time Visibility Protection
------------------------------------

* No type representation details available outside the package
* Therefore users cannot compile code referencing representation
* This does not compile

   .. code:: Ada

      with Bounded_Stacks;
      procedure User is
        S : Bounded_Stacks.Stack;
      begin
        S.Top := 1;  -- Top is not visible
      end User;

-------------------
Benefits of Views
-------------------

* Users depend only on visible part of specification

   - Impossible for users to compile references to private part
   - Physically seeing private part in source code is irrelevant

* Changes to implementation don't affect users

   - No editing changes necessary for user code

* Implementers can create bullet-proof abstractions

   - If a facility isn't working, you know where to look

* Fixing bugs is less likely to introduce new ones

------
Quiz
------

.. code:: Ada

   package P is
      type Private_T is private;

      type Record_T is record

Which component is legal?

   A. ``Field_A : integer := Private_T'Pos (Private_T'First);``
   B. ``Field_B : Private_T := null;``
   C. ``Field_C : Private_T := 0;``
   D. :answermono:`Field_D : integer := Private_T'Size;`

      .. code:: Ada

         end record;

.. container:: animate

   Explanations

   A. Visible part does not know :ada:`Private_T` is discrete
   B. Visible part does not know possible values for :ada:`Private_T`
   C. Visible part does not know possible values for :ada:`Private_T`
   D. Correct - type will have a known size at run-time

===========================
Private Part Construction
===========================

-----------------------
Private Part Location
-----------------------

* Must be in package specification, not body
* Body usually compiled separately after declaration
* Users can compile their code before the package body is compiled or even written

   * Package definition

      .. code:: Ada

          package Bounded_Stacks is
            type Stack is private;
            ...
          private
            type Stack is ...
          end Bounded_Stacks;

   * Package reference

      .. code:: Ada

          with Bounded_Stacks;
          procedure User is
            S : Bounded_Stacks.Stack;
          ...
          begin
            ...
          end User;

--------------------------------
Private Part and Recompilation
--------------------------------

* Private part is part of the specification

   - Compiler needs info from private part for users' code, e.g., storage layouts for private-typed objects

* Thus changes to private part require user recompilation
* Some vendors avoid "unnecessary" recompilation

   - Comment additions or changes
   - Additions which nobody yet references

---------------------
Declarative Regions
---------------------

* Declarative region of the spec extends to the body

   - Anything declared there is visible from that point down
   - Thus anything declared in specification is visible in body

.. code:: Ada

   package Foo is
      type Private_T is private;
      procedure X (B : in out Private_T);
   private
      -- Y and Hidden_T are not visible to users
      procedure Y (B : in out Private_T);
      type Hidden_T is ...;
      type Private_T is array (1 .. 3) of Hidden_T;
   end Foo;

   package body Foo is
      -- Z is not visible to users
      procedure Z (B : in out Private_T) is ...
      procedure Y (B : in out Private_T) is ...
      procedure X (B : in out Private_T) is ...
    end Foo;

-----------------------
Full Type Declaration
-----------------------

.. container:: columns

 .. container:: column

    * May be any type

       - Predefined or user-defined
       - Including references to imported types

    * Contents of private part are unrestricted

       - Anything a package specification may contain
       - Types, subprograms, variables, etc.

 .. container:: column

   .. container:: latex_environment small

    .. code:: Ada

       package P is
         type T is private;
         ...
       private
         type Vector is array (1.. 10)
            of Integer;
         function Initial
            return List;
         type T is record
           A, B : List := Initial;
         end record;
       end P;

.. container:: speakernote

   List and Initial are not visible to callers

--------------------
Deferred Constants
--------------------

* Visible constants of a hidden representation

   - Value is "deferred" to private part
   - Value must be provided in private part

* Not just for private types, but usually so

.. code:: Ada

   package P is
     type Set is private;
     Null_Set : constant Set; -- exported name
     ...
   private
     type Index is range ...
     type Set is array (Index) of Boolean;
     Null_Set : constant Set :=  -- definition
        (others => False);
   end P;

------
Quiz
------

.. code:: Ada

   package P is
      type Private_T is private;
      Object_A : Private_T;
      procedure Proc (Param : in out Private_T);
   private
      type Private_T is new integer;
      Object_B : Private_T;
   end package P;

   package body P is
      Object_C : Private_T;
      procedure Proc (Param : in out Private_T) is null;
   end P;

Which object definition is **not** legal?

   A. :answermono:`Object_A`
   B. ``Object_B``
   C. ``Object_C``
   D. None of the above

.. container:: animate

   An object cannot be declared until its type is fully declared.
   :ada:`Object_A` could be declared constant, but then it would
   have to be finalized in the :ada:`private` section.

=================
View Operations
=================

-----------------
View Operations
-----------------

* A matter of inside versus outside the package

   - Inside the package the view is that of the designer
   - Outside the package the view is that of the user

.. container:: latex_environment footnotesize

 .. container:: columns

  .. container:: column

    * **User** of package has **Partial** view

       - Operations exported by package
       - Basic operations

  .. container:: column

    * **Designer** of package has **Full** view

       - **Once** completion is reached
       - All operations based upon full definition of type
       - Indexed components for arrays
       - components for records
       - Type-specific attributes
       - Numeric manipulation for numerics
       - et cetera

-------------------------------------
Designer View Sees Full Declaration
-------------------------------------

.. code:: Ada

   package Bounded_Stacks is
     Capacity : constant := 100;
     type Stack is private;
     procedure Push (Item : in Integer; Onto : in out Stack);
     ...
   private
     type Index is range 0 .. Capacity;
     type Vector is array (Index range 1..Capacity) of Integer;
     type Stack is record
        Top : integer;
        ...
   end Bounded_Stacks;

.. container:: speakernote

   Inside BoundedStacks, STACK is just a normal record

--------------------------------------
Designer View Allows All Operations
--------------------------------------

.. code:: Ada

   package body Bounded_Stacks is
     procedure Push (Item : in Integer;
                     Onto : in out Stack) is
     begin
        Onto.Top := Onto.Top + 1;
        ...
     end Push;

     procedure Pop (Item : out Integer;
                    From : in out Stack) is
     begin
        Onto.Top := Onto.Top - 1;
        ...
     end Pop;
   end Bounded_Stacks;

-----------------------------
Users Have the Partial View
-----------------------------

* Since they are outside package
* Basic operations
* Exported subprograms

.. code:: Ada

   package Bounded_Stacks is
     type Stack is private;
     procedure Push (Item : in Integer; Onto : in out Stack);
     procedure Pop (Item : out Integer; From : in out Stack);
     function Empty (S : Stack) return Boolean;
     procedure Clear (S : in out Stack);
     function Top (S : Stack) return Integer;
   private
     ...
   end Bounded_Stacks;

------------------------
User View's Activities
------------------------

* Declarations of objects

   - Constants and variables
   - Must call designer's functions for values

   .. code:: Ada

      C : Complex.Number := Complex.I;

* Assignment, equality and inequality, conversions
* Designer's declared subprograms
* User-declared subprograms

   - Using parameters of the exported private type
   - Dependent on designer's operations

-----------------------------
User View Formal Parameters
-----------------------------

* Dependent on designer's operations for manipulation

   - Cannot reference type's representation

* Can have default expressions of private types

.. code:: Ada

   -- external implementation of "Top"
   procedure Get_Top (
       The_Stack : in out Bounded_Stacks.Stack;
       Value : out Integer) is
     Local : Integer;
   begin
     Bounded_Stacks.Pop (Local, The_Stack);
     Value := Local;
     Bounded_Stacks.Push (Local, The_Stack);
   end Get_Top;

-----------------
Limited Private
-----------------

* :ada:`limited` is itself a view

    - Cannot perform assignment, copy, or equality

* :ada:`limited private` can restrain user's operation

    - Actual type **does not** need to be :ada:`limited`

.. code:: Ada

    package UART is
        type Instance is limited private;
        function Get_Next_Available return Instance;
    [...]

.. code:: Ada

    declare
       A, B := UART.Get_Next_Available;
    begin
       if A = B -- Illegal
       then
           A := B; -- Illegal
       end if;

====================================
When To Use or Avoid Private Types
====================================

---------------------------
When To Use Private Types
---------------------------

* Implementation may change

   - Allows users to be unaffected by changes in representation

* Normally available operations do not "make sense"

   - Normally available based upon type's representation
   - Determined by intent of ADT

   .. code:: Ada

      A : Valve;
      B : Valve;
      C : Valve;
      ...
      C := A + B;  -- addition not meaningful

* Users have no "need to know"

   - Based upon expected usage

-----------------------------
When To Avoid Private Types
-----------------------------

* If the abstraction is too simple to justify the effort

   - But that's the thinking that led to Y2K rework

* If normal user interface requires representation-specific operations that cannot be provided

   - Those that cannot be redefined by programmers
   - Would otherwise be hidden by a private type
   - If `Vector` is private, indexing of elements is annoying

      .. code:: Ada

        type Vector is array (Positive range <>) of Float;
        V : Vector (1 .. 3);
        ...
        V (1) := Alpha;

========
Idioms
========

---------------------------------------
Effects of Hiding Type Representation
---------------------------------------

* Makes users independent of representation

   - Changes cannot require users to alter their code
   - Software engineering is all about money...

* Makes users dependent upon exported operations

   - Because operations requiring representation info are not available to users

      + Expression of values (aggregates, etc.)
      + Assignment for limited types

* Common idioms are a result

   - :dfn:`Constructor`
   - :dfn:`Selector`

--------------
Constructors
--------------

* Create designer's objects from user's values
* Usually functions

.. code:: Ada

   package Complex is
     type Number is private;
     function Make (Real_Part : Float; Imaginary : Float) return Number;
   private
     type Number is record ...
   end Complex;

   package body Complex is
      function Make (Real_Part : Float; Imaginary_Part : Float)
        return Number is ...
   end Complex:
   ...
   A : Complex.Number :=
       Complex.Make (Real_Part => 2.5, Imaginary => 1.0);

----------------------------
Procedures As Constructors
----------------------------

* Spec

   .. code:: Ada

      package Complex is
        type Number is private;
        procedure Make (This : out Number;  Real_Part, Imaginary : in Float) ;
        ...
      private
        type Number is record
          Real_Part, Imaginary : Float;
        end record;
      end Complex;

* Body (partial)

   .. code:: Ada

      package body Complex is
        procedure Make (This : out Number;
                        Real_Part, Imaginary : in Float) is
          begin
            This.Real_Part := Real_Part;
            This.Imaginary := Imaginary;
          end Make;
      ...

-----------
Selectors
-----------

* Decompose designer's objects into user's values
* Usually functions

.. code:: Ada

   package Complex is
     type Number is private;
     function Real_Part (This: Number) return Float;
     ...
   private
     type Number is record
       Real_Part, Imaginary : Float;
     end record;
   end Complex;

   package body Complex is
     function Real_Part (This : Number) return Float is
     begin
       return This.Real_Part;
     end Real_Part;
     ...
   end Complex;
   ...
   Phase : Complex.Number := Complex.Make (10.0, 5.5);
   Object : Float := Complex.Real_Part (Phase);

========
Lab
========

.. include:: labs/110_private_types.lab.rst

=========
Summary
=========

---------
Summary
---------

* Tool-enforced support for Abstract Data Types

   - Same protection as Abstract Data Machine idiom
   - Capabilities and flexibility of types

* May also be :ada:`limited`

   - Thus additionally no assignment or predefined equality
   - More on this later

* Common interface design idioms have arisen

   - Resulting from representation independence

* Assume private types as initial design choice

   - Change is inevitable
