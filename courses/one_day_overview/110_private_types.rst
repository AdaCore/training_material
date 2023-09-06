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

============================================
Implementing Abstract Data Types via Views
============================================

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
