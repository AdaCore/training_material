============================================
Implementing Abstract Data Types Via Views
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

      type <identifier> is private;

* Private type declaration must occur in visible part

   - :dfn:`Partial view`
   - Only partial information on the type
   - Users can reference the type name

      - But cannot create an object of that type until after the full type declaration

* Full type declaration must appear in private part

   - Completion is the :dfn:`Full view`
   - **Never** visible to users
   - **Not** visible to designer until reached

.. code:: Ada

    package Bounded_Stacks is
      type Stack is private;
      procedure Push (Item : in Integer; Onto : in out Stack);
      ...
    private
      ...
      type Stack is record
         Top : Positive;
         ...
    end Bounded_Stacks;

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

   X, Y, Z : Bounded_Stacks.Stack;
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

Which component(s) is (are) legal?

   A. ``Component_A : Integer := Private_T'Pos (Private_T'First);``
   B. ``Component_B : Private_T := null;``
   C. ``Component_C : Private_T := 0;``
   D. :answermono:`Component_D : Integer := Private_T'Size;`

      .. code:: Ada

         end record;

.. container:: animate

   Explanations

   A. Visible part does not know :ada:`Private_T` is discrete
   B. Visible part does not know possible values for :ada:`Private_T`
   C. Visible part does not know possible values for :ada:`Private_T`
   D. Correct - type will have a known size at run-time

