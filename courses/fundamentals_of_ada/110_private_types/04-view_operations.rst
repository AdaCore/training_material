=================
View Operations
=================

-----------------
View Operations
-----------------

* Reminder: view is the *interface* you have on the type

.. container:: columns

  .. container:: column

    * **User** of package has **Partial** view

       - Operations **exported** by package

  .. container:: column

    * **Designer** of package has **Full** view

       - **Once** completion is reached
       - All operations based upon **full definition** of type

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
       A, B : UART.Instance := UART.Get_Next_Available;
    begin
       if A = B -- Illegal
       then
           A := B; -- Illegal
       end if;

