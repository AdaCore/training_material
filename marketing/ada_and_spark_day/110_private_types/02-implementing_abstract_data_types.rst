==================================
Implementing Abstract Data Types
==================================

-----------------------------------
Declaring Private Types for Views
-----------------------------------

**Syntax**

.. code:: bnf

  private_type_declaration ::=
     type identifier [discriminant_part] is private;

* Private type declaration must occur in visible section

   - :dfn:`Partial view`
   - Only partial information on the type
   - **Clients** can reference the type name

      - But cannot create an object of that type until after the full type declaration

* Full type declaration must appear in private section

   - Completion is the :dfn:`Full view`
   - **Never** visible to **clients**
   - **Not** visible to **designer** until reached

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

-------------------------------------
Clients Declare Objects of the Type
-------------------------------------

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
* Therefore **clients** cannot compile code referencing representation

.. code:: Ada

  with Bounded_Stacks;
  procedure Client is
    My_Stack : Bounded_Stacks.Stack;
  begin
    My_Stack.Top := 1;  -- Client cannot see inside "Stack"
  end Client;
