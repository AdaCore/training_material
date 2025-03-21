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

-----------------------------------
Users Declare Objects of the Type
-----------------------------------

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
