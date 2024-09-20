**********
Packages
**********

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
Declarations
==============

----------------------
Package Declarations
----------------------

* Required in all cases

   - Cannot have a package without the declaration

* Describe the client's interface

   - Declarations are exported to clients
   - Effectively the "pin-outs" for the black-box

* When changed, requires clients recompilation

   - The "pin-outs" have changed

.. code:: Ada

   package Float_Stack is
     Max : constant := 100;
     procedure Push (X : in Float);
     procedure Pop (X : out Float);
   end Float_Stack;

   package Data is
      Object : integer;
   end Data;

---------------------------------
Compile-Time Visibility Control
---------------------------------

* Items in the declaration are visible to users

   .. code:: Ada

      package Some_Package is
        -- exported declarations of
        --   types, variables, subprograms ...
      end Some_Package;

* Items in the body are never externally visible

   - Compiler prevents external references

   .. code:: Ada

      package body Some_Package is
        -- hidden declarations of
        --   types, variables, subprograms ...
        -- implementations of exported subprograms etc.
      end Some_Package;

---------------------------------
Example of Exporting To Clients
---------------------------------

* Variables, types, exception, subprograms, etc.

   - The primary reason for separate subprogram declarations

.. code:: Ada

   package P is
      procedure This_Is_Exported;
   end P;

   package body P is
      procedure Not_Exported is
         ...
      procedure This_Is_Exported is
         ...
   end P;

----------------------------
Referencing Exported Items
----------------------------

* Achieved via "dot notation"
* Package Specification

   .. code:: Ada

      package Float_Stack is
        Max : constant := 100;
        procedure Push (X : in Float);
        procedure Pop (X : out Float);
      end Float_Stack;

* Package Reference

   .. code:: Ada

      with Float_Stack;
      procedure Test is
         X : Float;
      begin
         Float_Stack.Pop (X);
         Float_Stack.Push (12.0);
         if Count < Float_Stack.Max then ...
