************
Visibility
************

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

===============
"use" Clauses
===============

----------------
 `use` Clauses
----------------

* Provide direct visibility into packages' exported items

   + :dfn:`Direct Visibility` - as if object was referenced from within package being used

* May still use expanded name

.. code:: Ada

   package Ada.Text_IO is
     procedure Put_Line(...);
     procedure New_Line(...);
     ...
   end Ada.Text_IO;

   with Ada.Text_IO;
   procedure Hello is
     use Ada.Text_IO;
   begin
     Put_Line("Hello World");
     New_Line(3);
     Ada.Text_IO.Put_Line ("Good bye");
   end Hello;

--------------------
`use` Clause Scope
--------------------

* Applies to end of body, from first occurrence

.. code:: Ada

   package Pkg_A is
      Constant_A : constant := 123;
   end Pkg_A;

   package Pkg_B is
      Constant_B : constant := 987;
   end Pkg_B;

   with Pkg_A;
   with Pkg_B;
   use Pkg_A; -- everything in Pkg_A is now visible
   package P is
      A  : Integer := Constant_A; -- legal
      B1 : Integer := Constant_B; -- illegal
      use Pkg_B; -- everything in Pkg_B is now visible
      B2 : Integer := Constant_B; -- legal
      function F return Integer;
   end P;

   package body P is
     -- all of Pkg_A and Pkg_B is visible here
     function F return Integer is (Constant_A + Constant_B);
   end P;
