*************
Subprograms
*************

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

========
Syntax
========

--------------------
Syntax (Simplified)
--------------------

.. code:: Ada

   procedure Hello (Str : String);
   function F (X : Float) return Float;

.. code:: Ada

   procedure Hello (Str : String) is
   begin
      Ada.Text_IO.Put_Line ("Hello World!" & Str);
   end Hello;

   function F (X : Float) return Float is
      Y : constant Float := X + 3.0;
   begin
      return X * Y;
   end F;

============
Parameters
============

----------------------------------
Subprogram Parameter Terminology
----------------------------------

* :dfn:`Actual parameters` are values passed to a call

   - Variables, constants, expressions

* :dfn:`Formal parameters` are defined by specification

   - Receive the values passed from the actual parameters
   - Specify the types required of the actual parameters
   - Type **cannot** be anonymous

   .. code:: Ada

      procedure Something (Formal1 : in Integer);

      ActualX : Integer;
      ...
      Something (ActualX);

---------------------------------
Parameter Associations In Calls
---------------------------------

* Associate formal parameters with actuals
* Both positional and named association allowed

.. code:: Ada

   Something (ActualX, Formal2 => ActualY);
   Something (Formal2 => ActualY, Formal1 => ActualX);

* Having named **then** positional is forbidden

.. code:: Ada

   -- Compilation Error
   Something (Formal1 => ActualX, ActualY);

----------------------------
Parameter Modes and Return
----------------------------

* Mode :ada:`in`

   - Actual parameter is :ada:`constant`
   - Can have **default**, used when **no value** is provided

    .. code:: Ada

       procedure P (N : in Integer := 1; M : in Positive);
       [...]
       P (M => 2);

* Mode :ada:`out`

   - Writing is **expected**
   - Reading is **allowed**
   - Actual **must** be a writable object

* Mode :ada:`in out`

   - Actual is expected to be **both** read and written
   - Actual **must** be a writable object

* Function :ada:`return`

   - **Must** always be handled

------------------------------
Parameter Passing Mechanisms
------------------------------

* :dfn:`By-Copy`

   - The formal denotes a separate object from the actual
   - :ada:`in`, :ada:`in out`: actual is copied into the formal **on entry to** the subprogram
   - :ada:`out`, :ada:`in out`: formal is copied into the actual **on exit from** the subprogram

* :dfn:`By-Reference`

   - The formal denotes a view of the actual
   - Reads and updates to the formal directly affect the actual
   - More efficient for large objects

* Parameter **types** control mechanism selection

   - Not the parameter **modes**
   - Compiler determines the mechanism

------------------------------------------
Unconstrained Formal Parameters or Return
------------------------------------------

* Unconstrained **formals** are allowed

    - Constrained by **actual**

* Unconstrained :ada:`return` is allowed too

    + Constrained by the **returned object**

.. code:: Ada

   type Vector is array (Positive range <>) of Float;
   procedure Print (Formal : Vector);

   Phase : Vector (X .. Y);
   State : Vector (1 .. 4);
   ...
   begin
     Print (Phase);          -- Formal'Range is X .. Y
     Print (State);          -- Formal'Range is 1 .. 4
     Print (State (3 .. 4)); -- Formal'Range is 3 .. 4

=====================
Nested Subprograms
=====================

----------------------------
Nested Subprogram Example
----------------------------

.. code:: Ada
   :number-lines: 1

   procedure Main is

      function Read (Prompt : String) return Types.Line_T is
      begin
         Put ("> ");
         return Types.Line_T'Value (Get_Line);
      end Read;

      Lines : Types.Lines_T (1 .. 10);
   begin
      for J in Lines'Range loop
         Lines (J) := Read ("Line " & J'Image);
      end loop;
