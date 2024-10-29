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

==============
Introduction
==============

--------------
Introduction
--------------

* Are syntactically distinguished as :ada:`function` and :ada:`procedure`

   - Functions represent *values*
   - Procedures represent *actions*

   .. code:: Ada

      function Is_Leaf (T : Tree) return Boolean
      procedure Split (T : in out Tree;
                       Left : out Tree;
                       Right : out Tree)

* Provide direct syntactic support for separation of specification from implementation

   .. code:: Ada

      function Is_Leaf (T : Tree) return Boolean;

      function Is_Leaf (T : Tree) return Boolean is
      begin
      ...
      end Is_Leaf;


============
Parameters
============

---------------------------------
Parameter Associations In Calls
---------------------------------

* Associate formal parameters with actuals
* Both positional and named association allowed

.. code:: Ada

   Something (ActualX, Formal2 => ActualY);
   Something (Formal2 => ActualY, Formal1 => ActualX);

* Having named followed by positional is forbidden

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
       ...
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
