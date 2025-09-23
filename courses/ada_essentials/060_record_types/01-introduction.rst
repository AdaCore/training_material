==============
Introduction
==============

---------------------
Syntax and Examples
---------------------

**Syntax**

.. container:: source_include 060_record_types/syntax.bnf :start-after:syntax_and_examples_begin :end-before:syntax_and_examples_end :code:bnf

**Example**

.. code:: Ada

   type Record1_T is record
      Is_Valid : Boolean := False;
      Content  : Integer;
   end record;

:color-white:`Blank line`

Records can be **discriminated** as well

  .. code:: Ada

    type Varying_Length_String (Size : Natural := 0) is record
       Text : String (1 .. Size);
    end record;
