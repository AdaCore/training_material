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
         Component1 : Integer;
         Component2 : Boolean;
      end record;

* Records can be **discriminated** as well

   .. code:: Ada

      type T (Size : Natural := 0) is record
         Text : String (1 .. Size);
      end record;

