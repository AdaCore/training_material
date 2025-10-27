=========
Aspects
=========

---------
Pragmas
---------

* Originated as a compiler directive for things like

   - Specifying the type of optimization

     .. code:: Ada

        pragma Optimize (Space);

   - Inlining of code

     .. code:: Ada

        pragma Inline (Some_Procedure);

   - Properties (:dfn:`aspects`) of an entity

* Appearance in code

   * Unrecognized pragmas

     .. code:: Ada

         pragma My_Own_Pragma;

      - **No effect**
      - Cause **warning** (standard mode)

   * Must follow correct syntax

     .. code:: Ada

         pragma Page;           -- parameterless
         pragma Optimize (Off); -- with parameter

.. warning:: Malformed pragmas are **illegal**

   :ada:`pragma Illegal One;    -- compile error`

----------------
Aspect Clauses
----------------

**Syntax**

.. container:: source_include 020_declarations/syntax.bnf :start-after:aspect_clauses_begin :end-before:aspect_clauses_end :code:bnf

* Define **additional** properties of an entity

    - Representation (eg. :ada:`with Pack`)
    - Operations (eg. :code:`Inline`)
    - Can be **standard** or **implementation**-defined

* Usage close to pragmas

    - More **explicit**, **typed**
    - **Recommended** over pragmas

.. note:: Aspect clauses always part of a **declaration**

..
  language_version 2012

--------------------------------
Aspect Clause Example: Objects
--------------------------------

**Updated object syntax**

.. container:: source_include 020_declarations/syntax.bnf :start-after:aspect_clause_example_begin :end-before:aspect_clause_example_end :code:bnf

**Example**

.. code:: Ada

   -- using aspects
   CR1 : Control_Register with
         Size    => 8,
         Address => To_Address (16#DEAD_BEEF#);

   -- using representation clauses
   CR2 : Control_Register;
   for CR2'Size use 8;
   for CR2'Address use To_Address (16#DEAD_BEEF#);

..
  language_version 2012

------------------------
Boolean Aspect Clauses
------------------------

* **Boolean** values only
* Longhand

  .. code:: Ada

     procedure Foo with Inline => True;

* Aspect name only |rightarrow| **True**

  .. code:: Ada

     procedure Foo with Inline; -- Inline is True

* No aspect |rightarrow| **False**

  .. code:: Ada

     procedure Foo; -- Inline is False

..
  language_version 2012

