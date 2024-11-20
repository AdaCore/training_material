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

* Define **additional** properties of an entity

    - Representation (eg. :ada:`with Pack`)
    - Operations (eg. :code:`Inline`)
    - Can be **standard** or **implementation**-defined

* Usage close to pragmas

    - More **explicit**, **typed**
    - **Recommended** over pragmas

* Syntax

    .. code:: Ada

       with aspect_mark [ => expression]
           {, aspect_mark [ => expression] }

.. note:: Aspect clauses always part of a **declaration**
..
  language_version 2012

--------------------------------
Aspect Clause Example: Objects
--------------------------------

* Updated **object syntax**

   .. code:: Ada

      <name> : <subtype_indication> [:= <initial value>]
                     with aspect_mark [ => expression]
                     {, aspect_mark [ => expression] };

* Usage

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

* **Boolean** aspects only
* Longhand

  .. code:: Ada

     procedure Foo with Inline => True;

* Aspect name only |rightarrow| **True**

  .. code:: Ada

     procedure Foo with Inline; -- Inline is True

* No aspect |rightarrow| **False**

  .. code:: Ada

     procedure Foo; -- Inline is False

  - Original form!

..
  language_version 2012

