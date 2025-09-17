===============
Miscellaneous
===============

-----------------------------
 Checked Type Conversions
-----------------------------

* Between "closely related" types

   - Numeric types
   - Inherited types
   - Array types

* Illegal conversions **rejected**

   - Unsafe **Unchecked_Conversion** available

* Called as if it was a function

   - Named using destination type name

      .. code:: Ada

         Target_Float := Float (Source_Integer);

   - Implicitly defined
   - **Must** be explicitly called

-------------
Default Value
-------------

* Not defined by language for **scalars**
* Can be done with an **aspect clause**

  - Only during type declarations
  - :code:`<value>` must be static

   .. code:: Ada

      type <typemark> is <type_definition>
           with Default_Value => <value>;

* Example

   .. code:: Ada

      type Tertiary_Switch is (Off, On, Neither)
         with Default_Value => Neither;
      Implicit : Tertiary_Switch; -- Implicit = Neither
      Explicit : Tertiary_Switch := Neither;

..
  language_version 2012

-------------------------------
Simple Static Type Derivation
-------------------------------

**Syntax**

.. container:: source_include 030_scalar_types/syntax.bnf :start-after:simple_static_type_derivation_begin :end-before:simple_static_type_derivation_end :code:bnf

* **identifier** will copy the behavior of **parent_subtype_mark**
* **constraint** optionally adds limitations to the parent type's behavior

* New type from an existing type

  - **Limited** form of inheritance: operations
  - **Not** fully OOP
  - More details later

* Strong type benefits

  - Only **explicit** conversion possible
  - eg. :code:`Meters` can't be set from a :code:`Feet` value


**Example**

.. code:: Ada

   type Measurement is digits 6;
   type Distance is new Measurement
         range 0.0 .. Measurement'Last;
