============
Attributes
============

-----------------------
What is an Attribute?
-----------------------

* Properties of entities that can be queried like a function

  - May take input parameters

* Defined by the language and/or compiler

   - Language-defined attributes found in RM K.2
   - *May* be implementation-defined

     * GNAT-defined attributes found in GNAT Reference Manual

   - Cannot be user-defined

* Attribute behavior is generally pre-defined

-----------------
Image Attribute
-----------------

* One of the most common attributes is :ada:`'Image`

  * Convert an object to a string representation

* Originally treated like a subprogram to convert scalar objects

  .. code:: Ada

    Typemark'Image (Scalar_Object)

* Ada 2012 added the ability to use the attribute directly

  .. code:: Ada

    Scalar_Object'Image

* Ada 2022 added the ability to use the attribute on non-scalar objects

  .. code:: Ada

    Any_Object'Image

----------------------------------
Attributes for All Numeric Types
----------------------------------

.. code:: Ada

   type Signed_T is range -99 .. 100;

* :ada:`T'First`

  - First (**smallest**) value of type :ada:`T`
  - :ada:`Signed_T'First` |rightarrow| **-99**

* :ada:`T'Last`

  - Last (**greatest**) value of type :ada:`T`
  - :ada:`Signed_T'Last` |rightarrow| **100**

* :ada:`T'Range`

  - Shorthand for :ada:`T'First .. T'Last`
  - :ada:`Signed_T'Range` |rightarrow| **-99 .. 100**

* :ada:`T'Min (Left, Right)`

  - **Lesser** of two values of type :ada:`T`
  - :ada:`Signed_T'Min (12, 34)` |rightarrow| **12**

* :ada:`T'Max (Left, Right)`

  - **Greater** of two values of type :ada:`T`
  - :ada:`Signed_T'Max (12, 34)` |rightarrow| **34**
