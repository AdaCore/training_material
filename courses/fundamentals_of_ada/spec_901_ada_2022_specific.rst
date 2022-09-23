**********
Ada 2022
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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

============
What's New
============

--------------------
Image and Literals
--------------------

* :ada:`'Image` generalized to all types
* User-defined :ada:`'Image` attribute
* User-defined literals

-----------------
Composite Types
-----------------

* Square-bracket array aggregates
* Iteration filters
* Container aggregates
* Delta aggregates

--------------
Standard Lib
--------------

* :ada:`Ada.Numerics.Big_Numbers`
* :ada:`Ada.Strings.Text_Buffers`
* :ada:`System.Atomic_Operations`

---------------
Miscellaneous
---------------

* Target Name Symbol (``@``)

.. code:: Ada

    Count := @ + 1;

* Enumeration representation attributes

.. code:: Ada

    type E is (A => 10, B => 20);
    ...
    E'Enum_Rep (A); -- 10
    E'Enum_Val (10); -- A

* C variadic functions interface


* Staticness

.. include:: examples/ada2022/staticness/extracts/static_expr_fun.ads
    :code: Ada

---------------
Unimplemented
---------------

* Global states
* Parallel loops
* Conflict checking
* Chunked iterators
* Procedural iterators

====================
Image and Literals
====================

---------------------------
Generalized :ada:`'Image`
---------------------------

* All types have a :ada:`Image` attribute
* Its return value is (mostly) standardized

    - Except for e.g. unchecked unions

* Non-exhaustive example

.. container:: columns

 .. container:: column

  * Code

  .. include:: examples/ada2022/generalized_image_attribute/extracts/put_line.adb
    :code: Ada

 .. container:: column

  * Output

  .. include:: examples/ada2022/generalized_image_attribute/out.txt
    :code:

.

---------------------------
User-defined :ada:`Image`
---------------------------

* User-defined types can have a :ada:`Image` attribute

    - Need to specify the :ada:`Put_Image` aspect

.. code:: Ada

   type My_Type
   [...]
     with Put_Image => My_Put_Image;

.. code:: Ada

   procedure My_Put_Image
     (Buffer : in out
               Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg   : in T);

* Using the new package :ada:`Text_Buffers`

------------------------------------
User-defined :ada:`'Image` example
------------------------------------

* Spec

.. include:: examples/ada2022/user_defined_image_attribute/extracts/spec.ads
   :code: Ada

* Body

.. include:: examples/ada2022/user_defined_image_attribute/extracts/body.adb
   :code: Ada

-----------------------
User-defined literals
-----------------------

* User-defined types can accept literals as inputs

    - :ada:`Integer`, :ada:`Float`, or :ada:`String`

* Example
* :filename:`my_int.ads`

.. include:: examples/ada2022/generalized_literals/extracts/my_int.ads
    :code: Ada

* :filename:`main.adb`

.. include:: examples/ada2022/generalized_literals/extracts/main.adb
    :code: Ada

=================
Composite Types
=================

---------------------------------
Square Bracket Array Aggregates
---------------------------------

.

-------------------
Iteration filters
-------------------

.

----------------------
Container aggregates
----------------------

.

------------------
Delta aggregates
------------------

.

==============
Standard Lib
==============

------------------------------
``Ada.Numerics.Big_Numbers``
------------------------------

* Numbers of arbitary precision
* :ada:`Big_Integers` child package

.. code:: Ada

    type Big_Integer is private
      with Integer_Literal => From_Universal_Image,
           Put_Image => Put_Image;
    subtype Big_Positive is Big_Integer [...]
    subtype Big_Natural is Big_Integer [...]

    function To_Big_Integer (Arg : Integer) return Valid_Big_Integer;

* Comparison operators

.. code:: Ada

    function "=" (L, R : Valid_Big_Integer) return Boolean;
    function "<" (L, R : Valid_Big_Integer) return Boolean;
    function ">" (L, R : Valid_Big_Integer) return Boolean;
    [...]

* Arithmetic operators

.. code:: Ada

    function "-" (L : Valid_Big_Integer) return Valid_Big_Integer;
    function "abs" (L : Valid_Big_Integer) return Valid_Big_Integer;
    function "+" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
    [...]

------------------------------
``Ada.Strings.Text_Buffers``
------------------------------

* Object-oriented package
* :ada:`Root_Buffer_Type`

    - Basically a text stream
    - Abstract object

.. code:: Ada

    type Root_Buffer_Type is abstract tagged private [...];

    procedure Put (
      Buffer : in out Root_Buffer_Type;
      Item   : in     String) is abstract;

    procedure Wide_Put (
      Buffer : in out Root_Buffer_Type;
      Item   : in     Wide_String) is abstract;

    procedure Wide_Wide_Put (
      Buffer : in out Root_Buffer_Type;
      Item   : in     Wide_Wide_String) is abstract;

    procedure Put_UTF_8 (
      Buffer : in out Root_Buffer_Type;
      Item   : in     UTF_Encoding.UTF_8_String) is abstract;

------------------------------
``System.Atomic_Operations``
------------------------------

* Atomic types

    - May be used for lock-free synchronization

* Several child packages

    - :ada:`Exchange` 

        + :ada:`function Atomic_Exchange` ...

    - :ada:`Test_And_Set`

        + :ada:`function Atomic_Test_And_Set` ...

    - :ada:`Integer_Arithmetic`, and :ada:`Modular_Arithmetic`

        + :ada:`generic package`
        + :ada:`procedure Atomic_Add` ...
