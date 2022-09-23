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
* Jorvik profile

---------------------
Miscellaneous (1/2)
---------------------

* Target Name Symbol (``@``)

.. code:: Ada

    Count := @ + 1;

* Enumeration representation attributes

.. code:: Ada

    type E is (A => 10, B => 20);
    ...
    E'Enum_Rep (A); -- 10
    E'Enum_Val (10); -- A

* Staticness

.. include:: examples/ada2022/staticness/extracts/static_expr_fun.ads
    :code: Ada

* C variadic functions interface

.. include:: examples/ada2022/variadic/extracts/variadic_decl.ads
    :code: Ada

---------------------
Miscellaneous (2/2)
---------------------

* Contract on access types

.. include:: examples/ada2022/contract_access/extracts/decl.ads
    :code: Ada

* Declare expressions

.. code:: Ada

    Area : Float := (declare
                        Pi : constant Float := 3.14159
                     begin
                        (Pi ** 2) * R);

* More expressive renamings

.. code:: Ada

    A : Integer;
    B renames A; -- B type is infered

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

* :filename:`custom_image.ads`

.. include:: examples/ada2022/user_defined_image_attribute/extracts/spec.ads
   :code: Ada

* :filename:`custom_image.adb`

.. include:: examples/ada2022/user_defined_image_attribute/extracts/body.adb
   :code: Ada

-----------------------
User-defined literals
-----------------------

* User-defined types can accept literals as inputs

    - :ada:`Integer`, :ada:`Float`, or :ada:`String`

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

* Only for :ada:`array` aggregates
* Syntax not backwards-compatible

.. code:: Ada

    A : array (1 .. 1) of Integer := [99]; -- Legal
    B : array (1 .. 1) of Integer := (99); -- Not legal

* Allows for more complex initialization

.. include:: examples/ada2022/general_aggregates/extracts/square_brackets.ads
    :code: Ada

-------------------
Iteration filters
-------------------

* For any iteration
* Using the :ada:`when` keyword

.. code:: Ada

    for J in 1 .. 100 when J mode 2 /= 0 loop

* Can be used for aggregates as well

.. include:: examples/ada2022/general_aggregates/extracts/iteration_filters.ads
    :code: Ada

----------------------
Container aggregates
----------------------

* Using :ada:`with Aggregate => (<Args>)`
* Args are

    - :ada:`Empty` init function (or else default)
    - :ada:`Add_Named` named aggregate element
    - :ada:`Add_Unnamed` positional aggregate element

* You **cannot** mix named and unnamed

.. include:: examples/ada2022/container_aggregates/extracts/decl_aspect.ads
    :code: Ada

.. include:: examples/ada2022/container_aggregates/extracts/decl_object.ads
    :code: Ada

* Implemented on standard lib's containers

------------------
Delta aggregates
------------------

* Can build an object from another one

    - Similarly to tagged types' extension aggregates
    - Using :ada:`with delta` in the aggregate

.. include:: examples/ada2022/delta_aggregates/extracts/spec.ads
    :code: Ada

==============
Standard Lib
==============

------------------------------
``Ada.Numerics.Big_Numbers``
------------------------------

* Numbers of arbitary size

    - Particularly useful for cryptography

* :ada:`Big_Integers`, :ada:`Big_Reals` child packages

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
    [...]

* Arithmetic operators

.. code:: Ada

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

----------------
Jorvik Profile
----------------

* A **non-backwards compatible profile** based on Ravenscar

  + Defined in the RM D.13 (Ada 2022)

* Remove some constraints

  - Number of protected entries, entry queue length...
  - Scheduling analysis may be harder to perform

* Subset of Ravenscars' requirements
* :ada:`pragma Profile (Jorvik)`

=========
Summary
=========

----------
Ada 2022
----------

* Adapting to new usages

    - Cryptography
    - Lock-free synchronizations

* More expressive syntax

    - :ada:`'Image` and literals
    - Functional approach: filters...
    - Simplified declarations and renamings

* Some features are not implemented...

    - ...by anyone
    - Those are related to parallelization
    - Subject to change in the future

---------------
Unimplemented
---------------

* Global states
* Parallel loops
* Conflict checking
* Chunked iterators
* Procedural iterators
