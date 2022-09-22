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

------------
Aggregates
------------

* Square-bracket array aggregates
* Container aggregates
* Delta aggregates

---------------
Miscellaneous
---------------

* Target Name Symbol (``@``)
* Enumeration representation attributes
* Big Numbers
* C variadic functions interface

---------------
Unimplemented
---------------

* Global
* Parallel loops

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

* :ada:`Root_Buffer_Type` is a text stream

.. code:: Ada

   procedure Put (
      Buffer : in out Root_Buffer_Type;
      Item   : in     String) is abstract;

* Several variants (wide, UTF8...)
   
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

.. include:: examples/
