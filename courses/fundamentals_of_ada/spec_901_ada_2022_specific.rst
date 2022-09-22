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

  .. include:: examples/spec_901_ada_2022_specific/generalized_image_attribute/extracts/put_line.adb
    :code: Ada

 .. container:: column

 .. container:: column

  * Output

  .. include:: examples/spec_901_ada_2022_specific/generalized_image_attribute/out.txt
    :code:

.

---------------------------
User-defined :ada:`Image`
---------------------------
