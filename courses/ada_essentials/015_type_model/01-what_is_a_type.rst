=================
What Is A Type?
=================

---------------------
Definiton of a Type
---------------------

* A :dfn:`type` classifies values and tells the compiler/interpreter

   * What they mean
   * How to use them.

* **Type** = label + rules:

   * What kind of data (e.g., number, text, boolean)
   * What operations are allowed (e.g., addition, comparison)

* Examples:

   * Rust: :rust:`u64`, :rust:`isize`
   * C: :cpp:`int`, :cpp:`char *`
   * Ada: :ada:`Integer`, :ada:`Boolean`
   
.. note::

   A type is like a container label: You can’t treat a box of oranges like a blender.

---------------------------
Real Job of a Type System
---------------------------

Types serve multiple critical roles:

   * Validation

      * Catch errors before they happen

   * Documentation

      * Make code more readable and self-explanatory

   * Optimization

      * Help the compiler produce efficient code

   * Abstraction

      * Allow complex operations to be packaged cleanly

----------------------
Type System Spectrum
----------------------

.. list-table::
   :header-rows: 1

   * - Language
     - Static Typing
     - Strong Typing
     - Implicit Conversion

   * - Ada
     - |checkmark|
     - |checkmark|
     - :color-red:`X`

   * - C/C++
     - |checkmark|
     - :color-red:`X`
     - |checkmark|

   * - Python
     - :color-red:`X`
     - |checkmark|
     - |checkmark|

   * - Rust
     - |checkmark|
     - |checkmark|
     - :color-red:`X`

   * - Java
     - |checkmark|
     - |checkmark| (mostly)
     - :color-red:`X`

   * - JavaScript
     - :color-red:`X`
     - :color-red:`X`
     - |checkmark|
