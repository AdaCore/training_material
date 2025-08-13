=================
What Is A Type?
=================

---------------------
Definiton of a Type
---------------------

* A :dfn:`type` classifies values and tells the compiler/interpreter

   * What they mean
   * How to use them

* **Type** = label + rules

   * What kind of data (e.g., number, text, boolean)
   * What operations are allowed (e.g., addition, comparison)

* Examples

   * Rust: :rust:`u64`, :rust:`isize`
   * C: :cpp:`int`, :cpp:`char *`
   * Ada: :ada:`Integer`, :ada:`Boolean`
   
.. note::

   A **type** is a blueprint for data; you can't mistake the blueprint for a bicycle with the blueprint for a car

---------------------------
Real Job of a Type System
---------------------------

Types serve multiple critical roles

   * Validation

      * Catch errors before they happen

   * Documentation

      * Make code more readable and self-explanatory

   * Optimization

      * Help the compiler produce efficient code

   * Abstraction

      * Allow complex operations to be packaged cleanly
