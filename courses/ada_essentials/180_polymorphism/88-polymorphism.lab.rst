========
Lab
========

.. |rightarrow| replace:: :math:`\rightarrow`

------------------
Polymorphism Lab
------------------
* Requirements

   - Create a multi-level types hierarchy of shapes

      + Level 1: Shape |rightarrow| Quadrilateral ``|`` Triangle
      + Level 2: Quadrilateral |rightarrow| Square

   - Types should have the following primitive operations

      + Description
      + Number of sides
      + Perimeter

   - Create a main program that has multiple shapes

      + Create a nested subprogram that takes any shape and prints all appropriate information

* Hints

   - Top-level type should be abstract

      + But can have concrete operations

   - Nested subprogram in `main` should take a shape class parameter

-------------------------------------------
Polymorphism Lab Solution - Shapes (Spec)
-------------------------------------------

.. container:: source_include 180_polymorphism/lab/polymorphism/answer/shapes.ads :code:Ada :number-lines:1

-------------------------------------------
Polymorphism Lab Solution - Shapes (Body)
-------------------------------------------

.. container:: source_include 180_polymorphism/lab/polymorphism/answer/shapes.adb :code:Ada :number-lines:1

----------------------------------
Polymorphism Lab Solution - Main
----------------------------------

.. container:: source_include 180_polymorphism/lab/polymorphism/answer/main.adb :code:Ada :number-lines:1
