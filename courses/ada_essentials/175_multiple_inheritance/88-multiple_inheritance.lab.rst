========
Lab
========

------------------------------------------
Multiple Inheritance Lab
------------------------------------------
   
* Requirements
   
   - Create a tagged type to define shapes
 
      - Possible components could include location of shape
 
   - Create an interface to draw lines
 
      - Possible accessor functions could include line color and width
 
   - Create a new type inheriting from both of the above for a "printable object"
 
      - Implement a way to print the object using `Ada.Text_IO`
      - Does not have to be fancy!
 
   - Create a "printable object" type to draw something (rectangle, triangle, etc)
 
* Hints
 
   - This example is taken from Barnes' *Programming in Ada 2012* Section 21.2
   
---------------------------------------
Inheritance Lab Solution - Data Types
---------------------------------------

.. container:: source_include 175_multiple_inheritance/lab/multiple_inheritance/answer/base_types.ads :code:Ada :number-lines:1

---------------------------------------
Inheritance Lab Solution - Shapes
---------------------------------------

.. container:: source_include 175_multiple_inheritance/lab/multiple_inheritance/answer/geometry.ads :code:Ada :number-lines:1
   
-------------------------------------------
Inheritance Lab Solution - Drawing (Spec)
-------------------------------------------

.. container:: source_include 175_multiple_inheritance/lab/multiple_inheritance/answer/line_draw.ads :code:Ada :number-lines:1

-------------------------------------------
Inheritance Lab Solution - Drawing (Body)
-------------------------------------------

.. container:: source_include 175_multiple_inheritance/lab/multiple_inheritance/answer/line_draw.adb :code:Ada :number-lines:1
   
---------------------------------------------
Inheritance Lab Solution - Printable Object
---------------------------------------------

.. container:: source_include 175_multiple_inheritance/lab/multiple_inheritance/answer/printable_object.ads :code:Ada :number-lines:1

.. container:: source_include 175_multiple_inheritance/lab/multiple_inheritance/answer/printable_object.adb :code:Ada :number-lines:1
   
---------------------------------------------
Inheritance Lab Solution - Rectangle
---------------------------------------------

.. container:: source_include 175_multiple_inheritance/lab/multiple_inheritance/answer/rectangle.ads :code:Ada :number-lines:1

.. container:: source_include 175_multiple_inheritance/lab/multiple_inheritance/answer/rectangle.adb :code:Ada :number-lines:1
   
---------------------------------------------
Inheritance Lab Solution - Main
---------------------------------------------

.. container:: source_include 175_multiple_inheritance/lab/multiple_inheritance/answer/main.adb :code:Ada :number-lines:1
