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

.. container:: source_include labs/answers/175_multiple_inheritance.txt :start-after:--Types :end-before:--Types :code:Ada :number-lines:1

---------------------------------------
Inheritance Lab Solution - Shapes
---------------------------------------

.. container:: source_include labs/answers/175_multiple_inheritance.txt :start-after:--Shapes :end-before:--Shapes :code:Ada :number-lines:1
   
-------------------------------------------
Inheritance Lab Solution - Drawing (Spec)
-------------------------------------------

.. container:: source_include labs/answers/175_multiple_inheritance.txt :start-after:--Drawing_Spec :end-before:--Drawing_Spec :code:Ada :number-lines:1
   
-------------------------------------------
Inheritance Lab Solution - Drawing (Body)
-------------------------------------------

.. container:: source_include labs/answers/175_multiple_inheritance.txt :start-after:--Drawing_Body :end-before:--Drawing_Body :code:Ada :number-lines:1
   
---------------------------------------------
Inheritance Lab Solution - Printable Object
---------------------------------------------

.. container:: source_include labs/answers/175_multiple_inheritance.txt :start-after:--Printable_Object :end-before:--Printable_Object :code:Ada :number-lines:1
   
---------------------------------------------
Inheritance Lab Solution - Rectangle
---------------------------------------------

.. container:: source_include labs/answers/175_multiple_inheritance.txt :start-after:--Rectangle :end-before:--Rectangle :code:Ada :number-lines:1
   
---------------------------------------------
Inheritance Lab Solution - Main
---------------------------------------------

.. container:: source_include labs/answers/175_multiple_inheritance.txt :start-after:--Main :end-before:--Main :code:Ada :number-lines:1
   
