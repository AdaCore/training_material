========
Lab
========

----------------
Visibility Lab
----------------

* Requirements

   - Create two types packages for two different shapes. Each package should have the following components:

      + :ada:`Number_of_Sides` - indicates how many sides in the shape
      + :ada:`Side_T` - numeric value for length
      + :ada:`Shape_T` - array of :ada:`Side_T` components whose length is :ada:`Number_of_Sides`

   - Create a main program that will

      + Create an object of each :ada:`Shape_T`
      + Set the values for each component in :ada:`Shape_T`
      + Add all the components in each object and print the total

* Hints

   - There are multiple ways to resolve this!

----------------------------------------
Visibility Lab Solution - Types
----------------------------------------

.. container:: source_include 135_visibility/lab/visibility/answer/quads.ads :code:Ada :number-lines:1

.. container:: source_include 135_visibility/lab/visibility/answer/triangles.ads :code:Ada :number-lines:1

-----------------------------------
Visibility Lab Solution - Main #1
-----------------------------------

.. container:: source_include 135_visibility/lab/visibility/answer/main1.adb :code:Ada :number-lines:1

-----------------------------------
Visibility Lab Solution - Main #2
-----------------------------------

.. container:: source_include 135_visibility/lab/visibility/answer/main2.adb :code:Ada :number-lines:1
