=====
Lab
=====

-------------------
Private Types Lab
-------------------

* Requirements

   - Implement a program to create a map such that

      + Map key is a country name
      + Map component should contain the associated continent and the colors in the flag

   - Map operations should include

      + Add a country (and its information) to the map
      + Query the map for countries
      + Query each country for its content

   - Main program should

      + Print the entire map
      + Show a count of how many countries in the map are on each continent
      + Show a count of how many countries have the color red in their flag

* Hints

   - Should implement a **map** ADT where the key is the country

      + For each country entry, the data will consist of continent and flag color set

   - Should implement a **set** ADT to collect colors

      + Each flag will be represented by a set of colors

   - Each ADT should be its own package
   - At a minimum, the **country map** and **color set** type should be `private`

   - :ada:`Types` package containing enumerals for continents, countries,
     and colors is part of the **prompt**

----------------------------------------
Private Types Lab Solution - Color_Set
----------------------------------------

.. container:: source_include 110_private_types/lab/private_types/answer/color_set.ads ::code:Ada :number-lines:1

.. container:: source_include 110_private_types/lab/private_types/answer/color_set.adb ::code:Ada :number-lines:1

---------------------------------------------------
Private Types Lab Solution - Countries Map (Spec)
---------------------------------------------------

.. container:: source_include 110_private_types/lab/private_types/answer/countries.ads :code:Ada :number-lines:1

-------------------------------------------------------
Private Types Lab Solution - Countries Map (Body 1/2)
-------------------------------------------------------

.. container:: source_include 110_private_types/lab/private_types/answer/countries.adb :start-after:countries_1_begin :end-before:countries_1_end :code:Ada :number-lines:3

-------------------------------------------------------
Private Types Lab Solution - Countries Map (Body 2/2)
-------------------------------------------------------

.. container:: source_include 110_private_types/lab/private_types/answer/countries.adb :start-after:countries_2_begin :end-before:countries_2_end :code:Ada :number-lines:42

-----------------------------------
Private Types Lab Solution - Main
-----------------------------------

.. container:: source_include 110_private_types/lab/private_types/answer/main.adb :code:Ada :number-lines:1
