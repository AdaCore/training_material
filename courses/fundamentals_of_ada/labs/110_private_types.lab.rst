-------------------
Private Types Lab
-------------------

* Requirements

   - Implement a program to create a map such that

      + Map key is a description of a flag
      + Map element content is the set of colors in the flag

   - Operations on the map should include: Add, Remove, Modify, Get, Exists, Image
   - Main program should print out the entire map before exiting

* Hints

   - Should implement a **map** ADT (to keep track of the flags)

      + This **map** will contain all the flags and their color descriptions

   - Should implement a **set** ADT (to keep track of the colors)

      + This **set** will be the description of the map element

   - Each ADT should be its own package
   - At a minimum, the **map** and **set** type should be `private`

---------------------------------------------
Private Types Lab Solution (Color Set)
---------------------------------------------

.. container:: source_include labs/answers/110_private_types.txt :start-after:--Colors :end-before:--Colors :code:Ada

---------------------------------------------
Private Types Lab Solution (Flag Map Spec)
---------------------------------------------

.. container:: source_include labs/answers/110_private_types.txt :start-after:--Flags_Spec :end-before:--Flags_Spec :code:Ada

---------------------------------------------------
Private Types Lab Solution (Flag Map Body 1 of 2)
---------------------------------------------------

.. container:: source_include labs/answers/110_private_types.txt :start-after:--Flags_Body_1 :end-before:--Flags_Body_1 :code:Ada

---------------------------------------------------
Private Types Lab Solution (Flag Map Body 2 of 2)
---------------------------------------------------

.. container:: source_include labs/answers/110_private_types.txt :start-after:--Flags_Body_2 :end-before:--Flags_Body_2 :code:Ada

---------------------------------------------------
Private Types Lab Solution (Main)
---------------------------------------------------

.. container:: source_include labs/answers/110_private_types.txt :start-after:--Main :end-before:--Main :code:Ada

