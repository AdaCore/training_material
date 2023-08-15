----------------
Genericity Lab
----------------

* Requirements

   - Create a record structure containing multiple fields

      - Need subprograms to convert the record to a string, and compare the order of two records
      - Lab prompt package :ada:`Data_Type` contains a framework

   - Create a generic list implementation

      - Need subprograms to add items to the list, sort the list, and print the list

   - The `main` program should:

      + Add many records to the list
      + Sort the list
      + Print the list

* Hints

   - Sort routine will need to know how to compare elements
   - Print routine will need to know how to print one element

------------------------------------------
Genericity Lab Solution - Generic (Spec)
------------------------------------------

.. container:: source_include labs/answers/160_genericity.txt :start-after:--Generic_List_Spec :end-before:--Generic_List_Spec :code:Ada

------------------------------------------
Genericity Lab Solution - Generic (Body)
------------------------------------------

.. container:: source_include labs/answers/160_genericity.txt :start-after:--Generic_List_Body :end-before:--Generic_List_Body :code:Ada

-----------------------------------
Genericity Lab Solution - Main
-----------------------------------

.. container:: source_include labs/answers/160_genericity.txt :start-after:--Main :end-before:--Main :code:Ada
