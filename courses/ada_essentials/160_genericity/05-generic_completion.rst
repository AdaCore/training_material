====================
Generic Completion
====================

-----------------------------
Generic and Freezing Points
-----------------------------

* A generic type **freezes** the type and needs the **full view**
* May force separation between its declaration (in spec) and instantiations (in private or body)

.. code:: Ada

  generic
     type Formal_T is private;
  package Generic_Package is
     Pointer : access Formal_T;
  end Generic_Package;

.. code:: Ada
  :number-lines: 1

  with Generic_Package;
  package Example is
     type Actual_T is private;
     package Instance is new Generic_Package (Actual_T);
  private
     type Actual_T is null record;
  end Example;

:error:`example.ads:4:45: error: premature use of private type`

-------------------------------
Generic Incomplete Parameters
-------------------------------

* A generic type can be incomplete
* Allows generic instantiations before full type definition
* Restricts the possible usages (only :ada:`access`)

.. code:: Ada

   generic
      type Formal_T; -- incomplete
   package Generic_Package is
      Pointer : access Formal_T;
   end Generic_Package;

   package Example is
      type Actual_T is private;
      package Instance is new Generic_Package (Actual_T);
   private
      type Actual_T is null record;
   end Example;

------
Quiz
------

.. include:: ../quiz/genericity_private_type/quiz.rst
