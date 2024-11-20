==================
Components Rules
==================

-------------------------------
Characteristics of Components
-------------------------------

* **Heterogeneous** types allowed
* Referenced **by name**
* May be no components, for **empty records**
* **No** anonymous types (e.g., arrays) allowed

   .. code:: Ada

      type Record_1 is record
         This_Is_Not_Legal : array (1 .. 3) of Integer;
      end record;

* **No** constant components

   .. code:: Ada

      type Record_2 is record
         This_Is_Not_Legal : constant Integer := 123;
      end record;

* **No** recursive definitions

   .. code:: Ada

      type Record_3 is record
         This_Is_Not_Legal : Record_3;
      end record;

* **No** indefinite types

   .. code:: Ada

      type Record_5 is record
         This_Is_Not_Legal : String;
         But_This_Is_Legal : String (1 .. 10);
      end record;

-----------------------
Multiple Declarations
-----------------------

* Multiple declarations are allowed (like objects)

   .. code:: Ada

      type Several is record
         A, B, C : Integer := F;
      end record;

* Equivalent to

   .. code:: Ada

      type Several is record
         A : Integer := F;
         B : Integer := F;
         C : Integer := F;
      end record;

-----------------------------------------
"Dot" Notation for Components Reference
-----------------------------------------

.. code:: Ada

   type Months_T is (January, February, ..., December);
   type Date is record
      Day : Integer range 1 .. 31;
      Month : Months_T;
      Year : Integer range 0 .. 2099;
   end record;
   Arrival : Date;
   ...
   Arrival.Day := 27;  -- components referenced by name
   Arrival.Month := November;
   Arrival.Year := 1990;

* Can reference nested components

.. code:: Ada

   Employee
      .Birth_Date
        .Month := March;

------
Quiz
------

.. include:: ../quiz/record_component_decl/quiz.rst

------
Quiz
------

.. code:: Ada

    type Cell is record
       Val : Integer;
       Message : String;
    end record;

Is the definition legal?

A. Yes
B. :answer:`No`

.. container:: animate

    A :ada:`record` definition cannot have a component of an indefinite type. :ada:`String` is indefinite if you don't specify its size.

