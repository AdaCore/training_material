==================
Components Rules
==================

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
