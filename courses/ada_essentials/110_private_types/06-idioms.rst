========
Idioms
========

---------------------------------------
Effects of Hiding Type Representation
---------------------------------------

* Assume we have a database of employees

  * We want to track name, birth date, pay 

* Implementation details:

  * How do we store the name? Date? Pay?
  * Why should the client care?

* Client interface should be some private type and its primitives:

  .. code:: Ada

    package Database is
      type Employee_T is private;
      procedure Update_Name
        (Employee : in out Employee_T;
         First, Last : String);

* Implementation changes do not require client rework

* Common idioms are a result

   - :dfn:`Constructor`
   - :dfn:`Selector`

--------------
Constructors
--------------

* Create designer's objects from client's values
* Usually functions

.. code:: Ada

   type Types_Pkg;
   package Database is
     type Employee_T is private;
     function Make (Last_Name  : String;
                    First_Name : String;
                    Pay        : Types_Pkg.Pay_T)
                    return Employee_T;
   private
     type Employee_T is record ...
   end Employee_T;

   with Database;
   procedure Client is
      Employee : Database.Employee_T;
   begin
      Employee := Database.Make
         (Last_Name  => "Flintstone",
          First_Name => "Fred",
          Pay        => 1.23);

-----------
Selectors
-----------

* Decompose designer's objects into client's values
* Usually functions

.. code:: Ada

   type Types_Pkg;
   package Database is
     type Employee_T is private;
     function Last_Name
        (Employee : Employee_T)
         return String;
     function Pay
        (Employee : Employee_T)
         return Types_Pkg.Pay_T;

   with Ada.Text_IO; use Ada.Text_IO;
   with Database;
   procedure Client
      (Employee : Database.Employee_T) is
   begin
      Put_Line (Database.Last_Name (Employee) & ", " &
                Database.First_Name (Employee) & " => " &
                Database.Pay (Employee)'Image);
