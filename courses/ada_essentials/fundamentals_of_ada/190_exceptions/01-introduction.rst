==============
Introduction
==============

--------------------------
Rationale for Exceptions
--------------------------

* Textual separation from normal processing
* Rigorous Error Management

   - Cannot be ignored, unlike status codes from routines
   - Example: running out of gasoline in an automobile

.. code:: Ada

   package Automotive is
     type Vehicle is record
       Fuel_Quantity, Fuel_Minimum : Float;
       Oil_Temperature : Float;
       ...
     end record;
     Fuel_Exhausted : exception;
     procedure Consume_Fuel (Car : in out Vehicle);
     ...
   end Automotive;

--------------------
Semantics Overview
--------------------

* Exceptions become active by being :dfn:`raised`

   - Failure of implicit language-defined checks
   - Explicitly by application

* Exceptions occur at run-time

   - A program has no effect until executed

* May be several occurrences active at same time

   - One per task

* Normal execution abandoned when they occur

   - Error processing takes over in response
   - Response specified by :dfn:`exception handlers`
   - *Handling the exception* means taking action in response
   - Other tasks need not be affected

----------------------------
Semantics Example: Raising
----------------------------

.. code:: Ada

   package body Automotive is
     function Current_Consumption return Float is
       ...
     end Current_Consumption;
     procedure Consume_Fuel (Car : in out Vehicle) is
     begin
       if Car.Fuel_Quantity <= Car.Fuel_Minimum then
         raise Fuel_Exhausted;
       else -- decrement quantity
         Car.Fuel_Quantity := Car.Fuel_Quantity -
                              Current_Consumption;
       end if;
     end Consume_Fuel;
     ...
   end Automotive;

-----------------------------
Semantics Example: Handling
-----------------------------

.. code:: Ada

   procedure Joy_Ride is
     Hot_Rod : Automotive.Vehicle;
     Bored : Boolean := False;
     use Automotive;
   begin
     while not Bored loop
       Steer_Aimlessly (Bored);
       -- error situation cannot be ignored
       Consume_Fuel (Hot_Rod);
     end loop;
     Drive_Home;
   exception
     when Fuel_Exhausted =>
       Push_Home;
   end Joy_Ride;

.. container:: speakernote

   Cannot ignore exception (someone needs to handle it)

---------------------------------------
Handler Part Is Skipped Automatically
---------------------------------------

* If no exceptions are active, returns normally

.. code:: Ada

   begin
     ...
   -- if we get here, skip to end
   exception
     when Name1 =>
     ...
     when Name2 | Name3 =>
     ...
     when Name4 =>
     ...
   end;

