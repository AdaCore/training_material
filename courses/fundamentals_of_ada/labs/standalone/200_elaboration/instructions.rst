-----------------
Elaboration Lab
-----------------

* Requirements

   - Create a `pure` package containing some constants

      + Lower limit of some integer range
      + Upper limit of some integer range
      + Flag indicating an invalid state

   - Create a package whose interface consists solely of one global object

      + Array of integers initialized to the invalid state

   - During elaboration, fill in the array object by querying the user

      + All entries must be in the range of *Lower Limit* to *Upper Limit*

   - Create a `main` program to print out the array

      + Only print values set by the user

* Hints

   - The only indication of actual number of entries is the array itself
   - Need to tell the compiler that the global object is initialized in the package body

---------------------------------------
Elaboration Lab Solution - Data Store
---------------------------------------

.. code:: Ada

   package Datastore is
     pragma Elaborate_Body;
     Object : array (1 .. 100) of Integer;
   end Datastore;

   with Constants;
   with Ada.Text_IO; use Ada.Text_IO;
   package body Datastore is

     subtype Valid_Range is Integer range
       Constants.Minimum_Value .. Constants.Maximum_Value;
     Attempt : Integer;
     Count   : Integer := Object'First;

   begin

     loop
       Put ("Value: ");
       Attempt := Integer'Value (Ada.Text_IO.Get_Line);
       exit when Attempt not in Valid_Range;
       Object (Count) := Attempt;
       Count          := Count + 1;
     end loop;

     for I in Count .. Object'Last loop
       Object (I) := Constants.Invalid_Value;
     end loop;

   end Datastore;

---------------------------------
Elaboration Lab Solution - Main
---------------------------------
.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Constants;
   with Datastore;
   procedure Main is

   begin

     for I in Datastore.Object'First .. Datastore.Object'Last
     loop
       exit when Datastore.Object (I) = Constants.Invalid_Value;
       Put_Line
        (Integer'Image (I) & " =>" &
         Integer'Image (Datastore.Object (I)));
     end loop;

   end Main;

