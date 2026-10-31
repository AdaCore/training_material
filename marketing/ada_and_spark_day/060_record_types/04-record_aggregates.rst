===================
Record Aggregates
===================

------------
Aggregates
------------

* Literal values for composite types

   - As for arrays
   - Default value / selector: :ada:`<>`, :ada:`others`

* Can use both **named** and **positional**

    - Unambiguous

* Example:

  .. code:: Ada

     (Pos_1_Value,
      Pos_2_Value,
      Component_3 => Pos_3_Value,
      Component_4 => <>, -- Default value (Ada 2005)
      others => Remaining_Value)

---------------------------
Record Aggregate Examples
---------------------------

.. code:: Ada

   type Color_T is (Red);
   type Car_T is record
      Color    : Color_T;
      Plate_No : String (1 .. 6);
      Year     : Natural;
   end record;
   type Complex_T is record
      Real      : Float;
      Imaginary : Float;
   end record;

.. code:: Ada

   declare
      Car   : Car_T     := (Red, "ABC123", Year => 2_022);
      Phase : Complex_T := (1.2, 3.4);
   begin
      Phase := (Real => 5.6, Imaginary => 7.8);
   end;

------------------
Delta Aggregates
------------------

* A Record can use a :dfn:`delta aggregate` just like an array

  .. code:: Ada

     type Coordinate_T is record
        X, Y, Z : Float;
     end record;
     Location : constant Coordinate_T := (1.0, 2.0, 3.0);

* Prior to Ada 2022, code would have to copy and then modify

  .. code:: Ada

     declare
        New_Location : Coordinate_T := Location;
     begin
        New_Location.Z := 0.0;
        -- OR
        New_Location := (Z => 0.0, others => <>);
     end;

* Now in Ada 2022, can just specify the change during the copy

  .. code:: Ada

     New_Location : Coordinate_T := (Location with delta Z => 0.0);

.. note::

  For record delta aggregates, named notation must be used
