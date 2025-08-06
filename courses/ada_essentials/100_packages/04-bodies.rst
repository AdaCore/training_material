========
Bodies
========

----------------
Package Bodies
----------------

* Dependent on corresponding package specification

   - Obsolete if specification changed

* Clients need only to relink if body changed

   - Any code that would require editing would not have compiled in the first place

* Necessary for specifications that require a completion, for example:

   - Subprogram bodies
   - Task bodies
   - Incomplete types in :ada:`private` part
   - Others...

---------------------------
Bodies Are Never Optional
---------------------------

* Either required for a given spec or not allowed at all

   - Based on declarations in that spec

* A change from Ada 83
* A (nasty) justification example will be shown later

--------------------------------------
Example Spec That Cannot Have a Body
--------------------------------------

.. code:: Ada

   package Graphics_Primitives is
     type Coordinate is digits 12;
     type Device_Coordinates is record
       X, Y : Integer;
     end record;
     type Normalized_Coordinates is record
       X, Y : Coordinate range 0.0 .. 1.0;
     end record;
     type Offset is record
       X, Y : Coordinate range -1.0 .. 1.0;
     end record;
     -- nothing to implement, so no body allowed
   end Graphics_Primitives;

---------------------------------------
Example Spec Requiring a Package Body
---------------------------------------

.. code:: Ada

   package VT100 is
     subtype Rows is Integer range 1 .. 24;
     subtype Columns is Integer range 1 .. 80;
     type Position is record
       Row  : Rows := Rows'First;
       Col : Columns := Columns'First;
     end record;
      -- The following need to be defined in the body
     procedure Move_Cursor (To : in Position);
     procedure Home;
     procedure Clear_Screen;
     procedure Cursor_Up (Count : in Positive := 1);
   end VT100;

-----------------------
Required Body Example
-----------------------

.. code:: Ada

   package body VT100 is
     -- This function is not visible outside this package
     function Unsigned (Input : Integer) return String is
       Str : constant String := Integer'Image (Input);
     begin
       return Str (2 .. Str'Length);
     end Unsigned;
     procedure Move_Cursor (To : in Position) is
     begin
       Text_IO.Put (ASCII.Esc & 'I' &
                    Unsigned (To.Row) & ';' &
                    Unsigned (To.Col) & 'H');
     end Move_Cursor;
     procedure Home is
     begin
       Text_IO.Put (ASCII.Esc & "iH");
     end Home;
     procedure Cursor_Up (Count : in Positive := 1) is ...
       ...
   end VT100;

------
Quiz
------

.. include:: ../quiz/package_subprogram_completion/quiz.rst

