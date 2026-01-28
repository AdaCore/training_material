==============
Introduction
==============

--
Goals
--



-------------------------
The Notion of a Pattern
-------------------------

* Sometimes algorithms can be abstracted from types and subprograms

  .. code:: Ada

     procedure Swap_Int (Left, Right : in out Integer) is
       V : Integer := Left;
     begin
        Left := Right;
        Right := V;
     end Swap_Int;

     procedure Swap_Bool (Left, Right : in out Float) is
        V : Float := Left;
     begin
        Left := Right;
        Right := V;
     end Swap_Bool;

* It would be nice to extract these properties in some common pattern, and then just replace the parts that need to be replaced

  .. code:: Ada

     procedure Swap (Left, Right : in out (Integer | Float)) is
       V : (Integer | Float) := Left;
     begin
        Left := Right;
        Right := V;
     end Swap;





