package Subset
  with SPARK_Mode => On
is
   -- SPARK 2014 Tutorial - Exercise 1
   
   -- A few simple example of language constructs that currently
   -- violate the SPARK 2014 subset.


   -- Access type
   type T is access Integer;

   -- SPARK 2014 now permits record types with partial default
   -- values (initialisation checked per record field)
   type R is record
      F1 : Integer := 0;
      F2 : Boolean;
   end record;
   
   -- Discriminated and variant records are allowed in SPARK 2014...
   type R2 (D : Boolean) is record
      F1 : Integer;
      case D is
	 when False =>
	    F2 : Character;
	 when True =>
	    F3 : Float;
      end case;
   end record with Unchecked_Union; -- and Unchecked_Union (statically checked).
   
   -- Task type are not currently permitted.
   task type My_Task;
   
end Subset;
