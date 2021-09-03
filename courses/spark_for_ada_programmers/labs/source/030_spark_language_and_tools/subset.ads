with Ada.Unchecked_Conversion;
with System;

package Subset
  with SPARK_Mode => On
is
   -- A few simple example of language constructs that currently violate the
   -- SPARK subset.

   -- Unchecked conversion with access type is not allowed in SPARK
   type T is access Integer;
   function Conv is new Ada.Unchecked_Conversion (System.Address, T);
   
   -- Access discriminant is not allowed in SPARK
   type R2 (D : access Boolean) is record
      F1 : Integer;
   end record;
   
   -- Task requires the use of Ravenscar profile
   task type My_Task;
   
end Subset;
