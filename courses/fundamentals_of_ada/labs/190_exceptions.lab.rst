----------------
Exceptions Lab
----------------

* Requirements

   - Create a simple math package that performs *square*, *square root*, *multiply, *divide*

      + *square* and *square root* should raise a user-defined exception if their input value is out of range
      + *multiply* and *divide* should not do any range checking

   - Your main program should call the four functions with various values

      + If the user-defined exception is caught, print whatever message was raised
      + If a pre-defined exception is caught, print all available exception information

* Hints

   - Define a floating point type with a range that includes negative numbers

      + This allows illegal values for *square*

   - Use `Ada.Numerics.Generic_Elementary_Functions` for the *square* function

--------------------------------
Exceptions Lab Solution - Math 
--------------------------------
.. code:: Ada

   package Simple_Math is
     type Float_T is digits 6 range -1_000.0 .. 1_000.0;
     Illegal_Operation : exception;
     function Sqrt (X : Float_T) return Float_T;
     function Square (X : Float_T) return Float_T;
     function Multiply (L, R : Float_T) return Float_T;
     function Divide (N, D : Float_T) return Float_T;
   end Simple_Math;

   with Ada.Numerics.Generic_Elementary_Functions;
   package body Simple_Math is

     package Math is new Ada.Numerics.Generic_Elementary_Functions (Float_T);

     function Sqrt (X : Float_T) return Float_T is
      (if X >= 0.0 then Math.Sqrt (X)
       else raise Illegal_Operation with "negative number");
     function Square (X : Float_T) return Float_T is
     begin
       if abs (X) < 1.0 then
         return X * X;
       elsif (Float_T'Last / X) < X then
         raise Illegal_Operation with "number too large";
       else
         return X * X;
       end if;
     end Square;

     function Multiply (L, R : Float_T) return Float_T is (L * R);
     function Divide (N, D : Float_T) return Float_T is (N / D);

   end Simple_Math;

---------------------------------------------
Exceptions Lab Solution - Main (incomplete)
---------------------------------------------
.. code:: Ada

   procedure Main is
     package Io is new Ada.Text_IO.Float_IO (Simple_Math.Float_T);
   
     function Get (Prompt : String) return Simple_Math.Float_T;
   
   begin
   
     Put_Line ("Square");
     declare
       Number : Simple_Math.Float_T := Get ("Number");
     begin
       Io.Put (Simple_Math.Square (Number), Exp => 0);
       New_Line;
     exception
       when Error : Simple_Math.Illegal_Operation =>
         Put_Line (Ada.Exceptions.Exception_Message (Error));
       when Error : others =>
         Put_Line (Ada.Exceptions.Exception_Information (Error));
     end;
   
     Put_Line ("Multiply");
     declare
       Left  : Simple_Math.Float_T := Get ("Left");
       Right : Simple_Math.Float_T := Get ("Right");
     begin
       Io.Put (Simple_Math.Multiply (Left, Right), Exp => 0);
       New_Line;
     exception
       when Error : Simple_Math.Illegal_Operation =>
         Put_Line (Ada.Exceptions.Exception_Message (Error));
       when Error : others =>
         Put_Line (Ada.Exceptions.Exception_Information (Error));
     end;
   
   exception
     when The_Err : others =>
       Put_Line ("Other error: " & Ada.Exceptions.Exception_Information (The_Err));
   
   end Main;
