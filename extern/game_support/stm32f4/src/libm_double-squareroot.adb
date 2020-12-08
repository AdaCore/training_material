------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                S Y S T E M . L I B M _ D O U B L E . S Q R T             --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Ada Cert Math specific implementation of sqrt (powerpc)

with Ada.Unchecked_Conversion;

with System.Machine_Code;

package body Libm_Double.Squareroot is

   function Rsqrt (X : Long_Float) return Long_Float;
   --  Compute the reciprocal square root. There are two reasons for computing
   --  the reciprocal square root instead of computing directly the square
   --  root: PowerPc provides an instruction (fsqrte) to compute an estimate of
   --  the reciprocal (with 5 bits of precision), and the Newton-Raphson method
   --  is more efficient on the reciprocal than on the direct root (because the
   --  direct root needs divisions, while the reciprocal does not). Note that
   --  PowerPc core e300 doesn't support the direct square root operation.

   -----------
   -- Rsqrt --
   -----------

   function Rsqrt (X : Long_Float) return Long_Float is
      X_Half : constant Long_Float := X * 0.5;
      Y      : Long_Float;

   begin
      if Standard'Target_Name = "powerpc-elf" then

         --  On powerpc, the precision of fsqrte is at least 5 binary digits

         System.Machine_Code.Asm ("frsqrte %0,%1",
                                  Outputs => Long_Float'Asm_Output ("=f", Y),
                                  Inputs  => Long_Float'Asm_Input ("f", X));
      else
         --  Provide the exact result for 1.0

         if X = 1.0 then
            return X;
         end if;

         --  Use the method described in Fast Inverse Square Root article by
         --  Chris Lomont (http://www.lomont.org/Math/Papers/2003/InvSqrt.pdf),
         --  although the code was known before that article.

         declare
            type Unsigned_Long is mod 2**64;

            function To_Unsigned_Long is new Ada.Unchecked_Conversion
              (Long_Float, Unsigned_Long);
            function From_Unsigned_Long is new Ada.Unchecked_Conversion
              (Unsigned_Long, Long_Float);
            U : Unsigned_Long;

         begin
            U := To_Unsigned_Long (X);
            U := 16#5fe6ec85_e7de30da# - (U / 2);
            Y := From_Unsigned_Long (U);

            --  Precision is about 4 digits
         end;
      end if;

      --  Newton iterations: X <- X - F(X)/F'(X)
      --  Here F(X) = 1/X^2 - A,  so F'(X) = -2/X^3
      --  So: X <- X - (1/X^2 - A) / (-2/X^3)
      --        <- X + .5(X - A*X^3)
      --        <- X + .5*X*(1 - A*X^2)
      --        <- X (1 + .5 - .5*A*X^2)
      --        <- X(1.5 - .5*A*X^2)
      --  Precision is doubled at each iteration.

      --  Refine: 10 digits (PowerPc) or 8 digits (fast method)

      Y := Y * (1.5 - X_Half * Y * Y);

      --  Refine: 20 digits (PowerPc) or 16 digits (fast method)

      Y := Y * (1.5 - X_Half * Y * Y);

      --  Refine: 40 digits (PowerPc) or 32 digits (fast method)

      Y := Y * (1.5 - X_Half * Y * Y);

      --  Refine (beyond the precision of Long_Float)

      Y := Y * (1.5 - X_Half * Y * Y);

      return Y;
   end Rsqrt;

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Long_Float) return Long_Float is
   begin
      if X <= 0.0 then
         if X = 0.0 then
            return X;
         else
            return NaN;
         end if;

      elsif X = Infinity then
         return X;

      else
         return X * Rsqrt (X);
      end if;
   end Sqrt;

end Libm_Double.Squareroot;
