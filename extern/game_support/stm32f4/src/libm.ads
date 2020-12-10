------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          S Y S T E M . L I B M                           --
--                                                                          --
--                                 S p e c                                  --
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

--  This is the Ada Cert Math specific version of s-libm.ads

with Numerics;

package Libm is
   pragma Pure;

   Ln_2       : constant := 0.69314_71805_59945_30941_72321_21458_17656_80755;
   Ln_3       : constant := 1.09861_22886_68109_69139_52452_36922_52570_46474;
   Half_Ln_2  : constant := Ln_2 / 2.0;
   Inv_Ln_2   : constant := 1.0 / Ln_2;
   Half_Pi    : constant := Numerics.Pi / 2.0;
   Third_Pi   : constant := Numerics.Pi / 3.0;
   Quarter_Pi : constant := Numerics.Pi / 4.0;
   Sixth_Pi   : constant := Numerics.Pi / 6.0;

   Max_Red_Trig_Arg : constant := 0.26 * Numerics.Pi;
   --  This constant representing the maximum absolute value of the reduced
   --  argument of the approximation functions for Sin, Cos and Tan. A value
   --  slightly larger than Pi / 4 is used. The reduction doesn't reduce to
   --  an interval strictly within +-Pi / 4, as that would complicate the
   --  reduction code.

   One_Over_Pi : constant := 1.0 / Numerics.Pi;
   Two_Over_Pi : constant := 2.0 / Numerics.Pi;

   Sqrt_2      : constant := 1.41421_35623_73095_04880_16887_24209_69807_85696;
   Sqrt_3      : constant := 1.73205_08075_68877_29352_74463_41505_87236_69428;

   Root16_Half : constant := 0.95760_32806_98573_64693_63056_35147_91544;
   --  Sixteenth root of 0.5

   Sqrt_Half : constant := 0.70710_67811_86547_52440_08443_62104;

   subtype Quadrant is Integer range 0 .. 3;

   generic
      type T is digits <>;
      with function Sqrt (X : T) return T is <>;
      with function Approx_Asin (X : T) return T is <>;
   function Generic_Acos (X : T) return T;

   generic
      type T is digits <>;
      with function Approx_Atan (X : T) return T is <>;
   function Generic_Atan2 (Y, X : T) return T;

   generic
      type T is digits <>;
   procedure Generic_Pow_Special_Cases
     (Left       : T;
      Right      : T;
      Is_Special : out Boolean;
      Result     : out T);

   generic
      type T is private;
      Mantissa : Positive;
      with function "-" (X : T) return T is <>;
      with function "+" (X, Y : T) return T is <>;
      with function "-" (X, Y : T) return T is <>;
      with function "*" (X, Y : T) return T is <>;
      with function "/" (X, Y : T) return T is <>;
      with function "<=" (X, Y : T) return Boolean is <>;
      with function "abs" (X : T) return T is <>;
      with function Exact (X : Long_Float) return T is <>;
      with function Maximum_Relative_Error (X : T) return Float is <>;
      with function Sqrt (X : T) return T is <>;
   package Generic_Approximations is
      Epsilon : constant Float := 2.0**(1 - Mantissa);
      --  The approximations in this package will work well for single
      --  precision floating point types.

      function Approx_Asin (X : T) return T
        with Pre  => abs X <= Exact (0.25),
             Post => abs Approx_Asin'Result <= Exact (Half_Pi);
      --  @llr Approx_Asin
      --  The Approx_Asin function shallapproximate the mathematical function
      --  arcsin (sqrt (x)) / sqrt (x) - 1.0 on -0.25 .. 0.25.
      --
      --  Ada accuracy requirements:
      --  The approximation MRE shall be XXX T'Model_Epsilon.

      function Approx_Atan (X : T) return T
        with Pre  => abs X <= Exact (Sqrt_3),
             Post => abs (Approx_Atan'Result) <= Exact (Half_Pi) and then
                Maximum_Relative_Error (Approx_Atan'Result) <= 2.0 * Epsilon;
      --  @llr Approx_Atan
      --  The Approx_Atan approximates the mathematical inverse tangent on
      --  -Sqrt (3) .. Sqrt (3)
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Cos (X : T) return T
         with Pre  => abs (X) <= Exact (Max_Red_Trig_Arg),
              Post => abs (Approx_Cos'Result) <= Exact (1.0)
                         and then Maximum_Relative_Error (Approx_Cos'Result)
                                     <= 2.0 * Epsilon;
      --  @llr Approx_Cos
      --  The Approx_Cos approximates the mathematical cosine on
      --  -0.26 * Pi .. 0.26 * Pi
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Exp (X : T) return T
        with Pre  => abs (X) <= Exact (Ln_2 / 2.0),
             Post => Exact (0.0) <= Approx_Exp'Result and then
                Maximum_Relative_Error (Approx_Exp'Result) <= 2.0 * Epsilon;
      --  @llr Approx_Exp
      --  The Approx_Exp function approximates the mathematical exponent on
      --  -Ln (2.0) / 2.0 .. Ln (2.0) / 2.0
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Exp2 (X : T) return T
        with Pre  => abs (X) <= Exact (Ln_2 / 2.0),
             Post => Exact (0.0) <= Approx_Exp2'Result and then
                Maximum_Relative_Error (Approx_Exp2'Result) <= 2.0 * Epsilon;
      --  @llr Approx_Exp2
      --  The Approx_Exp2 function approximates the mathematical function
      --  (x-> 2.0 ** x) on -Ln (2.0) / 2.0 .. Ln (2.0) / 2.0
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Log (X : T) return T
      with
         Pre  => Exact (Sqrt_2 / 2.0) <= X and then
                 X <= Exact (Sqrt_2),
         Post => Maximum_Relative_Error (Approx_Log'Result) <= 2.0 * Epsilon;
      --  @llr Approx_Log
      --  The Approx_Log function approximates the mathematical logarithm on
      --  Sqrt (2.0) /2.0 .. Sqrt (2.0)
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Power_Log (X : T) return T;
      --  @llr Approx_Power_Log
      --  The Approx_Power_Log approximates the function
      --  (x -> Log ((x-1) / (x+1), base => 2)) on the interval
      --  TODO
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Sin  (X : T) return T
        with Pre  => (abs X) <= Exact (Max_Red_Trig_Arg),
             Post => abs Approx_Sin'Result <= Exact (1.0) and then
                  Maximum_Relative_Error (Approx_Sin'Result) <= 2.0 * Epsilon;
      --  @llr Approx_Sin
      --  The Approx_Sin function approximates the mathematical sine on
      --  -0.26 * Pi .. 0.26 * Pi
      --
      --  Ada accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Sinh (X : T) return T
        with Pre  => True, -- The original X >= 0.0 and X <= 1.0, fails ???
             Post => abs Approx_Sinh'Result <= Exact
                               ((Numerics.e - 1.0 / Numerics.e) / 2.0)
                     and then Maximum_Relative_Error (Approx_Sinh'Result)
                                 <= 2.0 * Epsilon;
      --  @llr Approx_Sinh
      --  The Approx_Sinh function approximates the mathematical hyperbolic
      --  sine on XXX
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Tan (X : T) return T
        with Pre => abs X <= Exact (Max_Red_Trig_Arg),
             Post =>
                  Maximum_Relative_Error (Approx_Tan'Result) <= 2.0 * Epsilon;
      --  @llr Approx_Tan
      --  The Approx_Tan function approximates the mathematical tangent on
      --  -0.26 * Pi .. 0.26 * Pi
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Cot (X : T) return T
        with Pre => abs X <= Exact (Max_Red_Trig_Arg),
             Post =>
                  Maximum_Relative_Error (Approx_Cot'Result) <= 2.0 * Epsilon;
      --  @llr Approx_Cot
      --  The Approx_Cot function approximates the mathematical cotangent on
      --  -0.26 * Pi .. 0.26 * Pi
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Approx_Tanh (X : T) return T
        with Pre  => Exact (0.0) <= X and then X <= Exact (Ln_3 / 2.0),
             Post => abs (Approx_Tanh'Result) <= Exact (Half_Pi)
                     and then Maximum_Relative_Error (Approx_Tanh'Result)
                                 <= 2.0 * Epsilon;
      --  @llr Approx_Tanh
      --  The Approx_Tanh function approximates the mathematical hyperbolic
      --  tangent on 0.0 .. Ln (3.0) / 2.0
      --
      --  Accuracy requirements:
      --  The approximation MRE is XXX T'Model_Epsilon

      function Asin (X : T) return T
        with Pre => abs X <= Exact (1.0),
             Post => Maximum_Relative_Error (Asin'Result) <= 400.0 * Epsilon;
      --  @llr Asin
      --  The Asin function has a maximum relative error of 2 epsilon.
   end Generic_Approximations;

end Libm;
