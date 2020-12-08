------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          S Y S T E M . L I B M                           --
--                                                                          --
--                                 B o d y                                  --
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

--  This is the Ada Cert Math specific version of s-libm.adb

--  When Cody and Waite implementation is cited, it refers to the
--  Software Manual for the Elementary Functions by William J. Cody, Jr.
--  and William Waite, published by Prentice-Hall Series in Computational
--  Mathematics. Version??? ISBN???

--  When Hart implementation is cited, it refers to
--  "The Computer Approximation" by John F. Hart, published by Krieger.
--  Version??? ISBN???

with Numerics; use Numerics;
with Libm; use Libm;

package body Libm_Single is
   subtype F is Float;

   pragma Assert (F'Machine_Radix = 2);
   pragma Assert (F'Machine_Mantissa = 24);

   F_HM        : constant Integer := Float'Machine_Mantissa / 2;

   type Float_Table is array (Positive range <>) of Float;

   --  A1 (i) = Float (2**((1-i)/16))

   A1_Tab_F : constant Float_Table :=
                (1.0,
                 Root16_Half,
                 Root16_Half**2,
                 Root16_Half**3,
                 Root16_Half**4,
                 Root16_Half**5,
                 Root16_Half**6,
                 Root16_Half**7,
                 Root16_Half**8,
                 Root16_Half**9,
                 Root16_Half**10,
                 Root16_Half**11,
                 Root16_Half**12,
                 Root16_Half**13,
                 Root16_Half**14,
                 Root16_Half**15,
                 0.5);

   --  A2 (i) = 2**((1-2i)/16) - A1(2i)

   A2_Tab_F : constant Float_Table :=
                (Root16_Half     - Float'Machine (Root16_Half),
                 Root16_Half**3  - Float'Machine (Root16_Half**3),
                 Root16_Half**5  - Float'Machine (Root16_Half**5),
                 Root16_Half**7  - Float'Machine (Root16_Half**7),
                 Root16_Half**9  - Float'Machine (Root16_Half**9),
                 Root16_Half**11 - Float'Machine (Root16_Half**11),
                 Root16_Half**13 - Float'Machine (Root16_Half**13),
                 Root16_Half**15 - Float'Machine (Root16_Half**15));

   Sqrt_Epsilon_F : constant Float :=
                      Sqrt_2 ** (1 - Float'Machine_Mantissa);

   --  Intermediary functions

   function Reconstruct_Pow
     (Z : Float;
      P : Integer;
      M : Integer) return Float;

   procedure Reduce_044
     (X         : Float;
      Reduced_X : out Float;
      P         : out Integer)
     with Post => abs (Reduced_X) < 0.044;

   function Reduce_1_16 (X : Float) return Float
     with Post => abs (X - Reduce_1_16'Result) <= 0.0625;

   function Reduce_1_16 (X : Float) return Float is
     (F'Machine_Rounding (X * 16.0) * (1.0 / 16.0));

   package Float_Approximations is
      new Generic_Approximations (Float, Mantissa => 24);

   use Float_Approximations;

   --  The following function reduces a positive X into the range
   --  -ln (2) / 2 .. ln (2) / 2

   --  It returns a reduced X and an integer N such that:
   --  X'Old = X'New + N * Log (2)

   --  It is used by Exp function

   --  The result should be correctly rounded

   procedure Reduce_Ln_2    (X : in out Float; N : out Integer)
     with Pre  => abs (X) <= Float'Ceiling
       (Float'Pred (88.72283_90520_7) * Inv_Ln_2);
   --  @llr Reduce_Ln_2 Float
   --  The following is postcondition doesn't hold. Suspicious "=" ???
   --  Post => abs (X) <= Ln_2 / 2.0 and
   --          X'Old = X + Float (N) * Ln_2;

   --  The reduction is used by the Sin, Cos and Tan functions.

   procedure Reduce_Half_Pi (X : in out Float; Q : out Quadrant)
     with Pre  => X >= 0.0,
          Post => abs (X) <= Max_Red_Trig_Arg;
   --  @llr Reduce_Half_Pi Float
   --  The following functions reduce a positive X into the range
   --  -(Pi/4 + E) .. Pi/4 + E, with E a small fraction of Pi.
   --
   --  The reason the normalization is not strict is that the computation of
   --  the number of times to subtract half Pi is not exact. The rounding
   --  error is worst for large arguments, where the number of bits behind
   --  the radix point reduces to half the mantissa bits.

   --  While it would be possible to correct for this, the polynomial
   --  approximations work well for values slightly outside the -Pi/4 .. Pi/4
   --  interval, so it is easier for both error analysis and implementation
   --  to leave the reduction non-strict, and assume the reduced argument is
   --  within -0.26 * Pi .. 0.26 * Pi rather than a quarter of pi.

   --  The reduction is guaranteed to be correct to within 0.501 ulp for
   --  values of X for which Ada's accuracy guarantees apply:
   --     abs X <= 2.0**(T'Machine_Mantissa / 2)
   --  For values outside this range, an attempt is made to have significance
   --  decrease only proportionally with increase of magnitued. In any case,
   --  for all finite arguments, the reduction will succeed, though the reduced
   --  value may not agree with the mathematically correct value in even its
   --  sign.

   procedure Reduce_Half_Pi_Large (X : in out F;  N : F; Q : out Quadrant);
   pragma Unreferenced (Reduce_Half_Pi_Large);

   procedure Split_Veltkamp (X : Float; X_Hi, X_Lo : out Float)
      with Post => X = X_Hi + X_Lo;

   function Multiply_Add (X, Y, Z : F)   return F  is (X * Y + Z);

   ---------------------
   -- Reconstruct_Pow --
   ---------------------

   function Reconstruct_Pow
     (Z : Float;
      P : Integer;
      M : Integer) return Float
   is
      --  Cody and Waite implementation (in "**" function page 84)

      Result : Float;

   begin
      Result := A1_Tab_F (P + 1) * Z;
      return Float'Scaling (Result, M);
   end Reconstruct_Pow;

   ----------------
   -- Reduce_044 --
   ----------------

   procedure Reduce_044
     (X         : Float;
      Reduced_X : out Float;
      P         : out Integer)
   is
      --  Cody and Waite implementation (in "**" function page 84)

   begin
      P := 1;

      if X <= A1_Tab_F (9) then
         P := 9;
      end if;

      if X <= A1_Tab_F (P + 4) then
         P := P + 4;
      end if;

      if X <= A1_Tab_F (P + 2) then
         P := P + 2;
      end if;

      Reduced_X := (X - A1_Tab_F (P + 1)) - A2_Tab_F ((P + 1) / 2);
      Reduced_X := Reduced_X / (X + A1_Tab_F (P + 1));
      Reduced_X := Reduced_X + Reduced_X;
   end Reduce_044;

   -----------------
   -- Reduce_Ln_2 --
   -----------------

   procedure Reduce_Ln_2 (X : in out Float; N : out Integer) is
      L1 : constant := Float'Leading_Part (Ln_2, F_HM);
      L2 : constant := Float'Leading_Part (Ln_2 - L1, F_HM);
      L3 : constant := Ln_2 - L1 - L2;
      XN : constant Float := Float'Rounding (X * Inv_Ln_2);

   begin
      --  The argument passed to the function is smaller than Ymax * 1/log(2)
      --  No overflow is possible for N (Ymax is the largest machine number
      --  less than Log (F'Last)).

      N := Integer (XN);
      X := ((X - XN * L1) - XN * L2) - XN * L3;

      if X < -Ln_2 / 2.0 then
         X := X + Ln_2;
         N := N - 1;
      end if;

      if X > Ln_2 / 2.0 then
         X := X - Ln_2;
         N := N + 1;
      end if;
   end Reduce_Ln_2;

   --------------------
   -- Split_Veltkamp --
   --------------------

   procedure Split_Veltkamp (X : Float; X_Hi, X_Lo : out Float) is
      M : constant F := 0.5 + 2.0**(1 - F'Machine_Mantissa / 2);
   begin
      X_Hi := X * M - (X * M - X);
      X_Lo := X - X_Hi;
   end Split_Veltkamp;

   --------------------------
   -- Reduce_Half_Pi_Large --
   --------------------------

   procedure Reduce_Half_Pi_Large (X : in out F; N : F; Q : out Quadrant) is
      type Int_64 is range -2**63 .. 2**63 - 1; -- used for conversions

      HM : constant Positive := F'Machine_Mantissa / 2;
      C1 : constant F := F'Leading_Part (Half_Pi, HM);
      C2 : constant F := F'Leading_Part (Half_Pi - C1, HM);
      C3 : constant F := F'Leading_Part (Half_Pi - C1 - C2, HM);
      C4 : constant F := Half_Pi - C1 - C2 - C3;

      K    : F := N;
      K_Hi : F;
      K_Lo : F;
      Tmp : Int_64;
   begin
      Q := 0;

      loop
         Split_Veltkamp (X => K, X_Hi => K_Hi, X_Lo => K_Lo);

         X := Multiply_Add (-K_Hi, C1, X);
         X := Multiply_Add (-K_Hi, C2, Multiply_Add (-K_Lo, C1, X));
         X := Multiply_Add (-K_Hi, C3, Multiply_Add (-K_Lo, C2, X));
         X := Multiply_Add (-K_Hi, C4, Multiply_Add (-K_Lo, C3, X));
         X := Multiply_Add (-K_Lo, C4, X);

         if abs K < 2.0**62 then
            Tmp := (Int_64 (Q) + Int_64 (N)) mod 4;
            Q := Quadrant (Tmp);

         elsif abs K_Lo <= 2.0**62 then
            Q := Quadrant ((Int_64 (Q) + Int_64 (N)) mod 4);
         end if;

         exit when X in -0.26 * Pi .. 0.26 * Pi;

         K := F'Machine_Rounding (X * Half_Pi**(-1));
      end loop;
   end Reduce_Half_Pi_Large;

   --------------------
   -- Reduce_Half_Pi --
   --------------------

   procedure Reduce_Half_Pi (X : in out Float; Q : out Quadrant) is
      K      : constant       := Pi / 2.0;
      Bits_N : constant       := 9;
      Max_N  : constant       := 2.0**Bits_N - 1.0;
      Max_X  : constant Float := Float'Pred (K * Max_N);
      Bits_C : constant       := Float'Machine_Mantissa - Bits_N;
      C1     : constant Float := Float'Leading_Part (K, Bits_C);
      C2     : constant Float := Float'Leading_Part (K - C1, Bits_C);
      C3     : constant Float := Float'Leading_Part (K - C1 - C2, Bits_C);
      C4     : constant Float := K - C1 - C2 - C3;
      N      : constant Float := Float'Machine_Rounding (X * K**(-1));

   begin
      if abs X < Max_X then

         --  X is in the range for which strict accuracy can be provided

         X := (((X - N * C1) - N * C2) - N * C3) - N * C4;
         Q := Integer (N) mod 4;

      elsif not X'Valid then

         --  X is an infinity or NaN

         X := X - X;
         Q := 0;

      else

         --  HACK ??? Use the first branch because we don't have 64bits
         --  precision integers on STM32F4

         X := (((X - N * C1) - N * C2) - N * C3) - N * C4;
         Q := Integer (N) mod 4;
      end if;
   end Reduce_Half_Pi;

   --------------------
   -- Instantiations --
   --------------------

   package Instantiations is
      function Acos  is new Generic_Acos (F);
      function Atan2 is new Generic_Atan2 (F);
   end Instantiations;

   ----------
   -- Acos --
   ----------

   function Acos (X : F)  return F  is (Instantiations.Acos (X));

   -----------
   -- Acosh --
   -----------

   function Acosh (X : F) return F is

      --  Math based implementation using Log1p: x-> Log (1+x)

      T : constant F := X - 1.0;

   begin
      if X > 1.0 / Sqrt_Epsilon_F then
         return Log (X) + Ln_2;
      elsif X < 2.0 then
         return Log1p (T + Sqrt (2.0 * T + T * T));
      else
         return Log (X + Sqrt ((X - 1.0) * (X + 1.0)));
      end if;
   end Acosh;

   ----------
   -- Asin --
   ----------

   function Asin (X : F)  return F  is (Float_Approximations.Asin (X));

   -----------
   -- Asinh --
   -----------

   function Asinh (X : F) return F is

      --  Math based implementation using Log1p: x-> Log (1+x)

      Y   : constant F := abs X;
      G   : constant F := X * X;
      Res : F;

   begin
      if Y < Sqrt_Epsilon_F then
         Res := Y;
      elsif Y > 1.0 / Sqrt_Epsilon_F then
         Res := Log (Y) + Ln_2;
      elsif Y < 2.0 then
         Res := Log1p (Y + G / (1.0 + Sqrt (G + 1.0)));
      else
         Res := Log (Y + Sqrt (G + 1.0));
      end if;

      return F'Copy_Sign (Res, X);
   end Asinh;

   ----------
   -- Atan --
   ----------

   function Atan (X : F)  return F  is (Instantiations.Atan2 (X, 1.0));

   -----------
   -- Atan2 --
   -----------

   function Atan2 (Y, X : F)  return F  is (Instantiations.Atan2 (Y, X));

   -----------
   -- Atanh --
   -----------

   function Atanh (X : F) return F is

      --  Math based implementation using Log1p: x-> Log (1+x)

     (if X >= 0.0
      then Log1p (2.0 * X / (1.0 - X)) / 2.0
      else -Log1p (-2.0 * X / (1.0 + X)) / 2.0);

   ---------
   -- Cos --
   ---------

   function Cos (X : F) return F is

      --  Math based implementation using Hart constants

      Y      : F := abs (X);
      Q      : Quadrant;
      Result : F;

   begin
      Reduce_Half_Pi (Y, Q);

      if Q mod 2 = 0 then
         Result := Approx_Cos (Y);
      else
         Result := Approx_Sin (Y);
      end if;

      return (if Q = 1 or else Q = 2 then -Result else Result);
   end Cos;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : F) return F is

      --  Cody and Waite implementation (page 217)

      Y : constant F := abs (X);

      --  Because the overflow threshold for cosh(X) is beyond the overflow
      --  threshold for exp(X), it appears natural to reformulate the
      --  computation as:

      --    Cosh (X) = Exp (X - Log (2))

      --  But because Log (2) is not an exact machine number, the finite word
      --  length of the machine implies that the absolute error in X - Log (2),
      --  and hence the transmitted error in Cosh (X) is proportional to the
      --  magnitude of X even when X is error-free.

      --  To avoid this problem, we revise the computation to:

      --    Cosh (X) = V/2 * exp(X - Log (V))

      --  where Log (V) is an exact machine number slightly larger than Log (2)
      --  with the last few digits of its significand zero.

      Ln_V : constant := 8#0.542714#;
      --  Machine value slightly above Ln_2

      V_2 : constant := 0.24999_30850_04514_99336;
      --  V**(-2)

      V_2_1 : constant := 0.13830_27787_96019_02638E-4;
      --  V / 2 - 1

      Y_Bar : constant := 88.72283_93554_69;
      --  The last floating point for which exp (Y) does not overflow and
      --  exp (-Y) does not underflow.

      W : F;
      Z : F;

   begin
      if Y >= Y_Bar then
         W := Y - Ln_V;
         Z := Exp (W);
         Z := Z + V_2 / Z;

         return Z + V_2_1 * Z; --  rewriting of V/2 * Z

      else
         Z := Exp (X);
         return (Z + 1.0 / Z) / 2.0;
      end if;
   end Cosh;

   ---------
   -- Exp --
   ---------

   function Exp (X : F) return F is

      --  Cody and Waite implementation (page 60)

      N : Integer;
      Y : F := X;
      R : F;

      Ymax : constant Float := F'Pred (88.72283_90520_7);
      --  The largest machine number less than Log (F'Last)

   begin

      if abs (Y) < 2.0**(-F'Machine_Mantissa - 1) then
         return 1.0;
      end if;

      if abs Y > Ymax then
         return (if Y > 0.0 then Infinity else 0.0);
      end if;

      Reduce_Ln_2 (Y, N);
      R := Approx_Exp (Y);
      return Float'Scaling (R, N);
   end Exp;

   ----------
   -- Exp2 --
   ----------

   function Exp2 (X : F) return F is

      --  Implementation based on Cody and Waite Exp implementation (page 217)
      --  but using Hart constants

      N : Integer;
      Y : F := X;
      R : F;

      Result : F;

   begin
      if abs (Y) < 2.0**(-F'Machine_Mantissa - 1) then
         return 1.0;
      end if;

      if abs Y > F'Pred (F (F'Machine_Emax)) then
         return (if Y > 0.0 then Infinity else 0.0);
      end if;

      N := Integer (X);
      Y := Y - Float (N);
      R := Approx_Exp2 (Y);
      Result := Float'Scaling (R, N + 1);

      if Result /= Result then
         return (if X < F'First then 0.0 else Infinity);
      else
         return Result;
      end if;
   end Exp2;

   ---------
   -- Log --
   ---------

   function Log (X : F) return F is

      --  Cody and Waite implementation (page 35)

      Exponent_X  : constant Integer := F'Exponent (X);
      XN          : F                := F (Exponent_X);
      Mantissa_X  : F                := F'Scaling (X, -Exponent_X);
      HM_F        : constant Integer :=  Integer (F'Machine_Mantissa / 2);
      L1          : constant F       := F'Leading_Part (Ln_2, HM_F);
      L2          : constant F       := Ln_2 - L1;
      Result      : F;

   begin
      if X <= 0.0 then
         if X < 0.0 then
            return NaN;
         else
            return -Infinity;
         end if;

      elsif X > Float'Last then
         return X;

         --  Making sure X is in Sqrt(0.5) .. Sqrt (2)

      elsif Mantissa_X <= Sqrt_Half then
         XN := XN - 1.0;
         Mantissa_X := Mantissa_X * 2.0;
      end if;

      Result := Approx_Log (Mantissa_X);
      Result := (XN * L2 + Result) + XN * L1;

      return Result;
   end Log;

   -----------
   -- Log1p --
   -----------

   function Log1p (X : Float) return Float is

      --  Quick implementation of Log1p not accurate to the Ada regular Log
      --  requirements, but accurate enough to compute inverse hyperbolic
      --  functions.

   begin
      if 1.0 + X = 1.0 then
         return X;
      elsif X > Float'Last then
         return X;
      else
         return Log (1.0 + X) * (X / ((1.0 + X) - 1.0));
      end if;
   end Log1p;

   ----------
   -- Log2 --
   ----------

   function Log2 (X : F) return F is

      --  Quick implementation of Log2 not accurate to the Ada regular Log
      --  (base e) requirement on the whole definition interval but accurate
      --  enough on 0 .. 2**(-1/16).

     (Log (X) * (1.0 / Ln_2));

   ----------
   -- Pow --
   ----------

   function Pow (Left, Right : F) return F is

      --  Cody and Waite implementation (page 84)

      procedure Pow_Special_Cases is new Generic_Pow_Special_Cases (F);

      One_Over_Sixteen : constant := 0.0625;

      M : constant Integer := F'Exponent (Left);
      G : constant F       := F'Fraction (Left);
      Y : constant F       := Right;
      Z : F;
      P : Integer;

      U2, U1, Y1, Y2, W1, W2, W : F;
      MM, PP, IW1, I            : Integer;
      Is_Special                : Boolean;
      Special_Result            : F;

   begin
      --  Special values

      Pow_Special_Cases (Left, Right, Is_Special, Special_Result);

      if Is_Special then
         return Special_Result;
      else
         --  Left**Right is calculated using the formula
         --  2**(Right * Log2 (Left))

         Reduce_044 (G, Z, P);

         --  At this point, Z <= 0.044

         U2 := Approx_Power_Log (Z);
         U1 := F (M * 16 - P) * 0.0625; --  U2 + U1 = Log2 (Left)

         --  Forming the pseudo extended precision product of U * Right

         Y1  := Reduce_1_16 (Y);
         Y2  := Y - Y1;

         W   := U2 * Y + U1 * Y2;
         W1  := Reduce_1_16 (W);
         W2  := W - W1;

         W   := W1 + U1 * Y1;
         W1  := Reduce_1_16 (W);
         W2  := W2 + (W - W1);

         W   := Reduce_1_16 (W2);
         IW1 := Integer (16.0 * (W1 + W));
         W2  := W2 - W;

         if W2 > 0.0 then
            W2 := W2 - One_Over_Sixteen;
            IW1 := 1 + IW1;
         end if;

         if IW1 < 0 then
            I := 0;
         else
            I := 1;
         end if;

         MM := Integer (IW1  / 16) + I;
         PP := 16 * MM - IW1;

         Z := Approx_Exp2 (W2);
         return Reconstruct_Pow (Z, PP, MM);
      end if;
   end Pow;

   ---------
   -- Sin --
   ---------

   function Sin (X : F) return F is

      --  Math based implementation using Hart constants

      Y      : F := abs X;
      Q      : Quadrant;
      Result : F;

   begin
      Reduce_Half_Pi (Y, Q);

      Result := (if Q mod 2 = 0 then Approx_Sin (Y) else Approx_Cos (Y));

      return F'Copy_Sign (1.0, X) * (if Q >= 2 then -Result else Result);
   end Sin;

   -----------
   -- Sinh --
   -----------

   function Sinh (X : F) return F is

      --  Cody and Waite implementation (page 217)

      Sign : constant F := F'Copy_Sign (1.0, X);
      Y    : constant F := abs (X);

      --  Because the overflow threshold for sinh(X) is beyond the overflow
      --  threshold for exp(X), it appears natural to reformulate the
      --  computation as:

      --    Sinh (X) = Exp (X - Log (2))

      --  But because Log (2) is not an exact machine number, the finite word
      --  length of the machine implies that the absolute error in X - Log (2),
      --  and hence the transmitted error in Sinh (X), is proportional to the
      --  magnitude of X even when X is error-free.

      --  To avoid this problem, we revise the computation to:

      --    Sinh (X) = V/2 * exp(X - Log (V))

      --  where Log (V) is an exact machine number slightly larger than Log (2)
      --  with the last few digits of its significand zero.

      Ln_V : constant := 8#0.542714#;
      --  Machine value slightly above Ln_2

      V_2 : constant := 0.24999_30850_04514_99336;
      --  V**(-2)

      V_2_1 : constant := 0.13830_27787_96019_02638E-4;
      --  V / 2 - 1

      Y_Bar : constant := 88.72283_93554_69;
      --  The last floating point for which exp (X) does not overflow and
      --  exp (-x) does not underflow

      W : F;
      Z : F;

   begin
      if Y < 1.0 then
         return Approx_Sinh (X);
      end if;

      if Y >= Y_Bar then
         W := Y - Ln_V;
         Z := Exp (W);
         Z := Z - V_2 / Z;

         return Sign * (Z + V_2_1 * Z); --  rewriting of V/2 * Z

      else
         Z := Exp (Y);
         return Sign * ((Z - 1.0 / Z) / 2.0);
      end if;
   end Sinh;

   ---------
   -- Tan --
   ---------

   function Tan (X : F) return F is

      --  Math based implementation using Hart constants

      Y : F := abs X;
      N : Integer;

   begin
      Reduce_Half_Pi (Y, N);

      --  The reconstruction is included in the algebraic fraction in
      --  Approx_Tan function.

      if N mod 2 = 0 then
         return Approx_Tan (Y) * F'Copy_Sign (1.0, X);

      else
         return Approx_Cot (Y) * F'Copy_Sign (1.0, X);
      end if;
   end Tan;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : F) return F is

      --  Cody and Waite implementation (page 239)

      Y      : constant F := abs (X);
      Xbig   : constant := Ln_2 * F (1 + F'Machine_Mantissa);
      LN_3_2 : constant := 0.54930_61443_34054_84570;
      Result : F;

   begin
      if Y > Xbig then
         Result := 1.0;
      else
         if Y > LN_3_2 then
            Result := 1.0 - 2.0 / (Exp (2.0 * Y) + 1.0);
         else
            Result := Approx_Tanh (Y);
         end if;
      end if;

      return F'Copy_Sign (Result, X);
   end Tanh;

end Libm_Single;
