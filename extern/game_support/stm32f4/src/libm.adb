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

package body Libm is
   type Unsigned_64 is mod 2**64;

   generic
      type T is private;
      with function Multiply_Add (X, Y, Z : T) return T is <>;
      --  The Multiply_Add function returns the value of X * Y + Z, ideally
      --  (but not necessarily) using a wider intermediate type, or a fused
      --  multiply-add operation with only a single rounding. They are used
      --  for evaluating polynomials.
   package Generic_Polynomials is

      type Polynomial is array (Natural range <>) of T;
      --  A value P of type PolynomialRepresents the polynomial
      --    P (X) = P_0 + P_1 * X + ... + P_(n-1) * X**(n-1) + P_n * X**n,
      --
      --  where n = P'Length - 1, P_0 is P (P'First) and P_n is P (P'Last)

      --  P (X) = P_0 + X * (P_1 + X * (P_2 + X * (... + X * P_n)))

      function Compute_Horner (P : Polynomial; X : T) return T with Inline;
      --  Computes the polynomial P using the Horner scheme:
      --     P (X) = P_0 + X * (P_1 + X * (P_2 + X * (... + X * P_n)))
   end Generic_Polynomials;

   ------------------------
   -- Generic_Polynomial --
   ------------------------

   package body Generic_Polynomials is

      --------------------
      -- Compute_Horner --
      ---------------------

      function Compute_Horner (P : Polynomial; X : T) return T is
         Result : T := P (P'Last);

      begin
         for P_j of reverse P (P'First .. P'Last - 1) loop
            Result := Multiply_Add (Result, X, P_j);
         end loop;

         return Result;
      end Compute_Horner;
   end Generic_Polynomials;

   ----------------------------------
   -- Generic_Float_Approximations --
   ----------------------------------

   package body Generic_Approximations is

      function Multiply_Add (X, Y, Z : T) return T is (X * Y + Z);

      package Float_Polynomials is new Generic_Polynomials (T);
      use Float_Polynomials;

      -----------------
      -- Approx_Asin --
      -----------------

      function Approx_Asin (X : T) return T is
         P : T;
         Q : T;

      begin
         if Mantissa <= 24 then
            declare

               --  Approximation MRE = 6.0128E-9

               P1 : constant T := Exact (0.93393_5835);
               P2 : constant T := Exact (-0.50440_0557);

               Q0 : constant T := Exact (5.6036_3004);
               Q1 : constant T := Exact (-5.5484_6723);

            begin
               P := Compute_Horner ((P1, P2), X);
               Q := Compute_Horner ((Q0, Q1 + X), X);
            end;

         else
            declare
               --  Approximation MRE = 2.0975E-18

               P1 : constant T := Exact (-0.27368_49452_41642_55994E+2);
               P2 : constant T := Exact (+0.57208_22787_78917_31407E+2);
               P3 : constant T := Exact (-0.39688_86299_75048_77339E+2);
               P4 : constant T := Exact (+0.10152_52223_38064_63645E+2);
               P5 : constant T := Exact (-0.69674_57344_73506_46411);

               Q0 : constant T := Exact (-0.16421_09671_44985_60795E+3);
               Q1 : constant T := Exact (+0.41714_43024_82604_12556E+3);
               Q2 : constant T := Exact (-0.38186_30336_17501_49284E+3);
               Q3 : constant T := Exact (+0.15095_27084_10306_04719E+3);
               Q4 : constant T := Exact (-0.23823_85915_36702_38830E+2);

            begin
               P := Compute_Horner ((P1, P2, P3, P4, P5), X);
               Q := Compute_Horner ((Q0, Q1, Q2, Q3, Q4 + X), X);
            end;
         end if;

         return X * P / Q;
      end Approx_Asin;

      -----------------
      -- Approx_Atan --
      -----------------

      function Approx_Atan (X : T) return T is
         G    : constant T := X * X;
         P, Q : T;

      begin
         if Mantissa <= 24 then
            declare
               --  Approximation MRE = 3.2002E-9

               P0 : constant T := Exact (-0.47083_25141);
               P1 : constant T := Exact (-0.50909_58253E-1);

               Q0 : constant T := Exact (0.14125_00740E1);

            begin
               P := Compute_Horner ((P0, P1), G);
               Q := Q0 + G;
            end;

         else
            declare
               --  Approximation MRE = 1.8154E-18

               P0 : constant T := Exact (-0.13688_76889_41919_26929E2);
               P1 : constant T := Exact (-0.20505_85519_58616_51981E2);
               P2 : constant T := Exact (-0.84946_24035_13206_83534E1);
               P3 : constant T := Exact (-0.83758_29936_81500_59274);

               Q0 : constant T := Exact (0.41066_30668_25757_81263E2);
               Q1 : constant T := Exact (0.86157_34959_71302_42515E2);
               Q2 : constant T := Exact (0.59578_43614_25973_44465E2);
               Q3 : constant T := Exact (0.15024_00116_00285_76121E2);

            begin
               P := Compute_Horner ((P0, P1, P2, P3), G);
               Q := Compute_Horner ((Q0, Q1, Q2, Q3 + G), G);
            end;
         end if;

         return Multiply_Add (X, (G * P / Q), X);
      end Approx_Atan;

      function Approx_Cos (X : T) return T is
         Cos_P : constant Polynomial :=
                   (if Mantissa <= 24
                    then
                      --  Hart's constants : #COS 3822# (p. 209)
                      --  Approximation MRE = 8.1948E-9

                      (0 => Exact (1.0),
                       1 => Exact (-0.49999_99404),
                       2 => Exact (0.41666_66046E-1),
                       3 => Exact (-0.13888_87875E-2),
                       4 => Exact (0.24827_63739E-4))

                    else
                      --  Hart's constants : #COS 3824# (p. 209)
                      --  Approximation MRE = 1.2548E-18

                      (0 => Exact (1.0),
                       1 => Exact (-0.5),
                       2 => Exact (+0.04166_66666_66666_43537),
                       3 => Exact (-0.13888_88888_88589_63271E-2),
                       4 => Exact (+0.24801_58728_28994_71149E-4),
                       5 => Exact (-0.27557_31286_56960_91429E-6),
                       6 => Exact (+0.20875_55514_56778_91895E-8),
                       7 => Exact (-0.11352_12320_57839_46664E-10)));
      begin
         return Compute_Horner (Cos_P, X * X);
      end Approx_Cos;

      ----------------
      -- Approx_Exp --
      ----------------

      function Approx_Exp (X : T) return T is
         Exp_P : constant Polynomial :=
                   (if Mantissa <= 24
                    then --  Approximation MRE = 8.1529E-10
                      (0 => Exact (0.24999_99995_0),
                       1 => Exact (0.41602_88626_0E-2))
                    else --  Approximation MRE = 1.0259E-17
                      (0 => Exact (0.24999_99999_99999_993),
                       1 => Exact (0.69436_00015_11792_852E-2),
                       2 => Exact (0.16520_33002_68279_130E-4)));

         Exp_Q : constant Polynomial :=
                   (if Mantissa <= 24
                    then
                      (0 => Exact (0.5),
                       1 => Exact (0.49987_17877_8E-1))
                    else
                      (0 => Exact (0.5),
                       1 => Exact (0.55553_86669_69001_188E-1),
                       2 => Exact (0.49586_28849_05441_294E-3)));

         G  : constant T := X * X;
         P  : T;
         Q  : T;

      begin
         P := Compute_Horner (Exp_P, G);
         Q := Compute_Horner (Exp_Q, G);

         return Exact (2.0) * Multiply_Add (X, P / (Multiply_Add (-X, P, Q)),
                                            Exact (0.5));
      end Approx_Exp;

      ----------------
      -- Approx_Log --
      ----------------

      function Approx_Log (X : T) return T is

         Log_P : constant Polynomial :=
                   (if Mantissa <= 24
                    then --  Approximation MRE = 1.0368E-10
                      (0 => Exact (-0.46490_62303_464),
                       1 => Exact (0.013600_95468_621))
                    else --  Approximation MRE = 4.7849E-19
                      (0 => Exact (-0.64124_94342_37455_81147E+2),
                       1 => Exact (0.16383_94356_30215_34222E+2),
                       2 => Exact (-0.78956_11288_74912_57267)));

         Log_Q : constant Polynomial :=
                   (if Mantissa <= 24
                    then
                      (0 => Exact (-5.5788_73750_242),
                       1 => Exact (1.0))
                    else
                      (0 => Exact (-0.76949_93210_84948_79777E+3),
                       1 => Exact (0.31203_22209_19245_32844E+3),
                       2 => Exact (-0.35667_97773_90346_46171E+2),
                       3 => Exact (1.0)));

         G  : T;
         P  : T;
         Q  : T;

         ZNum, ZDen, Z : T;

      begin
         ZNum := (X + Exact (-0.5)) + Exact (-0.5);
         ZDen := X * Exact (0.5) + Exact (0.5);
         Z := ZNum / ZDen;
         G := Z * Z;
         P := Compute_Horner (Log_P, G);
         Q := Compute_Horner (Log_Q, G);
         return Multiply_Add (Z, G * (P / Q), Z);
      end Approx_Log;

      ----------------------
      -- Approx_Power Log --
      ----------------------

      function Approx_Power_Log (X : T) return T is
         Power_Log_P  : constant Polynomial :=
                          (if Mantissa <= 24
                           then --  Approximation MRE = 7.9529E-4
                             (1 => Exact (0.83357_541E-1))
                           else --  Approximation MRE = 8.7973E-8
                             (1 => Exact (0.83333_33333_33332_11405E-1),
                              2 => Exact (0.12500_00000_05037_99174E-1),
                              3 => Exact (0.22321_42128_59242_58967E-2),
                              4 => Exact (0.43445_77567_21631_19635E-3)));

         K : constant T := Exact (0.44269_50408_88963_40736);
         G : constant T := X * X;
         P : T;

      begin
         P := Compute_Horner (Power_Log_P, G);
         P := (P * G) * X;
         P := Multiply_Add (P, K, P);

         return Multiply_Add (X, K, P) + X;
      end Approx_Power_Log;

      -----------------
      -- Approx_Exp2 --
      -----------------

      function Approx_Exp2 (X : T) return T is
         Exp2_P : constant Polynomial :=
                    (if Mantissa > 24
                     then --  Approximation MRE = 1.7418E-17
                       (1 => Exact (0.69314_71805_59945_29629),
                        2 => Exact (0.24022_65069_59095_37056),
                        3 => Exact (0.55504_10866_40855_95326E-1),
                        4 => Exact (0.96181_29059_51724_16964E-2),
                        5 => Exact (0.13333_54131_35857_84703E-2),
                        6 => Exact (0.15400_29044_09897_64601E-3),
                        7 => Exact (0.14928_85268_05956_08186E-4))
                     else --  Approximation MRE = 3.3642E-9
                       (1 => Exact (0.69314_675),
                        2 => Exact (0.24018_510),
                        3 => Exact (0.54360_383E-1)));
      begin
         return Exact (1.0) + Compute_Horner (Exp2_P, X) * X;
      end Approx_Exp2;

      ----------------
      -- Approx_Sin --
      ----------------

      function Approx_Sin  (X : T) return T is
         Sin_P  : constant Polynomial :=
                    (if Mantissa <= 24
                     then --  Hart's constants: #SIN 3040# (p. 199)
                       (1 => Exact (-0.16666_66567),
                        2 => Exact (0.83320_15015E-2),
                        3 => Exact (-0.19501_81031E-3))
                     else --  Hart's constants: #SIN 3044# (p. 199)
                       --  Approximation MRE = 2.4262E-18
                       (1 => Exact (-0.16666_66666_66666_71293),
                        2 => Exact (0.83333_33333_33332_28093E-2),
                        3 => Exact (-0.19841_26984_12531_12013E-3),
                        4 => Exact (0.27557_31921_33901_79497E-5),
                        5 => Exact (-0.25052_10473_82673_44045E-7),
                        6 => Exact (0.16058_34762_32246_14953E-9),
                        7 => Exact (-0.75778_67884_01271_54819E-12)));

         G  : constant T := X * X;

         Sqrt_Epsilon_LF : constant Long_Float :=
                       Sqrt_2 ** (1 - Long_Float'Machine_Mantissa);

      begin
         if abs X <= Exact (Sqrt_Epsilon_LF) then
            return X;
         end if;

         return Multiply_Add (X, Compute_Horner (Sin_P, G) * G, X);
      end Approx_Sin;

      -----------------
      -- Approx_Sinh --
      -----------------

      function Approx_Sinh (X : T) return T is
         Sinh_P : constant Polynomial :=
                    (if Mantissa <= 24
                     then --  Approximation MRE = 2.6841E-8
                       (0 => Exact (-0.71379_3159E1),
                        1 => Exact (-0.19033_3300))
                     else --  Approximation MRE = 4.6429E-18
                       (0 => Exact (-0.35181_28343_01771_17881E6),
                        1 => Exact (-0.11563_52119_68517_68270E5),
                        2 => Exact (-0.16375_79820_26307_51372E3),
                        3 => Exact (-0.78966_12741_73570_99479)));

         Sinh_Q : constant Polynomial :=
                    (if Mantissa <= 24
                     then
                       (0 => Exact (-0.42827_7109E2),
                        1 => Exact (1.0))
                     else
                       (0 => Exact (-0.21108_77005_81062_71242E7),
                        1 => Exact (0.36162_72310_94218_36460E5),
                        2 => Exact (-0.27773_52311_96507_01667E3),
                        3 => Exact (1.0)));

         G : constant T := X * X;
         P : T;
         Q : T;

      begin
         P := Compute_Horner (Sinh_P, G);
         Q := Compute_Horner (Sinh_Q, G);

         return Multiply_Add (X, (G * P / Q), X);
      end Approx_Sinh;

      ----------------
      -- Approx_Tan --
      ----------------

      function Approx_Tan (X : T) return T is
         Tan_P  : constant Polynomial :=
                    (if Mantissa <= 24
                     then --  Approximation MRE = 2.7824E-8
                       (1 => Exact (-0.95801_7723E-1))
                     else --  Approximation MRE = 3.5167E-18
                       (1 => Exact (-0.13338_35000_64219_60681),
                        2 => Exact (0.34248_87823_58905_89960E-2),
                        3 => Exact (-0.17861_70734_22544_26711E-4)));

         Tan_Q  : constant Polynomial :=
                    (if Mantissa <= 24
                     then
                       (0 => Exact (1.0),
                        1 => Exact (-0.42913_5777),
                        2 => Exact (0.97168_5835E-2))
                     else
                       (0 => Exact (1.0),
                        1 => Exact (-0.46671_68333_97552_94240),
                        2 => Exact (0.25663_83228_94401_12864E-1),
                        3 => Exact (-0.31181_53190_70100_27307E-3),
                        4 => Exact (0.49819_43399_37865_12270E-6)));

         G : constant T := X * X;
         P : constant T := Multiply_Add (X, G * Compute_Horner (Tan_P, G), X);
         Q : constant T := Compute_Horner (Tan_Q, G);

      begin
         return P / Q;
      end Approx_Tan;

      ----------------
      -- Approx_Cot --
      ----------------

      function Approx_Cot (X : T) return T is
         Tan_P  : constant Polynomial :=
                    (if Mantissa <= 24
                     then --  Approxmiation MRE = 1.5113E-17
                       (1 => Exact (-0.95801_7723E-1))
                     else
                       (1 => Exact (-0.13338_35000_64219_60681),
                        2 => Exact (0.34248_87823_58905_89960E-2),
                        3 => Exact (-0.17861_70734_22544_26711E-4)));

         Tan_Q  : constant Polynomial :=
                    (if Mantissa <= 24
                     then
                       (0 => Exact (1.0),
                        1 => Exact (-0.42913_5777),
                        2 => Exact (0.97168_5835E-2))
                     else
                       (0 => Exact (1.0),
                        1 => Exact (-0.46671_68333_97552_94240),
                        2 => Exact (0.25663_83228_94401_12864E-1),
                        3 => Exact (-0.31181_53190_70100_27307E-3),
                        4 => Exact (0.49819_43399_37865_12270E-6)));
         G : constant T := X * X;
         P : constant T := Multiply_Add (X, G * Compute_Horner (Tan_P, G), X);
         Q : constant T := Compute_Horner (Tan_Q, G);

      begin
         return -Q / P;
      end Approx_Cot;

      -----------------
      -- Approx_Tanh --
      -----------------

      function Approx_Tanh (X : T) return T is
         Tanh_P : constant Polynomial :=
                    (if Mantissa <= 24
                     then --  Approximation MRE = 2.7166E-9
                       (0 => Exact (-0.82377_28127),
                        1 => Exact (-0.38310_10665E-2))
                     else --  Approximation MRE = 3.2436E-18
                       (0 => Exact (-0.16134_11902_39962_28053E4),
                        1 => Exact (-0.99225_92967_22360_83313E2),
                        2 => Exact (-0.96437_49277_72254_69787)));

         Tanh_Q : constant Polynomial :=
                    (if Mantissa <= 24
                     then
                       (0 => Exact (2.4713_19654),
                        1 => Exact (1.0))
                     else
                       (0 => Exact (0.48402_35707_19886_88686E4),
                        1 => Exact (0.22337_72071_89623_12926E4),
                        2 => Exact (0.11274_47438_05349_49335E3),
                        3 => Exact (1.0)));

         G    : constant T := X * X;
         P, Q : T;

      begin
         P := Compute_Horner (Tanh_P, G);
         Q := Compute_Horner (Tanh_Q, G);

         return Multiply_Add (X, G * P / Q, X);
      end Approx_Tanh;

      ----------
      -- Asin --
      ----------

      function Asin (X : T) return T is

         --  Cody and Waite implementation (page 174)

         Y      : T := abs X;
         G      : T;
         Result : T;

      begin
         if Y <= Exact (0.5) then
            Result := X + X * Approx_Asin (X * X);

         else
            G := (Exact (1.0) + (-Y)) * Exact (0.5);
            Y := Sqrt (G);

            Result :=
              Exact (Pi / 2.0) - Exact (2.0) * (Y + Y * Approx_Asin (G));

            if not (Exact (0.0) <= X) then
               Result := -Result;
            end if;
         end if;

         return Result;
      end Asin;

   end Generic_Approximations;

   ------------------
   -- Generic_Acos --
   ------------------

   function Generic_Acos (X : T) return T is

      --  Cody and Waite implementation (page 174)

      Y      : T := abs (X);
      G      : T;
      Result : T;

   begin
      if Y <= 0.5 then

         --  No reduction needed

         G := Y * Y;
         Result := T'Copy_Sign (Y + Y * Approx_Asin (G), X);
         return 0.5 * Pi - Result;
      end if;

      --  In the reduction step that follows, it is not Y, but rather G that
      --  is reduced. The reduced G is in 0.0 .. 0.25.

      G := (1.0 - Y) / 2.0;
      Y := -2.0 * Sqrt (G);

      Result := Y + Y * Approx_Asin (G);

      return (if X < 0.0 then Pi + Result else -Result);
   end Generic_Acos;

   -------------------
   -- Generic_Atan2 --
   -------------------

   function Generic_Atan2 (Y, X : T) return T is

      --  Cody and Waite implementation (page 194)

      F : T;
      N : Integer := -1;

      --  Default value for N is -1 so that if X=0 or over/underflow
      --  tests on N are all false.

      Result : T;

   begin
      if Y = 0.0 then
         if T'Copy_Sign (1.0, X) < 0.0 then
            return T'Copy_Sign (Pi, Y);
         else
            return T'Copy_Sign (0.0, Y);
         end if;

      elsif X = 0.0 then
         return T'Copy_Sign (Half_Pi, Y);

      elsif abs (Y) > T'Last * abs (X) then  --  overflow
         Result := T (Half_Pi);

      elsif abs (X) > T'Last * abs (Y) then  --  underflow
         Result := 0.0;

      elsif abs (X) > T'Last and then abs (Y) > T'Last then

         --  NaN

         if X < 0.0 then
            return T'Copy_Sign (3.0 * Pi / 4.0, Y);
         else
            return T'Copy_Sign (Pi / 4.0, Y);
         end if;

      else
         F := abs (Y / X);

         if F > 1.0 then
            F := 1.0 / F;
            N := 2;
         else
            N := 0;
         end if;

         if F > 2.0 - Sqrt_3 then
            F :=  (((Sqrt_3 - 1.0) * F - 1.0) + F) / (Sqrt_3 + F);
            N := N + 1;
         end if;

         Result := Approx_Atan (F);
      end if;

      if N > 1 then
         Result := -Result;
      end if;

      case N is
         when 1 => Result := Result + Sixth_Pi;
         when 2 => Result := Result + Half_Pi;
         when 3 => Result := Result + Third_Pi;
         when others => null;
      end case;

      if T'Copy_Sign (1.0, X) < 0.0 then
         Result := Pi - Result;
      end if;

      return T'Copy_Sign (Result, Y);
   end Generic_Atan2;

   procedure Generic_Pow_Special_Cases
     (Left       : T;
      Right      : T;
      Is_Special : out Boolean;
      Result     : out T)
   is
      ------------
      -- Is_Even --
      ------------

      function Is_Even (X : T) return Boolean is
        (abs X >= 2.0**T'Machine_Mantissa
          or else Unsigned_64 (abs X) mod 2 = 0);
      pragma Assert (T'Machine_Mantissa <= 64);
      --  If X is large enough, then X is a multiple of 2. Otherwise,
      --  conversion to Unsigned_64 is safe, assuming a mantissa of at
      --  most 64 bits.

   begin
      Is_Special := True;
      Result := 0.0;

      --  value 'Result' is not used if the input is
      --  not a couple of special values

      if Right = 0.0 or else not (Left /= 1.0) then
         Result := (if Right = 0.0 then 1.0 else Left);

      elsif Left = 0.0 then
         if Right < 0.0 then
            if Right = T'Rounding (Right) and then not Is_Even (Right) then
               Result := 1.0 / Left; -- Infinity with sign of Left
            else
               Result := 1.0 / abs Left; -- +Infinity
            end if;

         else
            if Right = T'Rounding (Right)
              and then not Is_Even (Right)
            then
               Result := Left;
            else
               Result := +0.0;
            end if;
         end if;

      elsif abs (Right) > T'Last and then Left = -1.0 then
         Result := 1.0;

      elsif Left < 0.0
        and then Left >= T'First
        and then abs (Right) <= T'Last
        and then Right /= T'Rounding (Right)
      then
         Result := 0.0 / (Left - Left); -- NaN

      elsif Right < T'First then
         if abs (Left) < 1.0 then
            Result := -Right; -- Infinity
         else
            Result := 0.0; --  Cases where Left=+-1 are dealt with above
         end if;

      elsif Right > T'Last then
         if abs (Left) < 1.0 then
            Result := 0.0;
         else
            Result := Right;
         end if;

      elsif Left > T'Last then
         if Right < 0.0 then
            Result := 0.0;
         else
            Result := Left;
         end if;

      elsif Left < T'First then
         if Right > 0.0 then
            if Right = T'Rounding (Right)
              and then not Is_Even (Right)
            then
               Result := Left;
            else
               Result := -Left; --  -Left = +INF
            end if;
         else
            if Right = T'Rounding (Right)
              and then not Is_Even (Right)
            then
               Result := -0.0;
            else
               Result := +0.0;
            end if;
         end if;

      else
         Is_Special := False;
      end if;
   end Generic_Pow_Special_Cases;

end Libm;
