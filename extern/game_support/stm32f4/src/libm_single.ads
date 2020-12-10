------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . L I B M _ S I N G L E                   --
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

--  This is the Ada Cert Math specific version of s-libsin.ads

--  @llrset Libm
--  LLR Libm
--  ========

package Libm_Single is
   pragma Pure;

   --  This package provides an implementation of the various C99 functions
   --  used in the Ada run time. It is intended to be used for targets that
   --  do not have a C math library, or where the C math library isn't of
   --  sufficient quality and accuracy to meet Ada requirements.

   --  In case of error conditions, NaNs or infinities are returned as
   --  recommended in clause F.9 of the C99 standard. When called from C code,
   --  the implementation behaves as if the FENV_ACCESS state is off, assuming
   --  default rounding behavior and exception behavior.

   --  The following C99 elementary functions are provided for single
   --  precision (with the "f" suffix):

   --    Acos, Acosh, Asin, Asinh, Atan, Atan2, Atanh, Cosh, Exp, Exp2, Log,
   --    Log1p, Log2, Pow, Sin, Sinh, Tan, Tanh

   --  All functions with a NaN argument return a NaN result, except where
   --  stated otherwise. Unless otherwise specified, where the symbol +- occurs
   --  in both an argument and the result, the result has the same sign as
   --  the argument.

   --  Each function lists C special values, Ada expected values as well as
   --  Ada accuracy requirements the function meets. For accuracy requirements
   --  the maximum relative error (abbreviated as MRE) is given, as well as
   --  the domain for which the accuracy is guaranteed, where applicable.
   --  The maximum relative error is expressed as multiple of Eps,
   --  where Eps is Float'Model_Epsilon.

   --  What about principal branch ???

   ----------
   -- Acos --
   ----------

   function Acos (X : Float) return Float
      with Export, Convention => C, External_Name => "acosf";
   --  @llr acos (Float) Special Values
   --  The Acos function shall return the following special values:
   --
   --  C99 special values:
   --    Acos (1) = +0
   --    Acos (x) = NaN if abs (x) > 1
   --
   --  Ada expected values:
   --    Acos (0)  = Pi/2.0 (tightly approximated)
   --    Acos (-1) = Pi (tightly approximated)
   --    Acos (x) return a result in [0, Pi] radians
   --
   --  @llr acos (Float) Accuracy
   --  The Acos function shall return the inverse cosine of <X> with the
   --  following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 4.0 * Eps

   -----------
   -- Acosh --
   -----------

   function Acosh (X : Float) return Float
      with Export, Convention => C, External_Name => "acoshf";
   --  @llr acosh (Float) Special Values
   --  The Acosh function shall return the following special values:
   --
   --  C99 special values:
   --    Acosh (1) = +0
   --    Acosh (x) = NaN if abs X > 1
   --    Acosh (+INF) = +INF
   --
   --  @llr acosh (Float) Accuracy
   --  The Acosh function shall return the inverse hyperbolic tangent of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 8.0 * Eps

   ----------
   -- Asin --
   ----------

   function Asin (X : Float) return Float
      with Export, Convention => C, External_Name => "asinf";
   --  @llr asin (Float) Special Values
   --  The Asin function shall return the following special values:
   --
   --  C99 special values:
   --    Asin (+-0) = +-0
   --    Asin (x)   = NaN if abs (x) > 1
   --
   --  Ada expected values:
   --    Asin (1)  = Pi/2.0 (tightly approximated)
   --    Asin (-1) = -Pi/2 (tightly approximated)
   --    Asin (x) return a result in [-Pi/2, Pi/2] radians
   --
   --  @llr asin (Float) Accuracy
   --  The Asin function shall return the inverse sine of <X> with the
   --  following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 4.0 * Eps

   -----------
   -- Asinh --
   -----------

   function Asinh (X : Float) return Float
      with Export, Convention => C, External_Name => "asinhf";
   --  @llr asinh (Float) Special Values
   --  The Asinh function shall return the following special values:
   --
   --  C99 special values:
   --    Asinh (0) = 0
   --    Asinh (+INF) = +INF
   --
   --  @llr asinh (Float) Accuracy
   --  The Asinh function shall return the inverse hyperbolic sine of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 8.0 * Eps

   ----------
   -- Atan --
   ----------

   function Atan (X : Float) return Float
      with Export, Convention => C, External_Name => "atanf";
   --  @llr atan (Float) Special Values
   --  The Atan function shall return the following special values:
   --
   --  C99 special values:
   --    Atan (+-0)     = +-Pi
   --    Atan2 (+-INF)    = +-0.5 * Pi
   --
   --  C expected values:
   --    Atan (x) return a result in [-Pi/2, Pi/2]

   -----------
   -- Atan2 --
   -----------

   function Atan2 (Y : Float; X : Float) return Float
      with Export, Convention => C, External_Name => "atan2f";
   --  @llr atan2 (Float; Float) Special Values
   --  The Atan2 function shall return the following special values:
   --
   --  C99 special values:
   --    Atan2 (+-0, -0)     = +-Pi
   --    Atan2 (+-0, +0)     = +-0
   --    Atan2 (+-0, x)      = +-Pi,        if x < 0
   --    Atan2 (+-0, x)      = +-0,         if x > 0
   --    Atan2 (y, +-0)      = -0.5 * Pi,   if y < 0
   --    Atan2 (y, +-0)      =  0.5 * Pi,   if y > 0
   --    Atan2 (+-y, -INF)   = +-Pi,        if y > 0 and y is finite
   --      (tightly approximated)
   --    Atan2 (+-y, -INF)   = +-0,         if y < 0 and y is finite
   --    Atan2 (+-INF, x)    = +-0.5 * Pi,  if x is finite
   --      (tightly approximated)
   --    Atan2 (+-INF, -INF) = +-0.75 * Pi (tightly approximated)
   --    Atan2 (+-INF, +INF) = +-0.25 * Pi (tightly approximated)
   --
   --  Ada expected values:
   --    Atan2 (y, x) return a result in [-Pi, Pi]
   --
   --  @llr atan2 (Float; Float) Accuracy
   --  The Atan2 function shall return the inverse tangent of <Y> / <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 4.0 * Eps

   -----------
   -- Atanh --
   -----------

   function Atanh (X : Float) return Float
      with Export, Convention => C, External_Name => "atanhf";
   --  @llr atanh (Float) Special Values
   --  The Atanh function shall return the following special values:
   --
   --  C99 special values:
   --    Atanh (0) = 0
   --    Atanh (+-1) = +- INF
   --    Atanh (X) = NaN for abs X > 1
   --
   --  @llr atanh (Float) Accuracy
   --  The Atanh function shall return the inverse hyperbolic tangent of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 8.0 * Eps

   ---------
   -- Cos --
   ---------

   function Cos (X : Float) return Float
      with Export, Convention => C, External_Name => "cosf";
   --  @llr cos (Float) Special Values
   --  The Cos function shall return the following special values:
   --
   --  C99 special values:
   --    Cos (+-0)   = 1
   --    Cos (+-INF) = NaN
   --
   --  Ada expected values:
   --    abs (Cos (x)) <= 1
   --
   --  @llr cos (Float) Accuracy
   --  The Cos function shall return the cosine of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 2.0 * Eps

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Float) return Float
      with Export, Convention => C, External_Name => "coshf";
   --  @llr cosh (Float) Special Values
   --  The Cosh function shall return the following special values:
   --
   --  C99 special values:
   --    Cosh (+-0)   = 1
   --    Cosh (+-INF) = +INF
   --
   --  Ada expected values:
   --    abs (Cosh (x)) > 1
   --
   --  @llr cosh (Float) Accuracy
   --  The Cosh function shall return the inverse cosine of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 8.0 * Eps

   ---------
   -- Exp --
   ---------

   function Exp (X : Float) return Float
      with Export, Convention => C, External_Name => "expf";
   --  @llr exp (Float) Special Values
   --  The Exp function shall return the following special values:
   --
   --  C99 special values:
   --    Exp (+-0)  = 1
   --    Exp (-INF) = +0
   --    Exp (+INF) = +INF
   --
   --  @llr exp (Float) Accuracy
   --  The Exp function shall return the exponential of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 4.0 * Eps

   ----------
   -- Exp2 --
   ----------

   function Exp2 (X : Float) return Float
      with Export, Convention => C, External_Name => "exp2f";
   --  @llr exp2 (Float) Special Values
   --  The Exp2 function shall return the following special values:
   --
   --  C99 special values:
   --    Exp2 (+-0)  = 1
   --    Exp2 (-INF) = +0
   --    Exp2 (+INF) = +INF
   --
   --  @llr exp2 (Float) Accuracy
   --  The Exp2 function shall return the exponential of <X> in base 2
   --  with the following accuracy:
   --
   --  Accuracy requirements:
   --    MRE <= 4.0 * Eps

   ---------
   -- Log --
   ---------

   function Log (X : Float) return Float
      with Export, Convention => C, External_Name => "logf";
   --  @llr log (Float) Special Values
   --  The Log function shall return the following special values:
   --
   --  C99 special values:
   --    Log (+-0)  = -INF
   --    Log (1)    = +0
   --    Log (x)    = NaN if x<0
   --    Log (+INF) = +INF
   --
   --  @llr log (Float) Accuracy
   --  The Log function shall return the logarithm of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 4.0 * Eps

   -----------
   -- Log1p --
   -----------

   function Log1p (X : Float) return Float
      with Export, Convention => C, External_Name => "log1pf";
   --  @llr log1p (Float) Special Values:
   --  The Log1p function shall return the following special values:
   --
   --  C99 special values:
   --    Log1p (+-0)  = -INF
   --    Log1p (1)    = +0
   --    Log1p (x)    = NaN if x<0
   --    Log1p (+INF) = +INF
   --
   --  @llr log1p (Float) Accuracy
   --  The Log1p function shall return the logarithm of <X> + 1
   --  with the following accuracy:
   --
   --  Accuracy requirements:
   --    MRE <= 4.0 * Eps

   ----------
   -- Log2 --
   ----------

   function Log2 (X : Float) return Float
      with Export, Convention => C, External_Name => "log2f";
   --  @llr log2 (Float) Special Values
   --  The Log2 function shall return the following special values:
   --
   --  C99 Special values:
   --    Log2 (+-0)  = -INF
   --    Log2 (1)    = +0
   --    Log2 (x)    = NaN if x<0
   --    Log2 (+INF) = +INF
   --
   --  @llr log2 (Float) Accuracy
   --  The Log function shall return the logarithm of <X> in base 2
   --  with the following accuracy:
   --
   --  Accuracy requirements:
   --    MRE <= 4.0 * Eps

   ---------
   -- Pow --
   ---------

   function Pow (Left, Right : Float) return Float
      with Export, Convention => C, External_Name => "powf";
   --  @llr pow (Float; Float) Special Values
   --  The Pow function shall return the following special values
   --
   --  C99 Special values:
   --    Pow (+-0, y)    = +-INF, if y < 0 and y an odd integer
   --    Pow (+-0, y)    = +INF, if y < 0 and y not an odd integer
   --    Pow (+-0, y)    = +-0   if y > 0 and y an odd integer
   --    Pow (+-0, y)    = +0    if y > 0 and y not an odd integer
   --    Pow (-1, +-INF) = 1
   --    Pow (1, y)      = 1 for any y, even a NaN
   --    Pow (x, +-0)    = 1 for any x, even a NaN
   --    Pow (x, y)  = NaN, if x < 0 and both x and y finite and not integer
   --    Pow (x, -INF)   = +INF      if abs (x) < 1
   --    Pow (x, -INF)   = +0        if abs (x) > 1
   --    Pow (x, +INF)   = +0        if abs (x) < 1
   --    Pow (x, +INF)   = +INF      if abs (x) > 1
   --    Pow (-INF, y)   = -0        if y < 0 and y an odd integer
   --    Pow (-INF, y)   = +0        if y < 0 and y not an odd integer
   --    Pow (-INF, y)   = -INF      if y > 0 and y an odd integer
   --    Pow (-INF, y)   = +INF      if y > 0 and y not an odd integer
   --    Pow (+INF, y)   = +0        if y < 0
   --    Pow (+INF, y)   = +INF      if y > 0
   --
   --  @llr pow (Float; Float) Accuracy
   --  The Pow function shall return <Left> to the power of <Right>
   --  with the following accuracy:
   --
   --  Ada Accuracy requirements:
   --    MRE <= (4.0 + abs (x * Log (y)) / 32) * Eps

   ---------
   -- Sin --
   ---------

   function Sin (X : Float) return Float
      with Export, Convention => C, External_Name => "sinf";
   --  @llr sin (Float) Special Values
   --  The Sin function shall return the following special values:
   --
   --  C99 special values:
   --    Sin (+-0)   = +-0
   --    Sin (+-INF) = NaN
   --
   --  @llr sin (Float) Accuracy
   --  The Sin function shall return the sine of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 2.0 * Eps

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Float) return Float
      with Export, Convention => C, External_Name => "sinhf";
   --  @llr sinh (Float) Special Values
   --  The Sinh function shall return the following special values:
   --
   --  C99 Special values:
   --    Sinh (+-0)   = +-0
   --    Sinh (+-INF) = +-INF
   --
   --  @llr sinh (Float) Accuracy
   --  The Sinh function shall return the hyperbolic sine of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 8.0 * Eps

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Float) return Float
      with Import, Convention => Intrinsic, External_Name => "__builtin_sqrtf";
   --  @ignore
   --  The Sqrt function shall return the following special values:
   --
   --  C99 special values:
   --    Sqrt (+-0) = +-0
   --    Sqrt (INF) = INF
   --    Sqrt (X)   = NaN, for X < 0.0

   ---------
   -- Tan --
   ---------

   function Tan (X : Float) return Float
      with Export, Convention => C, External_Name => "tanf";
   --  @llr tan (Float) Special Values
   --  The Tan function shall return the following special values:
   --
   --  C99 special values:
   --    Tan (+-0)   = +0
   --    Tan (+-INF) = NaN
   --
   --  @llr tan (Float) Accuracy
   --  The Tan function shall return the tangent of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 4.0 * Eps

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Float) return Float
      with Export, Convention => C, External_Name => "tanhf";
   --  @llr tanh (Float) Special Values
   --  The Tanh function shall return the following special values:
   --
   --  C99 special values:
   --    Tanh (+-0) = +-0
   --    Tanh (+-INF) = +-1
   --
   --  @llr tanh (Float) Accuracy
   --  The Tanh function shall return the hyperbolic tangent of <X>
   --  with the following accuracy:
   --
   --  Ada accuracy requirements:
   --    MRE <= 8.0 * Eps

private
   function Identity (X : Float) return Float is (X);

   function Infinity return Float is (1.0 / Identity (0.0));

   function NaN return Float is (Infinity - Infinity);

   function Exact (X : Long_Float) return Float is (Float (X));

   function Epsilon return Float is (Float'Model_Epsilon);

   function Maximum_Relative_Error (X : Float) return Float is (0.0 * X);

end Libm_Single;
