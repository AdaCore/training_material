====================
Very Large Numbers
====================

------------------------------------------
Why Float or Integer Just Doesn’t Cut It
------------------------------------------

- Standard types (:ada:`Integer`, :ada:`Float`) have limited precision
- Overflow, underflow, and rounding errors are common
- Some applications need *arbitrary precision*

-------------------------------
Meet Ada.Numerics.Big_Numbers
-------------------------------

- Introduced in Ada 2022
- Provides arbitrary-precision arithmetic:
  
  - type :ada:`Big_Integer` in package :ada:`Ada.Numerics.Big_Numbers.Big_Integers`
  - type :ada:`Big_Real` in package :ada:`Ada.Numerics.Big_Numbers.Big_Reals`

- Ideal for high-accuracy applications

   - Create giant numbers or highly precise real values
   - Standard arithmetic operations
   - Control precision and rounding

-----------------------
Big_Integer in Action
-----------------------

.. code:: ada

    with Ada.Numerics.Big_Numbers.Big_Integers;
    use Ada.Numerics.Big_Numbers.Big_Integers;

    procedure Test_Big_Integer is
       X : Big_Integer := To_Big_Integer (2) ** 100;
       Y : Big_Integer := X * 123456789;
    begin
       Put_Line ("Result: " & Y'Image);
    end Test_Big_Integer;     


- Handles numbers with hundreds or thousands of digits

--------------------------
Big_Real: Real Precision
--------------------------

.. container:: latex_environment small

   .. code:: ada

       with Ada.Numerics.Big_Numbers.Big_Reals;
       use Ada.Numerics.Big_Numbers.Big_Reals;

       procedure Test_Big_Real is
          A : Big_Real := 3.141592653589793238462643383279;
          B : Big_Real := A ** 10;
       begin
          Put_Line ("Pi ^ 10 = " & B'Image);
       end Test_Big_Real;

- Arbitrary-precision floating-point numbers

------------------------
Who Needs Big_Numbers?
------------------------

- Scientific computing
- Finance and accounting
- Exact algorithms (symbolic math, computer algebra)
- Cryptography

.. note:: 

   Note that you typically wouldn't use :ada:`Big_Numbers` for cryptography because it's vulnerable to timing side-channels attacks.

-------------------------
Issues with Big Numbers
-------------------------

- Slower than native types
- Be selective: use when precision is critical
- Not all math functions implemented (e.g., trig/log)
- Watch memory usage for huge values
