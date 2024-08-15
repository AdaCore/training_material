===============
Modular Types
===============

------------------------------------------
Bit Pattern Values and Range Constraints
------------------------------------------

* Binary based assignments possible
* No :ada:`Constraint_Error` when in range
* **Even if** they would be ``<= 0`` as a **signed** integer type

.. code:: Ada

   procedure Demo is
     type Byte is mod 256;  -- 0 .. 255
     B : Byte;
   begin
     B := 2#1000_0000#; -- not a negative value
   end Demo;

---------------------------------
Modular Range Must Be Respected
---------------------------------

.. code:: Ada

   procedure P_Unsigned is
     type Byte is mod 2**8;  -- 0 .. 255
     B : Byte;
     type Signed_Byte is range -128 .. 127;
     SB : Signed_Byte;
   begin
     ...
     B := -256;       -- compile error
     SB := -1;
     B := Byte (SB);  -- run-time error
     ...
   end P_Unsigned;

--------------------------------------
Safely Converting Signed to Unsigned
--------------------------------------

* Conversion may raise :ada:`Constraint_Error`
* Use :ada:`T'Mod` to return :ada:`argument mod T'Modulus`

   - :ada:`Universal_Integer` argument
   - So **any** integer type allowed

  .. code:: Ada

     procedure Test is
       type Byte is mod 2**8;  -- 0 .. 255
       B : Byte;
       type Signed_Byte is range -128 .. 127;
       SB : Signed_Byte;
     begin
       SB := -1;
       B := Byte'Mod (SB);  -- OK (255)

-----------------------
Package **Interfaces**
-----------------------

* **Standard** package
* Integer types with **defined bit length**

   .. code:: Ada

      type My_Base_Integer is new Integer;
      pragma Assert (My_Base_Integer'First = -2**31);
      pragma Assert (My_Base_Integer'Last = 2**31-1);

    - Dealing with hardware registers

* Note: Shorter may not be faster for integer maths

    - Modern 64-bit machines are not efficient at 8-bit maths

.. code:: Ada

   type Integer_8 is range -2**7 .. 2**7-1;
   for Integer_8'Size use 8;
   -- and so on for 16, 32, 64 bit types...

------------------------
Shift/Rotate Functions
------------------------

* In :ada:`Interfaces` package

   - :ada:`Shift_Left`
   - :ada:`Shift_Right`
   - :ada:`Shift_Right_Arithmetic`
   - :ada:`Rotate_Left`
   - etc.

* See RM B.2 - *The Package Interfaces*

---------------------------------
Bit-Oriented Operations Example
---------------------------------

* Assuming :ada:`Unsigned_16` is used

    - 16-bits modular

.. code:: Ada

   with Interfaces;
   use Interfaces;
   ...
   procedure Swap (X : in out Unsigned_16) is
   begin
     X := (Shift_Left (X,8) and 16#FF00#) or
          (Shift_Right (X,8) and 16#00FF#);
   end Swap;

---------------------------------
Why No Implicit Shift and Rotate?
---------------------------------

* Arithmetic, logical operators available **implicitly**
* **Why not** :ada:`Shift`, :ada:`Rotate`, etc. ?
* By **excluding** other solutions

   - As functions in **standard** |rightarrow| May **hide** user-defined declarations
   - As new **operators** |rightarrow| New operators for a **single type**
   - As **reserved words** |rightarrow| Not **upward compatible**

-------------------------------------
Shift/Rotate for User-Defined Types
-------------------------------------

* **Must** be modular types
* Approach 1: use :ada:`Interfaces`'s types

    - :ada:`Unsigned_8`, :ada:`Unsigned_16` ...

* Approach 2: derive from :ada:`Interfaces`'s types

   - Operations are **inherited**
   - More on that later

   .. code:: Ada

      type Byte is new Interfaces.Unsigned_8;

* Approach 3: use GNAT's intrinsic

   - Conditions on function name and type representation
   - See GNAT UG 8.11

   .. code:: Ada

      function Shift_Left
        (Value  : T;
        Amount : Natural) return T with Import,
                                        Convention => Intrinsic;

------
Quiz
------

.. code:: Ada

    type T is mod 256;
    V : T := 255;

Which statement(s) is(are) legal?

A. :answermono:`V := V + 1`
B. :answermono:`V := 16#ff#`
C. ``V := 256``
D. :answermono:`V := 255 + 1`

------
Quiz
------

.. code:: Ada

    with Interfaces; use Interfaces;

    type T1 is new Unsigned_8;
    V1 : T1 := 255;

    type T2 is mod 256;
    V2 : T2 := 255;

Which statement(s) is(are) legal?

A. :answermono:`V1 := Rotate_Left (V1, 1)`
B. ``V1 := Positive'First``
C. :answermono:`V2 := 1 and V2`
D. ``V2 := Rotate_Left (V2, 1)``
E. ``V2 := T2'Mod (2.0)``

