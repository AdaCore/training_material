===========
Base Type
===========

-------------
Base Ranges
-------------

* Actual **hardware-supported** numeric type used
* **Predefined** operators

   - Work on full-range

        + **No range checks** on inputs or result
        + Best performance

   - Implementation may use wider registers

        + Intermediate values

* Can be accessed with :ada:`'Base` attribute

   .. code:: Ada

      type Foo is range -30_000 .. 30_000;
      function "+" (Left, Right : Foo'Base) return Foo'Base;

* Base range

    - Signed
    - 8 bits |rightarrow| :ada:`-128 .. 127`
    - 16 bits |rightarrow| :ada:`-32_768 .. 32767`

---------------------------------
Compile-Time Constraint Violation
---------------------------------

* *May* produce **warnings**

    - And compile successfuly

* *May* produce **errors**

    - And fail at compilation

* Requirements for rejection

   - Static value
   - Value not in range of **base** type
   - Compilation is **impossible**

.. code:: Ada

   procedure Test is
      type Some_Integer is range -200 .. 200;
      Object : Some_Integer;
   begin
      Object := 50_000; -- probable error
   end;

-------------------
Range Check Failure
-------------------

* Compile-time rejection

   - Depends on **base** type
   - Selected by the compiler
   - Depends on underlying **hardware**
   - Early error |rightarrow| "Best" case

* Else run-time **exception**

    - Most cases
    - Be happy when compilation failed instead


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

   procedure Unsigned is
     type Byte is mod 2**8;  -- 0 .. 255
     B : Byte;
     type Signed_Byte is range -128 .. 127;
     SB : Signed_Byte;
   begin
     ...
     B := -256;       -- compile error
     SB := -1;
     B := Byte (SB);  -- runtime error
     ...
   end Unsigned;

--------------------------------------
Safely Converting Signed To Unsigned
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

------------------------
Shift/Rotate Functions
------------------------

* In :ada:`Interfaces` package

   - `Shift_Left`
   - `Shift_Right`
   - `Shift_Right_Arithmetic`
   - `Rotate_Left`
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
   procedure Swap( X : in out Unsigned_16 ) is
   begin
     X := ( Shift_Left(X,8) and 16#FF00# ) or
          ( Shift_Right(X,8) and 16#00FF# );
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

    - `Unsigned_8`, `Unsigned_16` ...

* Approach 2: derive from :ada:`Interfaces`'s types

   - Operations are **inherited**
   - More on that later

   .. code:: Ada

      type Byte is new Interfaces.Unsigned_8;
      type Half_Word is new Interfaces.Unsigned_16;
      type Word is new Interfaces.Unsigned_32;

