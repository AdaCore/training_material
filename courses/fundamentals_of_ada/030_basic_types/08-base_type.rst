===========
Base Type
===========

-------------
Base Ranges
-------------

* Actual **hardware-supported** numeric type used

   - GNAT makes consistent and predictable choices on all major platforms

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

-----------------------------
Real Base Decimal Precision
-----------------------------

* Real types precision may be **better** than requested
* Example:

   - Available: 6, 12, or 24 digits of precision
   - Type with **8 digits** of precision

      .. code:: Ada

         type My_Type is digits 8;

   - :ada:`My_Type` will have 12 **or** 24 digits of precision

---------------------------------
Floating Point Division by Zero
---------------------------------

* Language-defined do as the machine does

   - If :ada:`T'Machine_Overflows` attribute is :ada:`True` raises :ada:`Constraint_Error`
   - Else :math:`+\infty` / :math:`-\infty`

      + Better performance

* User-defined types always raise :ada:`Constraint_Error`

 .. code:: Ada

    subtype MyFloat is Float range Float'First .. Float'Last;
    type MyFloat is new Float range Float'First .. Float'Last;

-----------------------------------------
Using Equality for Floating Point Types
-----------------------------------------

* Questionable: representation issue

   - Equality |rightarrow| identical bits
   - Approximations |rightarrow| hard to **analyze**, and **not portable**
   - Related to floating-point, not Ada

* Perhaps define your own function

   - Comparison within tolerance (:math:`+\varepsilon` / :math:`-\varepsilon`)

