========================
Discrete Numeric Types
========================

----------------------
Signed Integer Types
----------------------

* Range of signed **whole** numbers

   - Symmetric about zero (-0 = +0)

* Syntax

   .. code:: Ada

      type <identifier> is range  <lower> .. <upper>;

* Implicit numeric operators

   .. code:: Ada

      -- 12-bit device
      type Analog_Conversions is range 0 .. 4095;
      Count : Analog_Conversions := 0;
      ...
      begin
         ...
         Count := Count + 1;
         ...
      end;

---------------------
Signed Integer Bounds
---------------------

* Must be **static**

   - Compiler selects **base type**
   - Hardware-supported integer type
   - Compilation **error** if not possible

-------------------------------
Predefined Signed Integer Types
-------------------------------

* :ada:`Integer` **>= 16 bits** wide
* Other **probably** available

   - :ada:`Long_Integer`, :ada:`Short_Integer`, etc.
   - Guaranteed ranges: :ada:`Short_Integer` ``<=`` :ada:`Integer` ``<=`` :ada:`Long_Integer`
   - Ranges are all **implementation-defined**

* Portability not guaranteed

   - But may be difficult to avoid

---------------------------------
Operators for Signed Integer Type
---------------------------------

* By increasing precedence

   :relational operator: :ada:`= | /= | < | <= | > | >=`
   :binary adding operator: :ada:`+ | -`
   :unary adding operator: :ada:`+ | -`
   :multiplying operator: :ada:`* | / | mod | rem`
   :highest precedence operator: :ada:`** | abs`

* *Note*: for exponentiation :ada:`**`

   - Result will be a signed integer
   - So power **must** be :ada:`Integer` ``>= 0``

* Division by zero |rightarrow| :ada:`Constraint_Error`

------------------------
Signed Integer Overflows
------------------------

* Finite binary representation
* Common source of bugs

.. code:: Ada

   K : Short_Integer := Short_Integer'Last;
   ...
   K := K + 1;

    2#0111_1111_1111_1111#  = (2**15)-1

   +                    1

   =======================
    2#1000_0000_0000_0000#  = -32,768

--------------------------------------
Signed Integer Overflow: Ada Vs Others
--------------------------------------

* Ada

   - :ada:`Constraint_Error` standard exception
   - Incorrect numerical analysis

* Java

   - Silently **wraps** around (as the hardware does)

* C/C++

   - **Undefined** behavior (typically silent wrap-around)

-----------------------------------
String Attributes for All Scalars
-----------------------------------

* :ada:`T'Image (input)`

   - Converts :ada:`T` |rightarrow| :ada:`String`

* :ada:`T'Value (input)`

   - Converts :ada:`String` |rightarrow| :ada:`T`

.. code:: Ada

   Number : Integer := 12345;
   Input  : String (1 .. N);
   ...
   Put_Line (Integer'Image (Number));
   ...
   Get (Input);
   Number := Integer'Value (Input);

----------------------------------
Range Attributes for All Scalars
----------------------------------

* :ada:`T'First`

  - First (**smallest**) value of type :ada:`T`

* :ada:`T'Last`

  - Last (**greatest**) value of type :ada:`T`

* :ada:`T'Range`

  - Shorthand for :ada:`T'First .. T'Last`

.. code:: Ada

   type Signed_T is range -99 .. 100;
   Smallest : Signed_T := Signed_T'First; -- -99
   Largest  : Signed_T := Signed_T'Last;  -- 100

-------------------------------------
Neighbor Attributes for All Scalars
-------------------------------------

* :ada:`T'Pred (Input)`

   - Predecessor of specified value
   - :ada:`Input` type must be :ada:`T`

* :ada:`T'Succ (Input)`

   - Successor of specified value
   - :ada:`Input` type must be :ada:`T`

.. code:: Ada

   type Signed_T is range -128 .. 127;
   type Unsigned_T is mod 256;
   Signed   : Signed_T := -1;
   Unsigned : Unsigned_T := 0;
   ...
   Signed := Signed_T'Succ (Signed); -- Signed = 0
   ...
   Unsigned := Unsigned_T'Pred (Unsigned); -- Signed = 255

------------------------------------
Min/Max Attributes for All Scalars
------------------------------------

* :ada:`T'Min (Value_A, Value_B)`

  - **Lesser** of two :ada:`T`

* :ada:`T'Max (Value_A, Value_B)`

  - **Greater** of two :ada:`T`

.. code:: Ada

   Safe_Lower : constant := 10;
   Safe_Upper : constant := 30;
   C : Integer := 15;
   ...
   C := Integer'Max (Safe_Lower, C - 1);
   ...
   C := Integer'Min (Safe_Upper, C + 1);

------
Quiz
------

What happens when you try to compile/run this code?

.. code:: Ada

   C1 : constant := 2 ** 1024;
   C2 : constant := 2 ** 1024 + 10;
   C3 : constant := C1 - C2;
   V  : Integer := C1 - C2;

A. Compile error
B. Run-time error
C. :answer:`V is assigned to -10`
D. Unknown - depends on the compiler

.. container:: animate

   Explanations

   - 2:superscript:`1024` too big for most runtimes BUT
   - :ada:`C1`, :ada:`C2`, and :ada:`C3` are named numbers, not typed constants

      - Compiler uses unbounded precision for named numbers
      - Large intermediate representation does not get stored in object code

   - For assignment to :ada:`V`, subtraction is computed by compiler

      - :ada:`V` is assigned the value -10

