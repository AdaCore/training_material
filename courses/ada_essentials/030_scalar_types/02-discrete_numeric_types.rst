========================
Discrete Numeric Types
========================

----------------------
Signed Integer Types
----------------------

**Syntax**

.. container:: source_include 030_scalar_types/syntax.bnf :start-after:signed_integer_types_begin :end-before:signed_integer_types_end :code:bnf

* Range of signed **whole** numbers

   - Symmetric about zero (-0 = +0)

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

-----------------------
Signed Integer Bounds
-----------------------

* Bounds must be known at compile time

  *Later we'll learn about scoping and how to define dynamic bounds*

* Compiler selects **base type**

  - Hardware-supported integer type

    .. code:: Ada

      type My_Integer_T is range 0 .. 1000;

  * Compiler could select a 16-, 32-, or 64-bit representation

    * But not 8-bit, because it won't fit

  * Compilation **error** if not possible

    .. code:: ada
      :number-lines: 3

      type Big_Integer_T is range 0 .. 2**128;

    .. container:: latex_environment scriptsize

      :error:`main.adb:3:26: error: integer type definition bounds out of range`

-------------------------------
Predefined Signed Integer Types
-------------------------------

* :ada:`Integer` **>= 16 bits** wide
* Other **probably** available

   - :ada:`Long_Integer`, :ada:`Short_Integer`, etc.
   - Guaranteed ranges: :ada:`Short_Integer` ``<=`` :ada:`Integer` ``<=`` :ada:`Long_Integer`
   - Ranges are all **implementation-defined**

.. warning::

    Portability not guaranteed
        
        * But usage may be difficult to avoid

---------------------------------
Operators for Signed Integer Type
---------------------------------

* By increasing precedence

   :relational operator: **=** :nbsp:` ` **/=** :nbsp:` `   **<** :nbsp:` `   **<=** :nbsp:` `   **>** :nbsp:` `   **>=**
   :binary adding operator: **+** :nbsp:` `   **-**
   :unary adding operator: **+** :nbsp:` `   **-**
   :multiplying operator: **\*** :nbsp:` `   **/** :nbsp:` `   **mod** :nbsp:` `   **rem**
   :highest precedence operator: **\*\*** :nbsp:` `   **abs** 

.. note::

    Exponentiation (:ada:`**`) result will be a signed integer

       - Power **must** be :ada:`Integer` ``>= 0``


------------------------
Signed Integer Overflows
------------------------

* Finite binary representation
* Common source of bugs

::

   K : Short_Integer := 16#7FFF#;
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
C. :answer:`V is assigned the value -10`
D. Unknown - depends on the compiler

.. container:: animate

   Explanations

   - 2:superscript:`1024` too big for most runtimes BUT
   - :ada:`C1`, :ada:`C2`, and :ada:`C3` are named numbers, not typed constants

      - Compiler uses unbounded precision for named numbers
      - Large intermediate representation does not get stored in object code

   - For assignment to :ada:`V`, subtraction is computed by compiler

      - :ada:`V` is assigned the value -10

