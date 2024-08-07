====================
Enumeration Types
====================

-------------------
Enumeration Types
-------------------

* Enumeration of **logical** values

    - Integer value is an implementation detail

* Syntax

   .. code:: Ada

      type <identifier> is (<identifier-list>) ;

* Literals

   - Distinct, ordered
   - Can be in **multiple** enumerations

   .. code:: Ada

      type Colors is (Red, Orange, Yellow, Green, Blue, Violet);
      type Stop_Light is (Red, Yellow, Green);
      ...
      -- Red both a member of Colors and Stop_Light
      Shade : Colors := Red;
      Light : Stop_Light := Red;

-----------------------------
Enumeration Type Operations
-----------------------------

* Assignment, relationals
* **Not** numeric quantities

   - *Possible* with attributes
   - Not recommended

.. code:: Ada

   type Directions is (North, South, East, West);
   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   Heading : Directions;
   Today, Tomorrow : Days;
   ...
   Today := Mon;
   Today := North; -- compile error
   Heading := South;
   Heading := East + 1; -- compile error
   if Today < Tomorrow then ...

---------------
Character Types
---------------

* Literals

   - Enclosed in single quotes eg. :ada:`'A'`
   - Case-sensitive

* **Special-case** of enumerated type

   - At least one character enumeral

* System-defined :ada:`Character`
* Can be user-defined

   .. code:: Ada

      type EBCDIC is (nul, ..., 'a' , ..., 'A', ..., del);
      Control : EBCDIC := 'A';
      Nullo : EBCDIC := nul;

-------------------------------
Language-Defined Type Boolean
-------------------------------

* Enumeration

   .. code:: Ada

      type Boolean is (False, True);

* Supports assignment, relational operators, attributes

   .. code:: Ada

      A : Boolean;
      Counter : Integer;
      ...
      A := (Counter = 22);

* Logical operators :ada:`and`, :ada:`or`, :ada:`xor`, :ada:`not`

   .. code:: Ada

      A := B or (not C); -- For A, B, C boolean

------------------------------------
Why Boolean Isn't Just an Integer?
------------------------------------

.. container:: columns

 .. container:: column

    * Example: Real-life error

       - HETE-2 satellite **attitude control** system software (ACS)
       - Written in **C**

    * Controls four "solar paddles"

        - Deployed after launch

 .. container:: column

    .. image:: hete-2_satellite.jpeg

------------------------------------
Why Boolean Isn't Just an Integer!
------------------------------------

* **Initially** variable with paddles' state

    - Either **all** deployed, or **none** deployed

* Used :C:`int` as a boolean

   .. code:: C

      if (rom->paddles_deployed == 1)
        use_deployed_inertia_matrix();
      else
        use_stowed_inertia_matrix();

* Later :C:`paddles_deployed` became a **4-bits** value

    - One bit per paddle
    - :C:`0` |rightarrow| none deployed, :C:`0xF` |rightarrow| all deployed

* Then, :C:`use_deployed_inertia_matrix()` if only first paddle is deployed!
* Better: boolean function :C:`paddles_deployed()`

    - Single line to modify

---------------------------------------
Boolean Operators' Operand Evaluation
---------------------------------------

* Evaluation order **not specified**
* May be needed

  - Checking value **before** operation
  - Dereferencing null pointers
  - Division by zero

 .. code:: Ada

    if Divisor /= 0 and K / Divisor = Max then ... -- Problem!

-----------------------------
Short-Circuit Control Forms
-----------------------------

* **Short-circuit** |rightarrow| **fixed** evaluation order
* Left-to-right
* Right only evaluated **if necessary**

   - :ada:`and then`: if left is :ada:`False`, skip right

     .. code:: Ada

        Divisor /= 0 and then K / Divisor = Max

   - :ada:`or else`: if left is :ada:`True`, skip right

     .. code:: Ada

        Divisor = 0 or else K / Divisor = Max

------
Quiz
------

.. code:: Ada

   type Enum_T is (Able, Baker, Charlie);

Which statement will generate an error?

A. ``V1 :  Enum_T := Enum_T'Value ("Able");``
B. ``V2 :  Enum_T := Enum_T'Value ("BAKER");``
C. ``V3 :  Enum_T := Enum_T'Value (" charlie ");``
D. :answermono:`V4 : Enum_T := Enum_T'Value ("Able Baker Charlie");`

.. container:: animate

   Explanations

   A. Legal
   B. Legal - conversion is case-insensitive
   C. Legal - leading/trailing blanks are ignored
   D. :ada:`Value` tries to convert entire string, which will fail at run-time

