=====================
Object Declarations
=====================

---------------------
Object Declarations
---------------------

* An :ada:`object` is either :dfn:`variable` or :dfn:`constant`
* Basic Syntax

   .. code:: Ada

      <identifier> : <subtype> [:= <initial value>];
      <identifier> : constant <subtype> := <value>;

* Constant should have a value

   - Except for privacy (seen later)

* Examples

   .. code:: Ada

      Z, Phase : Analog;
      Max : constant Integer := 200;
      -- variable with a constraint
      Count : Integer range 0 .. Max := 0;
      -- dynamic initial value via function call
      Root : Tree := F(X);

------------------------------
Multiple Object Declarations
------------------------------

* Allowed for convenience

   .. code:: Ada

      A, B : Integer := Next_Available (X);

* Identical to series of single declarations

   .. code:: Ada

      A : Integer := Next_Available (X);
      B : Integer := Next_Available (X);

.. warning:: May get different value!

   .. code:: Ada

      T1, T2 : Time := Current_Time;

-------------------------
Predefined Declarations
-------------------------

* **Implicit** declarations
* Language standard
* Annex A for *Core*

   - Package :code:`Standard`
   - Standard types and operators

        + Numerical
        + Characters

   - About **half the RM** in size

* "Specialized Needs Annexes" for *optional*
* Also, implementation specific extensions

------------------------------------
Implicit Vs Explicit Declarations
------------------------------------

* :dfn:`Explicit` |rightarrow| in the source

   .. code:: Ada

      type Counter is range 0 .. 1000;

* :dfn:`Implicit` |rightarrow| **automatically** by the compiler

   .. code:: Ada

      function "+" (Left, Right : Counter) return Counter;
      function "-" (Left, Right : Counter) return Counter;
      function "*" (Left, Right : Counter) return Counter;
      function "/" (Left, Right : Counter) return Counter;
      ...

   * Compiler creates appropriate operators based on the underlying type

      * Numeric types get standard math operators
      * Array types get concatenation operator
      * Most types get assignment operator

-----------
Elaboration
-----------

* :dfn:`Elaboration` has several facets:

  * **Initial value** calculation

    - Evaluation of the expression
    - Done at **run-time** (unless static)

  * Object creation

    - Memory **allocation**
    - Initial value assignment (and type checks)

* Runs in linear order

   - Follows the program text
   - Top to bottom

   .. code:: Ada

      declare
        First_One : Integer := 10;
        Next_One : Integer := First_One;
        Another_One : Integer := Next_One;
      begin
        ...

------
Quiz
------

Which block(s) is (are) legal?

   A. :answermono:`A, B, C : Integer;`
   B. :answermono:`Integer : Standard.Integer;`
   C. ``Null : Integer := 0;``
   D. | :answermono:`A : Integer := 123;`
      | :answermono:`B : Integer := A * 3;`

.. container:: animate

   Explanations

   A. Multiple objects can be created in one statement
   B. :ada:`Integer` is *predefined* so it can be overridden
   C. :ada:`null` is *reserved* so it can **not** be overridden
   D. Elaboration happens in order, so :ada:`B` will be 369

