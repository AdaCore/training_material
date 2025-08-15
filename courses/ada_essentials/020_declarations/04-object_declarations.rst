=====================
Object Declarations
=====================

---------------------
Object Declarations
---------------------

* An :ada:`object` is either :dfn:`variable` or :dfn:`constant`
* Basic Syntax

   .. code:: Ada

      <name> : <typemark> [:= <initial_value>];
      <name> : constant <type> := <value>;

* Constant should have a value

   - Except for privacy (seen later)

* Examples

   .. code:: Ada

      An_Object : Some_Type;
      Max : constant Some_Type := 200;
      -- variable with a constraint
      Count : Some_Type range 0 .. Max := 0;
      -- dynamic initial value via function call
      Some_Object : Some_Type := Some_Function (Count);

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
        First_One : Some_Type := 10;
        Next_One : Some_Type := First_One;
        Another_One : Some_Type := Next_One;
      begin
        ...

------------------------------
Multiple Object Declarations
------------------------------

* Allowed for convenience

   .. code:: Ada

      Val_1, Val_2 : Some_Type := Next_Available (Some_Num);

* Identical to series of single declarations

   .. code:: Ada

      Val_1 : Some_Type := Next_Available (Some_Num);
      Val_2 : Some_Type := Next_Available (Some_Num);

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
* Also, implementation-specific extensions

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

      * **Numerics** - standard math operators
      * **Arrays** - concatenation operator
      * **Most types** - assignment operator

