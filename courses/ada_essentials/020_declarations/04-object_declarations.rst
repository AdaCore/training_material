=====================
Object Declarations
=====================

---------------------
Object Declarations
---------------------

**Syntax**

.. container:: source_include 020_declarations/syntax.bnf :start-after:object_declarations_begin :end-before:object_declarations_end :code:bnf

* An :ada:`object` is either :dfn:`variable` or :dfn:`constant`

   * where

     * **<identifier>** is the defining name for the object
     * **<typemark>** is the name describing the type of the object

* Simple objects

  .. code:: Ada

     An_Object : Some_Type;
     Max       : constant Some_Type := 200;
     Count     : Some_Type range 0 .. Max;

----------------
Initialization
----------------

* Constants **must** be initialized where defined

  .. code:: Ada

    Max_Count : constant Integer := 1_000;

  * *Special case: deferred constants (discussed later)*

* Variables **can** be initialized where defined

  .. code:: Ada

    First_Item   : Integer := 0;
    Out_Of_Range : Integer := First_Item - 1;
    Last_Item    : Integer := Max_Count;
    Next_Item    : Integer := Get_Next (First_Item);

* Evaluation order is guaranteed linear

.. warning::

  Runtime does not initialize variables

*Special cases (to be discussed later)*

  * *Access (pointer) types*
  * *Default value aspects*

-------------
Elaboration
-------------

* The act of creating/initializing an object is :dfn:`elaboration`

* **Compiler** will

  * Initialize static constants
  * Determine size of objects

* **Runtime** will

  * Create object on stack or heap
  * Initialize object when specified

    * Either by language or programmer

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

