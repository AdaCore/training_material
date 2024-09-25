=========================
Subtypes - Full Picture
=========================

----------------
Implicit Subtype
----------------

* The declaration

   .. code:: Ada

      type Typ is range L .. R;

* Is short-hand for

   .. code:: Ada

      type <Anon> is new Predefined_Integer_Type;
      subtype Typ is <Anon> range L .. R;

* :ada:`<Anon>` is the :dfn:`Base` type of :ada:`Typ`

    - Accessed with :ada:`Typ'Base`

----------------------------
Implicit Subtype Explanation
----------------------------

.. code:: Ada

   type <Anon> is new Predefined_Integer_Type;
   subtype Typ is <Anon> range L .. R;

* Compiler choses a standard integer type that includes :ada:`L .. R`

   - :ada:`Integer`, :ada:`Short_Integer`, :ada:`Long_Integer`, etc.
   - **Implementation-defined** choice, non portable

* New anonymous type ``<Anon>`` is derived from the predefined type
* ``<Anon>`` inherits the type's operations (``+``, ``-`` ...)
* ``Typ``, subtype of ``<Anon>`` is created with :ada:`range L .. R`
* :ada:`Typ'Base` will return the type ``<Anon>``

-----------------------------
Stand-Alone (Sub)Type Names
-----------------------------

* Denote all the values of the type or subtype

   - Unless explicitly constrained

   .. code:: Ada

      subtype Constrained_Sub is Integer range 0 .. 10;
      subtype Just_A_Rename is Integer;
      X : Just_A_Rename;
      ...
      for I in Constrained_Sub loop
        X := I;
      end loop;

--------------------------------
Subtypes Localize Dependencies
--------------------------------

* Single points of change
* Relationships captured in code
* No subtypes

.. code:: Ada

   type Vector is array (1 .. 12) of Some_Type;

   K : Integer range 0 .. 12 := 0; -- anonymous subtype
   Values : Vector;
   ...
   if K in 1 .. 12 then ...
   for J in Integer range 1 .. 12 loop ...

* Subtypes

.. code:: Ada

   type Counter is range 0 .. 12;
   subtype Index is Counter range 1 .. Counter'Last;
   type Vector is array (Index) of Some_Type;

   K : Counter := 0;
   Values : Vector;
   ...
   if K in Index then ...
   for J in Index loop ...

----------------------------------
Subtypes May Enhance Performance
----------------------------------

* Provides compiler with more information
* Redundant checks can more easily be identified

.. code:: Ada

   subtype Index is Integer range 1 .. Max;
   type Vector is array (Index) of Float;
   K : Index;
   Values : Vector;
   ...
   K := Some_Value;   -- range checked here
   Values (K) := 0.0; -- so no range check needed here

---------------------------------
Subtypes Don't Cause Overloading
---------------------------------

- Illegal code: re-declaration of `F`

   .. code:: Ada

      type A is new Integer;
      subtype B is A;
      function F return A is (0);
      function F return B is (1);

-------------------------------
Default Values and Option Types
-------------------------------

* Not allowed: Defaults on new :ada:`type` only

    - :ada:`subtype` is still the same type

* **Note:** Default value may violate subtype constraints

   - Compiler error for static definition
   - :ada:`Constraint_Error` otherwise

.. code:: Ada

   type Tertiary_Switch is (Off, On, Neither)
      with Default_Value => Neither;
   subtype Toggle_Switch is Tertiary_Switch
       range Off .. On;
   Safe : Toggle_Switch := Off;
   Implicit : Toggle_Switch; -- compile error: out of range

.. tip

   Using a meaningless value (:ada:`Neither`) to extend
   the range of the type is turning it into an :dfn:`option type`.
   This idiom is very rich and allows for e.g. "in-flow" errors handling.

..
  language_version 2012

----------------------------------------
Attributes Reflect the Underlying Type
----------------------------------------

.. code:: Ada

   type Color is
       (White, Red, Yellow, Green, Blue, Brown, Black);
   subtype Rainbow is Color range Red .. Blue;

* :ada:`T'First` and :ada:`T'Last` respect constraints

   - :ada:`Rainbow'First` |rightarrow| Red *but* :ada:`Color'First` |rightarrow| White
   - :ada:`Rainbow'Last` |rightarrow| Blue *but* :ada:`Color'Last` |rightarrow| Black

* Other attributes reflect base type

   - :ada:`Color'Succ (Blue)` = Brown = :ada:`Rainbow'Succ (Blue)`
   - :ada:`Color'Pos (Blue)` = 4 = :ada:`Rainbow'Pos (Blue)`
   - :ada:`Color'Val (0)` = White = :ada:`Rainbow'Val (0)`

* Assignment must still satisfy target constraints

   .. code:: Ada

      Shade : Color range Red .. Blue := Brown; -- run-time error
      Hue : Rainbow := Rainbow'Succ (Blue);     -- run-time error

------------------------
Idiom: Extended Ranges
------------------------

* ``Count`` / ``Positive_Count``

   - Sometimes as ``Type_Ext`` (extended) / ``Type``
   - For counting vs indexing
    
      + An index goes from 1 to max length
      + A count goes from 0 to max length

   .. code:: Ada
   
      -- ARM A.10.1
      package Text_IO is
         ...
         type Count is range 0 .. implementation-defined;
         subtype Pos_Count is Count range 1 .. Count'Last;

------------------
Idiom: Partition
------------------

* Useful for splitting-up large enums

.. warning::

   Be careful about checking that the partition is complete when
   items are added/removed.
   Using at least one :ada:`case` will check that for you.

.. tip::

   Can have non-consecutive values with the :ada:`Predicate` aspect.

.. code:: Ada

   type Commands_T is (Lights_On, Lights_Off, Read, Write, Accelerate, Stop);
   --  Complete partition of the commands
   subtype IO_Commands_T is Commands_T range Read .. Write;
   subtype Lights_Commands_T is Commands_T range Lights_On .. Lights_Off;
   subtype Movement_Commands_T is Commands_T range Accelerate .. Stop;

   subtype Physical_Commands_T is Commands_T
      with Predicate => Physical_Commands_T in Lights_Commands_T | Movement_Commands_T; 

   procedure Execute_Light_Command (C : Lights_Commands_T);

   procedure Execute_Command (C : Commands_T) is
   begin
      case C in --  partition must be exhaustive
         when Lights_Commands_T => Execute_Light_Command (C);
   ...

--------------------------------------
Idiom: Subtypes as Local Constraints
--------------------------------------

* Can replace defensive code
* Can be very useful in some identified cases
* Subtypes accept dynamic bounds, unlike types
* Checks happen through type-system

    - Can be disabled with :command:`-gnatp`, unlike conditionals
    - Can also be a disadvantage

.. warning::

   Do not use for checks that should **always** happen, even in production.

.. code:: Ada

   subtype Incrementable_Integer is Integer
      range Integer'First .. Integer'Last - 1;

   function Increment (I : Incrementable_Integer) return Integer;

.. code:: Ada

   subtype Valid_Fingers_T is Integer
      range 1 .. 5;
   Fingers : Valid_Fingers_T
      := Prompt_And_Get_Integer ("Give me the number of a finger");

.. code:: Ada

   procedure Read_Index_And_Manipulate_Char (S : String) is
      subtype S_Index is Positive range S'Range;
      I : constant S_Index := Read_Positive;
      C : Character renames S (I);

------
Quiz
------

.. code:: Ada
    :number-lines: 1

    type T1 is range 0 .. 10;
    function "-" (V : T1) return T1;
    subtype T2 is T1 range 1 .. 9;
    function "-" (V : T2) return T2;

    Obj : T2 := -T2 (1);

Which function is executed at line 6?

A. The one at line 2
B. The one at line 4
C. A predefined ``"-"`` operator for integer types
D. :answer:`None: The code is illegal`

.. container:: animate

    The :ada:`type` is used for the overload profile, and here both :ada:`T1` and :ada:`T2`
    are of type :ada:`T1`, which means line 4 is actually a redeclaration, which is forbidden.

------
Quiz
------

.. code:: Ada

   type T is range 0 .. 10;
   subtype S is T range 1 .. 9;

What is the value of :ada:`S'Succ (S (9))`?

A. 9
B. :answer:`10`
C. None, this fails at run-time
D. None, this does not compile

.. container:: animate

    :ada:`T'Succ` and :ada:`T'Pred` are defined on the :ada:`type`, not the :ada:`subtype`.

------
Quiz
------

.. code:: Ada

    type T is new Integer range 0 .. Integer'Last;
    subtype S is T range 0 .. 10;

    Obj : S;

What is the result of :ada:`Obj := S'Last + 1`?

A. 0
B. 11
C. :answer:`None, this fails at run-time`
D. None, this does not compile

