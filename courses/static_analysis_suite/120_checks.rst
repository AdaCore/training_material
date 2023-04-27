*****************
CodePeer Checks
*****************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

=================
Types of Checks
=================

---------------------
Messages Categories
---------------------

+ **Run-Time Checks**

  + Errors that will raise built-in exceptions at runtime
  + Or fail silently with :command:`-gnatp`

+ **User Checks**

  + Errors that will raise user exceptions at runtime
  + Or fail silently with :command:`-gnatp`

+ **Validity Checks**

  + Mishandled object scope and value

+ **Warnings**

  + Questionable code that seems to have logic flaws
  + Hints at logical errors

+ **Race Conditions**

  + Code unsafe due to multi-tasking

=================
Run-Time Checks
=================

-------------------------
Run-Time Check Messages
-------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Definition*

        * - ``divide by zero``

          - The second operand could be zero

        * -

          - On a division, :ada:`mod` or :ada:`rem` operation

        * - ``range check``

          - A discrete could reach a value out of its :ada:`range`

        * - ``overflow check``

          - An operation could overflow its numeric type

        * -

          - Note: Depends on the :ada:`'Base` representation

        * - ``array index check``

          - Array index could be out of bounds

        * - ``access check``

          - A :ada:`null` access could be dereferenced

        * - ``aliasing check``

          - A subprogram call could cause an aliasing error

        * -

          - eg. passing a single reference as two parameters

        * - ``tag check``

          - A dynamic :ada:`'Class` or :ada:`'Tag` check could fail

        * - ``validity``

          - An uninitialized or invalid object could be read

        * - ``discriminant check``

          - The wrong variant could be used

        * -

          - eg. copy with the wrong discriminant

        * - ``precondition``

          - A subprogram call could violate its deduced precondition

-----------------
Divide By Zero
-----------------

+ The second operand of a divide, :ada:`mod` or :ada:`rem` operation could be zero
+ Runtime :ada:`Constraint_Error`

.. code:: Ada
   :number-lines: 3

   procedure Div is
      type Int is range 0 .. 2**32 - 1;
      A : Int := Int'Last;
      X : Integer;
   begin
      for I in Int range 0 .. 2
      loop
         X := Integer (A / I);
      end loop;
   end Div;

| ``example.adb:10:23: high: divide by zero fails here: requires I /= 0``

-------------
Range Check
-------------

+ Calculation may generate a value outside the :ada:`range` of an Ada type or subtype
+ Will generate a :ada:`Constraint_Error`

.. code:: Ada
   :number-lines: 1

   procedure Example is

      subtype Constrained_Integer is Integer range 1 .. 2;
      A : Integer;

      procedure Proc_1 (I : in Constrained_Integer) is
      begin
         A := I + 1;
      end Proc_1;

   begin
      A := 0;
      Proc_1 (I => A);

| ``example.adb:13:17: high: range check fails here: requires A in 1..2``

----------------
Overflow Check
----------------

+ Calculation may overflow the bounds of a numeric type.
+ Depends on the size of the underlying (base) type
+ Will generate a :ada:`Constraint_Error`

.. code:: Ada
   :number-lines: 8

   procedure Example is
      Attempt_Count : Integer := Integer'Last;
   begin
      loop
         Put ("Enter password to delete system disk");
         if Get_Correct_Pw
         then
            Allow_Access;
         else
            Attempt_Count := Attempt_Count + 1;


| ``example.adb:17:44: high: overflow check fails here: requires Attempt_Count /= Integer_32'Last``

-------------------
Array Index Check
-------------------

+ Index value could be outside the array bounds
+ Also known as **buffer overflow**.
+ Will generate a :ada:`Constraint_Error`

.. code:: Ada
   :number-lines: 1

   procedure Example is
      type Int_Array is array (0 .. 2) of Integer;
      X, Y : Int_Array;
   begin
      for I in X'Range
      loop
         X (I) := I + 1;
      end loop;

      for I in X'Range
      loop
         Y (X (I)) := I;
      end loop;
   end Example;

| ``example.adb:12:7: high: array index check fails here: requires (X (I)) in 0..2``

--------------
Access Check
--------------

+ Attempting to dereference a reference that could be :ada:`null`
+ Will generate an :ada:`Access_Error`

.. code:: Ada
   :number-lines: 1

   procedure Example is
      type Int_Access is access Integer;
      X : Int_Access;
   begin
      if X = null
      then
         X.all := 1;
      end if;
   end Example;

| ``example.adb:7:7: high: access check fails here``

----------------
Aliasing Check
----------------

+ Some parameters could be passed as **reference**
+ Deduced preconditions:

  + Do not **reference** another parameter
  + Do not **match** the address of a global object

.. code:: Ada
   :number-lines: 1

   procedure Example is
      X : String := "Hello, World";
      procedure In_Out
        (A :     String;
         B : out String) is
      begin
         B (B'First) := A (A'First);
         if A'Length > 1
         then
            B (B'First) := A (A'Last);
         end if;
      end In_Out;
   begin
      In_Out (X, X);
   end Example;

| ``example.adb:14:4: high: precondition (aliasing check) failure on call to example.in_out: requires B /= A``

-----------
Tag Check
-----------

A tag check operation on a :ada:`tagged` object might raise a :ada:`Constraint_Error`

.. code:: Ada
   :number-lines: 5

   type T2 is new T1 with null record;

   procedure One (X1 : T1'Class) is
   begin
      An_Operation (T2'Class (X1));
   end One;

   procedure Two is
      X1 : T1;
      X2 : T2;
   begin
      One (X1);
   end Two;

| ``example.adb:16:7: high: precondition (tag check) failure on call to example.one: requires X1'Tag = example.t2``

----------
Validity
----------

.. code:: Ada
   :number-lines: 1

   procedure Example is
      A : Integer := 123;
      B : Integer;
   begin
      A := B;
   end Example;

| ``example.adb:5:9: high: validity check: B is uninitialized here``

--------------------
Discriminant Check
--------------------

A field for the wrong variant/discriminant is accessed

..
   :toolname:`CodePeer` example (4.1.1 - discriminant check)

.. code:: Ada
   :number-lines: 1

   procedure Example is
      type T (B : Boolean := True) is record
         case B is
            when True =>
               J : Integer;
            when False =>
               F : Float;
         end case;
      end record;

      X : T (B => True);

      function Create (F : Float) return T is
        (False, F);
   begin
      X := Create (6.0);
   end Example;

| ``example.adb:16:9: high: discriminant check fails here: requires (Create (6.0)).B = true``

--------------
Precondition
--------------

+ Subprogram call could violate preconditions, either

  + Where the error may occur
  + Where a caller passes in a value causing the error

+ Need to check generated preconditions
+ :toolname:`GNAT Studio` or :command:`--show-backtraces` to analyze checks

.. code:: Ada
   :number-lines: 1

   procedure Example is
      X : Integer := 0;
      function Call (X : Integer) return Integer is
      begin
         if X < 0
         then
            return -1;
         end if;
      end Call;
   begin
      for I in -5 .. 5
      loop
         X := X + Call (I);
      end loop;
   end Example;

| ``example.adb:13:16: high: precondition (conditional check) failure on call to example.call: requires X <= -1``

------
Quiz
------

.. code:: Ada

    function Before_First return Integer is
    begin
       return Integer'First - 1;
    end Before_First;

* Which check will be flagged with the above code?

A. Precondition check
B. Range check
C. :answer:`Overflow check`
D. Underflow check

.. container:: animate

    Out of representation range, so it is flagged for overflow error.
    Range check happens at boundaries: assignment, parameter passing...

------
Quiz
------

.. code:: Ada

   type Ptr_T is access Natural;
   type Idx_T is range 0 .. 10;
   type Arr_T is array (Idx_T) of Ptr_T;

   procedure Update
     (A : in out Arr_T) is
   begin
      for J in Idx_T loop
         declare
            K : constant Idx_T := J - 1;
         begin
            A (K).all := (if A (K) /= null then A (K).all - 1 else 0);
         end;
      end loop;
   end Update;

A. Array index check
B. :answer:`Range check`
C. Overflow check
D. Access check

.. container:: animate

    When :ada:`J = 0`, the declaration of :ada:`K` will raise a :ada:`Constraint_Error`

    If any :ada:`A (K).all = 0`, a second range check is flagged.

=============
User Checks
=============

---------------------
User Check Messages
---------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``assertion``

          - User assertion could fail

        * -

          - eg. :ada:`pragma Assert`

        * - ``conditional check``

          - :ada:`exception` could be raised conditionally

        * - ``raise exception``

          - :ada:`exception` raised on reachable path

        * -

          - Same as *conditional check*, but unconditionally

        * - ``user precondition``

          - Potential violation of specified precondition

        * -

          - :ada:`Pre` aspect or :ada:`pragma Precondition`

        * - ``postcondition``

          - Potential violation of specified postcondition

        * -

          - :ada:`Post` aspect or :ada:`pragma Postcondition`

-----------
Assertion
-----------

A user assertion (using e.g. :ada:`pragma Assert`) could fail

.. code:: Ada
   :number-lines: 1

   procedure Example is
      function And_Or (A, B : Boolean) return Boolean is
      begin
         return False;
      end And_Or;

   begin
      pragma Assert (And_Or (True, True));
   end Example;

| ``example.adb:8:19: high: assertion fails here: requires (and_or'Result) /= false``

-------------------
Conditional Check
-------------------

An exception could be raised **conditionally** in user code

.. code:: Ada
   :number-lines: 8

   if Wrong_Password
   then
      Attempt_Count := Attempt_Count + 1;

      if Attempt_Count > 3
      then
         Put_Line ("max password count reached");
         raise Program_Error;
      end if;
   end if;

| ``example.adb:15:10: high: conditional check raises exception here: requires Attempt_Count <= 3``

-----------------
Raise Exception
-----------------

An exception is raised **unconditionally** on a **reachable** path.

.. code:: Ada
   :number-lines: 2

   Bad : Integer := (raise Constraint_Error);

| ``example.adb:2:22: low: raise exception unconditional raise``

-------------------
User Precondition
-------------------

A call might violate a subprogram's specified precondition.

.. code:: Ada
   :number-lines: 1

   procedure Example is
      function "**" (Left, Right : Float) return Float with
        Import, Pre => Left /= 0.0;

      A : Float := 1.0;
   begin
      A := (A - 1.0)**2.0;
   end Example;

| ``example.adb:7:18: high: precondition (user precondition) failure on call to example."**": requires Left /= 0.0``

---------------
Postcondition
---------------

The subprogram's body may violate its specified postcondition.

.. code:: Ada
   :number-lines: 2

   type Stress_Level is (None, Under_Stress, Destructive);

   function Reduce (Stress : Stress_Level) return Stress_Level is
     (Stress_Level'Val (Stress_Level'Pos (Stress) + 1))
   with
     Pre => (Stress /= None),
     Post => (Reduce'Result /= Destructive);

   procedure Reduce (Stress : in out Stress_Level) is
   begin
      Stress := Reduce (Stress);
   end Reduce;

| ``example.adb:8:40: high: postcondition failure on call to example.reduce: requires example.reduce'Result /= Destructive``

------
Quiz
------

.. code:: Ada

   procedure Raise_Exc (X : Integer) is
   begin
      if X > 0 or X < 0 then
         raise Program_Error;
      else
         pragma Assert (X >= 0);
      end if;
   end Raise_Exc;

* Which check will be flagged with the above code?

A. :answer:`Conditional check`
B. Assertion
C. Raise Exception
D. User precondition

.. container:: animate

    The exception is raised on :ada:`X /= 0`, it is **conditionally** reachable.

    In other cases, :ada:`X = 0` so the assertion always holds.

==========
Warnings
==========

------------------------
Warning Messages (1/2)
------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``dead code``

          - Also called *unreachable code*.

        * -

          - All code should be reachable

        * - ``test always false``

          - Code always evaluating to :ada:`False`

        * - ``test always true``

          - Code always evaluating to :ada:`True`

        * - ``test predetermined``

          - Choice evaluating to constant value

        * -

          - For eg. :ada:`case` statements

        * - ``condition predetermined``

          - Constant operand in a conditional

        * - ``loop does not complete normally``

          - Loop :ada:`exit` condition always :ada:`False`

        * - ``unused assignment``

          - Redundant assignment

        * - ``unused assignment to global``

          - Redundant global object assignment

        * - ``unused out parameter``

          - Actual parameter of a call is ignored

        * -

          - Either never used or overwritten

------------------------
Warning Messages (2/2)
------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``useless reassignment``

          - Assignment does not modify the object

        * - ``suspicious precondition``

          - Precondition seems to have a logic flaw

        * -

          - eg. possible set of values is not contiguous

        * - ``suspicious input``

          - :ada:`out` parameter read before assignment

        * -

          - should be :ada:`in out`

        * - ``unread parameter``

          - :ada:`in out` parameter is never read

        * -

          - should be :ada:`out`

        * - ``unassigned parameter``

          - :ada:`in out` parameter is never assigned

        * -

          - should be :ada:`in`

        * - ``suspicious constant operation``

          - Constant result from variable operands

        * -

          - May hint at a typo, or missing operation

        * - ``subp never returns``

          - Subprogram will never terminate

        * - ``subp always fails``

          - Subprogram will always terminate in error

----------------------------------------
Dead Code | Always True | Always False
----------------------------------------

+ Also called **unreachable code**.
+ All code is expected to be reachable

.. code:: Ada
   :number-lines: 1

   procedure Example (X : out Integer) is
      I : Integer := 10;
   begin
      if I < 4 then
         X := -1;
      elsif I > 8 then
         X := 1;
      else
         X := 0;
      end if;
   end Example;

.. container:: latex_environment small

   | ``example.adb:4:9: low warning: test always false because I = 10``
   | ``example.adb:5:9: medium warning: dead code because I = 10``
   | ``example.adb:6:4: medium warning: test always true because I = 10``
   | ``example.adb:9:9: medium warning: dead code because I = 10``

--------------------
Test Predetermined
--------------------

+ Similar to ``test always true`` and ``test always false``

  + When choice is not binary
  + eg. :ada:`case` statement

.. code:: Ada
   :number-lines: 1

   procedure Example is
      I : Integer := 10;
   begin
      case I is
         when 0 =>
            null;
         when 1 =>
            null;
         when others =>
            null;
      end case;
   end Example;

| ``example.adb:4:4: low warning: test predetermined because I = 10``

-------------------------
Condition Predetermined
-------------------------

+ **Redundant** condition in a boolean operation

.. code:: Ada
   :number-lines: 2

   type Enum_T is (One, Two, Three);

   procedure Or_Else (V : Enum_T) is
   begin
      if V /= One or else V /= Two
      then
         return;
      else
         raise Program_Error;
      end if;
   end Or_Else;

| ``example.adb:6:29: medium warning: condition predetermined because (V /= Two) is always true``

*(If the first subcondition is false, that means V has to be One, so the second subcondition will always be true)*

---------------------------------
Loop Does Not Complete Normally
---------------------------------

+ The loop will never satisfies its **exit condition**
+ Causes can be

  + Exit condition is always :ada:`False`
  + An exception is raised
  + The exit condition code is unreachable (dead code)

.. code:: Ada
   :number-lines: 1

   procedure Example is
      Buf : String := "The" & ASCII.NUL;
      Bp  : Natural;
   begin
      Buf (4) := 'a';   -- Eliminates null terminator
      Bp      := Buf'First;

      loop
         Bp := Bp + 1;
         exit when Buf (Bp - 1) = ASCII.NUL; -- Condition never reached
      end loop;
   end Example;

| ``example.adb:9:10: medium warning: loop does not complete normally``

-------------------
Unused Assignment
-------------------

+ Object is assigned a value that is never read
+ Unintentional loss of result or unexpected control flow
+ Object with the following names won't be checked:

  + :ada:`ignore`, :ada:`unused`, :ada:`discard`, :ada:`dummy`, :ada:`tmp`, :ada:`temp`

+ :ada:`pragma Unreferenced` also ignored

.. code:: Ada
   :number-lines: 2

   procedure Example (I : out Integer) is
   begin
      I := Integer'Value (Get_Line);
      I := Integer'Value (Get_Line);
   end Example;

| ``example.adb:4:6: medium warning: unused assignment into I``

-----------------------------
Unused Assignment To Global
-----------------------------

+ Global variable assigned more than once between reads
+ Note: the redundant assignment may occur deep in the **call tree**

.. code:: Ada
   :number-lines: 1

   procedure Example (I : out Integer) is
      Var : Integer := 0;
      procedure Proc1 (X : Integer) is
      begin
         Var := X;
      end Proc1;
   begin
      Proc1 (123);
      Var := 456;
   end Example;

| ``example.adb:9:11: medium warning: unused assignment into Var``

----------------------
Unused Out Parameter
----------------------

+ Actual :ada:`out` parameter of a call is ignored

  + either never used
  + or overwritten

.. code:: Ada
   :number-lines: 1

   procedure Example is
      Y : Integer;
      procedure Proc (X : out Integer) is
      begin
         X := 1_234;
      end Proc;
   begin
      Proc (Y);
   end Example;

| ``example.adb:8:4: medium warning: unused out parameter Y``

----------------------
Useless Reassignment
----------------------

+ Assignments do not modify the value stored in the assigned object

.. code:: Ada
   :number-lines: 1

   procedure Example (A : in out Integer) is
      B : Integer := A;
   begin
      A := B;
   end Example;

| ``example.adb:4:6: medium warning: useless reassignment of A``

-------------------------
Suspicious Precondition
-------------------------

+ Set of allowed inputs is **not contiguous**

  + some values **in-between** allowed inputs can cause **runtime errors**

.. code:: Ada
   :number-lines: 8

   procedure Push (S : in out Stack_Type;
                   V :        Integer) is
   begin
      if S.Last = S.Tab'Last
      then
         raise Overflow;
      end if;
      -- Increment Last
      S.Last         := S.Last - 1;
      S.Tab (S.Last) := V;
   end Push;

| ``example.adb:8:4: medium warning: suspicious precondition for S.Last: not a contiguous range of values``

------------------
Suspicious Input
------------------

+ :ada:`out` parameter read before assignment
+ Should have been an :ada:`in out`
+ Ada standard allows it

  + but it is a bug most of the time

..
   :toolname:`CodePeer` example (4.1.4 - suspicious input)

.. code:: Ada
   :number-lines: 6

   procedure Take_In_Out (R : in out T) is
   begin
      R.I := R.I + 1;
   end Take_In_Out;

   procedure Take_Out (R : out T;
      B :     Boolean) is begin
      Take_In_Out (R);
   end Take_Out;

| ``example.adb:13:7: medium warning: suspicious input R.I: depends on input value of out-parameter``

------------------
Unread Parameter
------------------

+ :ada:`in out` parameter is not read

  + but is assigned on **all** paths
  + Could be declared :ada:`out`

.. code:: Ada
   :number-lines: 1

   procedure Example (X : in out Integer) is
   begin
      X := 0;
   end Example;

| ``example.adb:1:1: medium warning: unread parameter X: could have mode out``

----------------------
Unassigned Parameter
----------------------

+ :ada:`in out` parameter is never assigned

  + Could be declared :ada:`in`

.. code:: Ada
   :number-lines: 1

   procedure Example
     (X : in out Integer;
      Y :    out Integer) is
   begin
      Y := X;
   end Example;

| ``example.adb:1:1: medium warning: unassigned parameter X: could have mode in``

-------------------------------
Suspicious Constant Operation
-------------------------------

+ Constant value calculated from **non-constant operands**
+ Hint that there is a **coding mistake**

  + either a **typo**, using the **wrong variable**
  + or an operation that is **missing**

    + eg :ada:`Float` conversion before division

.. code:: Ada
   :number-lines: 2

   type T is new Natural range 0 .. 14;

   function Incorrect (X : T) return T is
   begin
      return X / (T'Last + 1);
   end Incorrect;

| ``example.adb:6:16: medium warning: suspicious constant operation X/15 always evaluates to 0``

--------------------
Subp Never Returns
--------------------

+ Subprogram will **never** return

  + presumably **infinite loop**

+ Typically, **another message** in the body can explain why

  + eg. ``test always false``

.. code:: Ada
   :number-lines: 3

   procedure Infinite_Loop is
      X : Integer := 33;
   begin
      loop
         X := X + 1;
      end loop;
   end Infinite_Loop;

| ``example.adb:3:4: medium warning: subp never returns: example.infinite_loop``
| ``example.adb:7:12: medium warning: loop does not complete normally``
| ``example.adb:7:17: low: overflow check might fail: requires X /= Integer_32'Last``

-------------------
Subp Always Fails
-------------------

+ A run-time problem could occur on **every** execution
+ Typically, **another message** in the body can explain why

.. code:: Ada
   :number-lines: 3

   procedure P is
      X : Integer := (raise Program_Error);
   begin
      null;
   end P;

| ``example.adb:3:4: high warning: subp always fails: example.p fails for all possible inputs``
| ``example.adb:4:23: low: raise exception unconditional raise``

------
Quiz
------

.. code:: Ada

    function F (A : Integer; B : Integer) return Integer is
    begin
        if A > B then
           return 0;
        elsif A < B + 1 then
           return 1;
        elsif A /= B then
           return 2;
        end if;
    end F;

* Which warning(s) will be reported with the above code?

A. :answer:`Dead Code`
B. Condition Predetermined
C. Test Always False
D. :answer:`Test Always True`

.. container:: animate

    The last :ada:`elsif` can never be reached.

=================
Race Conditions
=================

-------------------------
Race Condition Messages
-------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``unprotected access``

          - Shared object access without lock

        * - ``unprotected shared access``

          - Object is referenced is multiple tasks

        * -

          - And accessed without a lock

        * - ``mismatch protected access``

          - Mismatch in locks used

        * -

          - Checked for all shared objects access

        * -

          - eg. task1 uses lock1, task2 uses lock2

-------------------------
Race Condition Examples
-------------------------

.. code:: Ada
   :number-lines: 13

   procedure Increment is
   begin
      Acquire;
      if Counter = Natural'Last then
         Counter := Natural'First;
      else
         Counter := Counter + 1;
      end if;
      Release;
   end Increment;

   procedure Decrement is
   begin  --  no Acquire/Release
      if Counter = Natural'First then
         Counter := Natural'Last;
      else
         Counter := Counter - 1;
      end if;
   end Decrement;

.. container:: latex_environment tiny

  ``race.adb:26:10: medium warning: mismatched protected access of shared object Counter via race.increment``

  ``race.adb:26:10: medium warning: unprotected access of Counter via race.decrement``

  ``race.adb:27:18: medium warning: mismatched protected access of shared object Counter via race.increment``

  ``race.adb:27:18: medium warning: unprotected access of Counter via race.decrement``

=====================================
Automatically Generated Annotations
=====================================

-----------------------
Generated Annotations
-----------------------

+ :toolname:`CodePeer` generates **annotations** on the code
+ Not errors
+ Express **properties** and **assumptions** on the code
+ Can be reviewed

    + But not necessarily
    + Can help spot **inconsistencies**

+ Can help understand and **debug** messages

------------------------
Annotations Categories
------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Annotation*

          - *Description*

        * - ``precondition``

          - Requirements imposed on the subprogram's inputs

        * - ``postcondition``

          - Presumption on the outputs of a subprogram

        * - ``presumption``

          - Presumption on the result of an **external** subprogram

        * - ``unanalyzed call``

          - External calls to unanalyzed subprograms

        * - ``global inputs``

          - Global variables **referenced** by each subprogram

        * - ``global outputs``

          - Global variables **modified** by each subprogram

        * - ``new objects``

          - Unreclaimed heap-allocated object

--------------
Precondition
--------------

+ Requirements imposed on the subprogram inputs

    - eg. a certain parameter to be non-null

+ Checked at every call site
+ A message is given for any precondition that a caller **might** violate.

    - Includes the **checks involved** in the requirements

.. code:: ada

    procedure Assign (X : out Integer; Y : in Integer) is
    begin
      X := Y + 1;
    end Assign;
    -- assign.adb:1: (pre)- assign:(overflow check [CWE 190])
    -- Y /= 2_147_483_647

---------------
Postcondition
---------------

+ Inferences about the outputs of a subprogram

.. code:: ada
    :number-lines: 2

    -- assign.adb:1: (post)- assign:X /= -2_147_483_648
    -- assign.adb:1: (post)- assign:X = Y + 1

-------------
Presumption
-------------

+ Presumption about the results of an **external** subprogram

    - Code is unavailable
    - Code is in a separate partition

+ Separate presumptions for each call site

.. code::

    <subprogram-name>@<line-number-of-the-call>

+ Generally not used to determine preconditions of the calling routine

    - but they might influence postconditions of the calling routine.

.. code:: ada

    procedure Above_Call_Unknown (X : out Integer) is
    begin
      Call_Unknown (X);
      pragma Assert (X /= 10);
    end Above_Call_Unknown;
    -- (presumption)- above_call_unknown:unknown.X@4 /= 10

-----------------
Unanalyzed Call
-----------------

+ External calls to unanalyzed subprograms

    - Participate in the determination of presumptions

+ These annotations include **all** unanalyzed calls

    - **Direct** calls
    - Calls in the **call graph** subtree

        + **If** they have an influence on the current subprograms

.. code:: ada

    -- above_call_unknown.adb:2: (unanalyzed)-
    --     above_call_unknown:call on unknown

-----------------------
Global Inputs/Outputs
-----------------------

+ Global variables referenced by each subprogram
+ Only includes **enclosing** objects

    - Not e.g. specific components

+ For accesses, only the **access object** is listed

    - Dereference to accesses **may** be implied by the access object listed

.. code:: ada

    procedure Double_Pointer_Assign (X, Y : in Ptr) is
    begin
       X.all := 1;
       Y.all := 2;
    end Double_Pointer_Assign;
    -- call_double_pointer_assign.adb:4: (global outputs)-
    --     call_double_pointer_assign.call:X, Y

-------------
New Objects
-------------

+ Unreclaimed heap-allocated objects

    - **Created** by a subprogram
    - **Not reclaimed** during the execution of the subprogram itself

+ New objects that are accessible **after** return from the subprogram

.. code:: ada

   procedure Create (X : out Ptr) is
   begin
      X := new Integer;
   end;
   -- alloc.adb:2: (post)- alloc.create:X =
   --     new integer(in alloc.create)#1'Address
   -- alloc.adb:2: (post)- alloc.create:
   --     new integer(in alloc.create)#1.<num objects> = 1
