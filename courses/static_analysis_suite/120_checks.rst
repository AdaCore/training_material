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

..
   :toolname:`CodePeer` example (4.1.1 - divide by zero)

.. code:: Ada
   :number-lines: 1

   procedure Div is
      type Int is range 0 .. 2**32 - 1;
      A : Int := Int'Last;
      X : Integer;
   begin
      for I in Int range 0 .. 2 loop
         X := Integer (A / I); -- division by zero when I=0
      end loop;
   end Div;

| ``high: divide by zero fails here: requires I /= 0``

-------------
Range Check
-------------

+ Calculation may generate a value outside the :ada:`range` of an Ada type or subtype
+ Will generate a :ada:`Constraint_Error`

..
   :toolname:`CodePeer` example (4.1.1 - range check)

.. code:: Ada
   :number-lines: 1

   subtype Constrained_Integer is Integer range 1 .. 2;
   A : Integer;

   procedure Proc_1 (I : in Constrained_Integer) is
   begin
      A := I + 1;
   end Proc_1;
   ...
   A := 0;
   Proc_1 (I => A);  --  A is out-of-range of parameter I

| ``high: range check fails here: requires A in 1..2``

----------------
Overflow Check
----------------

+ Calculation may overflow the bounds of a numeric type.
+ Depends on the size of the underlying (base) type
+ Will generate a :ada:`Constraint_Error`

..
   :toolname:`CodePeer` example (4.1.1 - overflow check)

.. code:: Ada
   :number-lines: 1

   is
      Attempt_Count : Integer := Integer'Last;
   begin
      -- Forgot to reset Attempt_Count to 0
      loop
         Put ("Enter password to delete system disk");
         if Get_Correct_Pw then
            Allow_Access;
         else
            Attempt_Count := Attempt_Count + 1;

| ``high: overflow check fails here: requires Attempt_Count /= Integer_32'Last``
| ``high: overflow check fails here: requires Attempt_Count in Integer_32'First-1..Integer_32'Last-1``

-------------------
Array Index Check
-------------------

+ Index value could be outside the array bounds
+ Also known as **buffer overflow**.
+ Will generate a :ada:`Constraint_Error`

..
   :toolname:`CodePeer` example (4.1.1 - array index check)

.. code:: Ada
   :number-lines: 1

   procedure Buffer_Overflow is
      type Int_Array is array (0 .. 2) of Integer;
      X, Y : Int_Array;
   begin
      for I in X'Range loop
         X (I) := I + 1;
      end loop;

      for I in X'Range loop
         Y (X (I)) := I;  -- Bad when I = 2, since X (I) = 3
      end loop;
   end Buffer_Overflow;

| ``high: array index check fails here: requires (X (I)) in 0..2``

--------------
Access Check
--------------

+ Attempting to dereference a reference that could be :ada:`null`
+ Will generate an :ada:`Access_Error`

..
   :toolname:`CodePeer` example (4.1.1 - access check)

.. code:: Ada
   :number-lines: 1

   procedure Null_Deref is
      type Int_Access is access Integer;
      X : Int_Access;
   begin
      if X = null then
         X.all := 1;  -- null dereference
      end if;
   end Null_Deref;

| ``high: access check fails here``

----------------
Aliasing Check
----------------

+ Some parameters could be passed as **reference**
+ Deduced preconditions:

  + Do not **reference** another parameter
  + Do not **match** the address of a global object

..
   :toolname:`CodePeer` example (4.1.1 - aliasing check)

.. code:: Ada
   :number-lines: 1

      procedure In_Out (A : Int_Array; B : out Int_Array) is
      begin
         B (1) := A (1) + 1;
         ...
         B (1) := A (1) + 2;
      end In_Out;
   ...
      In_Out (A, A); -- Aliasing!

| ``high: precondition (aliasing check) failure on call to alias.in_out: requires B /= A``

-----------
Tag Check
-----------

A tag check operation on a :ada:`tagged` object might raise a :ada:`Constraint_Error`

..
   :toolname:`CodePeer` example (4.1.1 - tag check)

.. code:: Ada
   :number-lines: 1

   is
      type T1 is tagged null record;
      type T2 is new T1 with null record;

      procedure Call (X1 : T1'Class) is
      begin
         An_Operation (T2'Class (X1));
      end Call;

      X1 : T1;
      X2 : T2;
   begin
      Call (X1); -- not OK, Call requires T2'Class

| ``high: precondition (tag check) failure on call to tag.call: requires X1'Tag in {tag.pkg.t2}``

----------
Validity
----------

..
   :toolname:`CodePeer` example (4.1.3 - validity check)

.. code:: Ada

    procedure Uninit is
       A : Integer;
       B : Integer;
    begin
       A := B;  --  we are reading B which is uninitialized!
    end Uninit;

| ``high: validity check: B is uninitialized here``

--------------------
Discriminant Check
--------------------

A field for the wrong variant/discriminant is accessed

..
   :toolname:`CodePeer` example (4.1.1 - discriminant check)

.. code:: Ada
   :number-lines: 1

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
   ...
   X := Create (6.0);  -- discriminant check failure

| ``high: discriminant check fails here: requires (Create (6.0).b = True)``

--------------
Precondition
--------------

+ Subprogram call could violate preconditions, either

  + Where the error may occur
  + Where a caller passes in a value causing the error

+ Need to check generated preconditions
+ :toolname:`GNAT Studio` or :command:`--show-backtraces` to analyze checks

..
   :toolname:`CodePeer` example (4.1.1 - precondition)

.. code:: Ada
   :number-lines: 1

   function Call (X : Integer) return Integer is
   begin
      if X < 0 then
         return -1;
      end if;
   end Call;
   ...
   for I in -5 .. 5 loop
      X := X + Call (I);
   end loop;

| ``high: precondition (conditional check) failure on call to precondition.call: requires X < 0``

------
Quiz
------

* Which check will be flagged with the following?

.. code:: Ada

    function Before_First return Integer is
    begin
       return Integer'First - 1;
    end Before_First;

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

* Which check will be flagged with the following?

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

          - A user assertion could fail

        * -

          - eg. :ada:`pragma Assert`

        * - ``conditional check``

          - An :ada:`exception` could be raised conditionally

        * - ``raise exception``

          - An :ada:`exception` is raised on a reachable path

        * -

          - Same as *conditional check*, but unconditionally

        * - ``user precondition``

          - Potential violation of a specified precondition

        * -

          - As a :ada:`Pre` aspect or as a :ada:`pragma Precondition`

        * - ``postcondition``

          - Potential violation of a specified postcondition

        * -

          - As a :ada:`Post` aspect or as a :ada:`pragma Postcondition`

-----------
Assertion
-----------

A user assertion (using e.g. :ada:`pragma Assert`) could fail

..
   :toolname:`CodePeer` example (4.1.2 - assertion)

.. code:: Ada
   :number-lines: 1

   procedure Assert is

      function And_Or (A, B : Boolean) return Boolean is
      begin
         return False;
      end And_Or;

   begin
      pragma Assert (And_Or (True, True));
   end Assert;

| ``high: assertion fails here: requires (and_or'Result) /= false``

-------------------
Conditional Check
-------------------

An exception could be raised **conditionally** in user code

..
   :toolname:`CodePeer` example (4.1.2 - conditional check)

.. code:: Ada
   :number-lines: 1

   if Wrong_Password then
      Attempt_Count := Attempt_Count + 1;

      if Attempt_Count > 3 then
         Put_Line ("max password count reached");
         raise Program_Error;
      end if;
   end if;

| ``high: conditional check raises exception here: requires Attempt_Count <= 3``

-----------------
Raise Exception
-----------------

An exception is raised **unconditionally** on a **reachable** path.

..
   :toolname:`CodePeer` example (4.1.2 - raise exception)

.. code:: Ada
   :number-lines: 1

   procedure Raise_Exc is
      X : Integer := raise Program_Error;
   begin
      null;
   end Raise_Exc;

| ``low: raise exception unconditional raise``

-------------------
User Precondition
-------------------

A call might violate a subprogram's specified precondition.

..
   :toolname:`CodePeer` example (4.1.2 - user precondition)

.. code:: Ada
   :number-lines: 1

   procedure Pre is
      function "**" (Left, Right : Float) return Float with
         Import,
         Pre => Left /= 0.0;

      A : Float := 1.0;
   begin
      A := (A - 1.0)**2.0;
   end Pre;

| ``high: precondition (user precondition) failure on call to pre."**": requires Left /= 0.0``

---------------
Postcondition
---------------

The subprogram's body may violate its specified postcondition.

..
   :toolname:`CodePeer` example (4.1.2 - postcondition)

.. code:: Ada
   :number-lines: 1

   type Stress_Level is (None, Under_Stress, Destructive);

   function Reduce (Stress : Stress_Level)
     return Stress_Level with
      Pre  => (Stress /= None),
      Post => (Reduce'Result /= Destructive)
      is (Stress_Level'Val (Stress_Level'Pos (Stress) + 1));
      --                                              ^
      --                                             Typo!
   ...
   Reduce (My_Component_Stress);

| ``high: postcondition failure on call to post.reduce: requires Reduce'Result /= Destructive``

------
Quiz
------

* Which user check will be raised with the following?

.. code:: Ada

   procedure Raise_Exc (X : Integer) is
   begin
      if X > 0 or X < 0 then
         raise Program_Error;
      else
         pragma Assert (X >= 0);
      end if;
   end Raise_Exc;

A. :answer:`Conditional check`
B. Assertion
C. Raise Exception
D. User precondition

.. container:: animate

    The exception is raised on :ada:`X /= 0`, it is **conditionally** reachable.

    In other cases, :ada:`X = 0` so the assertion always holds.

=====================================
Uninitialized and Invalid Variables
=====================================

----------------------------------------------
Uninitialized and Invalid Variables Messages
----------------------------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``validity check``

          - An uninitialized or invalid value could be read

----------------
Validity Check
----------------

The code may be reading an uninitialized or invalid value

..
   :toolname:`CodePeer` example (4.1.3 - validity check)

.. code:: Ada
   :number-lines: 1

   procedure Uninit is
      A : Integer;
      B : Integer;
   begin
      A := B;  --  we are reading B which is uninitialized!
   end Uninit;

| ``high: validity check: B is uninitialized here``

==========
Warnings
==========

------------------------
Warning Messages (1/3)
------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``dead code``

          - Also called *unreachable code*.

        * -

          - Assumed all code should be reachable

        * - ``test always false``

          - Code always evaluating to :ada:`False`

        * - ``test always true``

          - Code always evaluating to :ada:`True`

        * - ``test predetermined``

          - Choice evaluating to a constant value

        * -

          - For eg. :ada:`case` statements

        * - ``condition predetermined``

          - Constant RHS or LHS in a conditional

        * - ``loop does not complete normally``

          - Loop :ada:`exit` condition is always :ada:`False`

        * - ``unused assignment``

          - Redundant assignment

        * - ``unused assignment to global``

          - Redundant global object assignment

        * - ``unused out parameter``

          - Actual parameter of a call is ignored

        * -

          - Either never used or overwritten

+ **RHS** : Right-Hand-Side of a binary operation
+ **LHS** : Left-Hand-Side of a binary operation

------------------------
Warning Messages (2/3)
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

-------------------------------
Warning Messages - infer (3/3)
-------------------------------

.. container:: latex_environment

   .. list-table::
        :header-rows: 1

        * - *Message*

          - *Description*

        * - ``same operands``

          - Binary operator has the same argument twice

        * - ``same logic``

          - Same argument appears twice in a boolean expression

        * - ``duplicate branches``

          - Duplicate code in 'if' or 'case' branches

        * - ``test duplication``

          - An expression is tested multiple times

        * -

          - in an :ada:`if ... elsif ... else`

-----------
Dead Code
-----------

+ Also called **unreachable code**.
+ All code is expected to be reachable

..
   :toolname:`CodePeer` example (4.1.4 - dead code)

.. code:: Ada
   :number-lines: 1

   procedure Dead_Code (X : out Integer) is
      I : Integer := 10;
   begin
      if I < 4 then
         X := 0;
      elsif I >= 8 then
         X := 0;
      end if;
   end Dead_Code;

| ``medium warning: dead code because I = 10``

-------------------
Test Always False
-------------------

Redundant conditionals, always :ada:`False`

..
   :toolname:`CodePeer` example (4.1.4 - test always false)

.. code:: Ada
   :number-lines: 1

   procedure Dead_Code (X : out Integer) is
      I : Integer := 10;
   begin
      if I < 4 then
         X := 0;
      end if;
   end Dead_Code;

| ``low warning: test always false because I = 10``

------------------
Test Always True
------------------

Redundant conditionals, always :ada:`True`

..
   :toolname:`CodePeer` example (4.1.4 - test always true)

.. code:: Ada
   :number-lines: 1

   procedure Dead_Code (X : out Integer) is
      I : Integer := 10;
   begin
      if I >= 8 then
         X := 0;
      end if;
   end Dead_Code;

| ``medium warning: test always true because I = 10``

--------------------
Test Predetermined
--------------------

+ Similar to ``test always true`` and ``test always false``

  + When choice is not binary
  + eg. :ada:`case` statement

..
   :toolname:`CodePeer` example (4.1.4 - test predetermined)

.. code:: Ada
   :number-lines: 1

   procedure Predetermined is
      I : Integer := 0;
   begin
      case I is
         when 0 =>
            null;
         when 1 =>
            null;
         when others =>
            null;
      end case;
   end Predetermined;

| ``low warning: test predetermined because I = 0``

-------------------------
Condition Predetermined
-------------------------

+ **Redundant** condition in a boolean operation
+ RHS operand is **constant** in this context

..
   :toolname:`CodePeer` example (4.1.4 - condition predetermined)

.. code:: Ada
   :number-lines: 1

      if V /= A or else V /= B then
         --     ^^^^^^^
         --     V = A, so V /= B
         raise Program_Error;
      end if;

| ``medium warning: condition predetermined because (V /= B) is always true``

---------------------------------
Loop Does Not Complete Normally
---------------------------------

+ The loop will never satisfies its **exit condition**
+ Causes can be

  + Exit condition is always :ada:`False`
  + An exception is raised
  + The exit condition code is unreachable (dead code)

..
   :toolname:`CodePeer` example (4.1.4 - loop does not complete normally)

.. code:: Ada
   :number-lines: 1

   procedure Loops is
      Buf : String := "The" & ASCII.NUL;
      Bp  : Natural;
   begin
      Buf (4) := 'a';   -- Eliminates null terminator
      Bp      := Buf'First;

      loop
         Bp := Bp + 1;
         exit when Buf (Bp - 1) = ASCII.NUL; -- Condition never reached
      end loop;
   end Loops;

| ``medium warning: loop does not complete normally``

-------------------
Unused Assignment
-------------------

+ Object is assigned a value that is never read
+ Unintentional loss of result or unexpected control flow
+ Object with the following names won't be checked:

  + :ada:`ignore`, :ada:`unused`, :ada:`discard`, :ada:`dummy`, :ada:`tmp`, :ada:`temp`

+ :ada:`pragma Unreferenced` also ignored

..
   :toolname:`CodePeer` example (4.1.4 - unused assignment)

.. code:: Ada
   :number-lines: 1

   I := Integer'Value (Get_Line);
   I := Integer'Value (Get_Line);

| ``medium warning: unused assignment into I``

-----------------------------
Unused Assignment To Global
-----------------------------

+ Global variable assigned more than once between reads
+ Note: the redundant assignment may occur deep in the **call tree**

..
   :toolname:`CodePeer` example (4.1.4 - unused assignment to global)

.. code:: Ada
   :number-lines: 1

   procedure Proc1 is
   begin
      G := 123;
   end Proc1;

   procedure Proc is
   begin
      Proc1;
      G := 456;  -- override effect of calling Proc1
   end Proc;

| ``low warning: unused assignment to global G in unused_global.p.proc1``

----------------------
Unused Out Parameter
----------------------

+ Actual :ada:`out` parameter of a call is ignored

  + either never used
  + or overwritten

..
   :toolname:`CodePeer` example (4.1.4 - unused out parameter)

.. code:: Ada
   :number-lines: 1

   procedure Search (Success : out Boolean);
   ...
   procedure Search is
      Ret_Val : Boolean;
   begin
      Search (Ret_Val);
   end Search;

| ``medium warning: unused out parameter Ret_Val``

----------------------
Useless Reassignment
----------------------

+ Assignments do not modify the value stored in the assigned object

..
   :toolname:`CodePeer` example (4.1.4 - useless reassignment)

.. code:: Ada
   :number-lines: 1

   procedure Self_Assign (A : in out Integer) is
      B : Integer;
   begin
      B := A;
      A := B;
   end Self_Assign;

| ``medium warning: useless reassignment of A``

-------------------------
Suspicious Precondition
-------------------------

+ Set of allowed inputs is **not contiguous**

  + some values **in-between** allowed inputs can cause **runtime errors**

+ Certain cases may be missing from the user's precondition
+ May be a **false-positive** depending on the algorithm

..
   :toolname:`CodePeer` example (4.1.4 - suspicious precondition)

.. code:: Ada
   :number-lines: 1

   if S.Last = S.Arr'Last then
      raise Overflow;
   end if;
   --  Typo: Should be S.Last + 1
   S.Last         := S.Last - 1;
   --  Error when S.Last = S.Arr'First - 1
   S.Arr (S.Last) := V;

| ``medium warning: suspicious precondition for S.Last: not a contiguous range of values``

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
   :number-lines: 1

   procedure Take_In_Out (R : in out T);
   ...
   procedure Take_Out (R : out T; B : Boolean) is
   begin
      Take_In_Out (R);  -- R is 'out' but used as 'in out'
   end Take_Out;

| ``medium warning: suspicious input R.I: depends on input value of out-parameter``

------------------
Unread Parameter
------------------

+ :ada:`in out` parameter is not read

  + but is assigned on **all** paths
  + Could be declared :ada:`out`

..
   :toolname:`CodePeer` example (4.1.4 - unread parameter)

.. code:: Ada
   :number-lines: 1

   procedure Unread (X : in out Integer) is
   begin
      X := 0;  -- X is assigned but never read
   end Unread;

| ``medium warning: unread parameter X: could have mode out``

----------------------
Unassigned Parameter
----------------------

+ :ada:`in out` parameter is never assigned

  + Could be declared :ada:`in`

..
   :toolname:`CodePeer` example (4.1.4 - unassigned parameter)

.. code:: Ada
   :number-lines: 1

   procedure Unassigned
     (X : in out Integer; Y : out Integer) is
   begin
      Y := X;  -- X is read but never assigned
   end Unassigned;

| ``medium warning: unassigned parameter X: could have mode in``

-------------------------------
Suspicious Constant Operation
-------------------------------

+ Constant value calculated from **non-constant operands**
+ Hint that there is a **coding mistake**

  + either a **typo**, using the **wrong variable**
  + or an operation that is **missing**

    + eg :ada:`Float` conversion before division

..
   :toolname:`CodePeer` example (4.1.4 - suspicious constant operation)

.. code:: Ada
   :number-lines: 1

   type T is new Natural range 0 .. 14;

   function Incorrect (X : T) return T is
   begin
      return X / (T'Last + 1);
   end Incorrect;

| ``medium warning: suspicious constant operation X/15 always evaluates to 0``

--------------------
Subp Never Returns
--------------------

+ Subprogram will **never** return

  + presumably **infinite loop**

+ Typically, **another message** in the body can explain why

  + eg. ``test always false``

..
   :toolname:`CodePeer` example (4.1.4 - subp never returns)

.. code:: Ada
   :number-lines: 1

   procedure Infinite_Loop is
      X : Integer := 33;
   begin
      loop
         X := X + 1;
      end loop;
   end Infinite_Loop;

| ``medium warning: subp never returns: infinite_loop``

-------------------
Subp Always Fails
-------------------

+ A run-time problem could occur on **every** execution
+ Typically, **another message** in the body can explain why

..
   :toolname:`CodePeer` example (4.1.4 - subp always fails)

.. code:: Ada
   :number-lines: 1

   procedure P is
      X : Integer := raise Program_Error;
   begin
      null;
   end P;

| ``high warning: subp always fails: p fails for all possible inputs``

-------------------
Same Operands
-------------------

+ The two operands of a binary operation are syntactically equivalent
+ The resulting expression will always yield the same value

.. code:: Ada
   :number-lines: 1

   function Same_Op (X : Natural) return Integer is
   begin
      --  Copy/paste error? Always return 1
      return (X + 1) / (X + 1);
   end Same_Op;

| ``medium warning: same operands (Infer): operands of '/' are identical``

-------------------
Same Logic
-------------------

+ The same sub-expression occurs twice in a boolean expression
+ The entire expression can be simplified, or always return the same value

.. code:: Ada
   :number-lines: 1

   function Same_Logic (A, B : Boolean) return Boolean is
   begin
      return A or else B or else A;
   end Same_Logic;

| ``medium warning: same operands (Infer): 'A' duplicated at line 3``

-------------------
Test duplication
-------------------

+ The same expression is tested twice in successive :ada:`if ... elsif ... elsif ...`
+ Usually indicates a copy-paste error

.. code:: Ada
   :number-lines: 1

   procedure Same_Test (Str : String) is
      A : constant String := "toto";
      B : constant String := "titi";
   begin
      if Str = A then
         Ada.Text_IO.Put_Line("Hello, tata!");
      elsif Str = B then
         Ada.Text_IO.Put_Line("Hello, titi!");
      elsif Str = A then
         Ada.Text_IO.Put_Line("Hello, toto!");
      else
         Ada.Text_IO.Put_Line("Hello, world!");
      end if;
   end Same_Test;

| ``medium warning: same test (Infer): test 'Str = A' duplicated at line 9``

-------------------
Duplicate branches
-------------------

+ Branches are duplicated in :ada:`if` or :ada:`case`
+ Should be refactored, or results from incorrect copy-paste

.. code:: Ada
   :number-lines: 1

   function Dup (X : Integer) return Integer is
   begin
      if X > 0 then
         declare
            A : Integer := X;
            B : Integer := A + 1;
         begin
            return B;
         end;
      else
         declare
            A : Integer := X;
            B : Integer := A + 1;
         begin
            return B;
         end;
      end if;
   end Dup;

| ``infer.adb:4:10: medium warning: duplicate branches (Infer): code duplicated at line 11``

------
Quiz
------

* Which warnings will be reported with the following?

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

A. :answer:`Dead Code`
B. Condition Predetermined
C. Test Always False
D. Test Always True

.. container:: animate

    The last elsif can never be reached.

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

..
   :toolname:`CodePeer` example (4.1.5 - race conditions)

.. code:: Ada
   :number-lines: 1

   procedure Increment is
   begin
      Mutex_Acquire;
      if Counter = Natural'Last then
         Counter := Natural'First;
      else
         Counter := Counter + 1;
      end if;
      Mutex_Release;
   end Increment;

   procedure Reset is
   begin
      Counter := 0; -- lock missing
   end Reset;

| ``medium warning: mismatched protected access of shared object Counter via race.increment``
| ``medium warning: unprotected access of Counter via race.reset``

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
