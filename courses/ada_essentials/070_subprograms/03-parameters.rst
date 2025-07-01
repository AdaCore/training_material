============
Parameters
============

----------------------------------
Subprogram Parameter Terminology
----------------------------------

* :dfn:`Actual parameters` are values passed to a call

   - Variables, constants, expressions

* :dfn:`Formal parameters` are defined by specification

   - Receive the values passed from the actual parameters
   - Specify the types required of the actual parameters
   - Type **cannot** be anonymous

   .. code:: Ada

      procedure Something (Formal1 : in Integer);

      ActualX : Integer;
      ...
      Something (ActualX);

---------------------------------
Parameter Associations in Calls
---------------------------------

* Associate formal parameters with actuals
* Both positional and named association allowed

.. code:: Ada

   Something (ActualX, Formal2 => ActualY);
   Something (Formal2 => ActualY, Formal1 => ActualX);

* Having named **then** positional is forbidden

.. code:: Ada

   -- Compilation Error
   Something (Formal1 => ActualX, ActualY);

----------------------------
Parameter Modes and Return
----------------------------

* Mode :ada:`in`

   - Formal parameter is :ada:`constant`

     * So actual is not modified either

   - Can have **default**, used when **no value** is provided

   .. code:: Ada

       procedure P (N : in Integer := 1; M : in Positive);
       [...]
       P (M => 2);

* Mode :ada:`out`

   - Writing is **expected**
   - Reading is **allowed**
   - Actual **must** be a writable object

* Mode :ada:`in out`

   - Actual is expected to be **both** read and written
   - Actual **must** be a writable object

* Function :ada:`return`

   - **Must** always be handled

---------------------------------
Why Read Mode `out` Parameters?
---------------------------------

* **Convenience** of writing the body

   - No need for readable temporary variable

* Warning: initial value is **not defined**

.. code:: Ada

   procedure Compute (Value : out Integer) is
   begin
     Value := 0;
     for K in 1 .. 10 loop
       Value := Value + K; -- this is a read AND a write
     end loop;
   end Compute;

------------------------------
Parameter Passing Mechanisms
------------------------------

* :dfn:`By-Copy`

   - The formal denotes a separate object from the actual
   - :ada:`in`, :ada:`in out`: actual is copied into the formal **on entry to** the subprogram
   - :ada:`out`, :ada:`in out`: formal is copied into the actual **on exit from** the subprogram

* :dfn:`By-Reference`

   - The formal denotes a view of the actual
   - Reads and updates to the formal directly affect the actual
   - More efficient for large objects

* Parameter **types** control mechanism selection

   - Not the parameter **modes**
   - Compiler determines the mechanism

-------------------------------
By-Copy Vs By-Reference Types
-------------------------------

* By-Copy

   - Scalar types
   - :ada:`access` types

* By-Reference

   - :ada:`tagged` types
   - :ada:`task` types and :ada:`protected` types
   - :ada:`limited` types


* :ada:`array`, :ada:`record`

   - By-Reference when they have by-reference **components**
   - By-Reference for **implementation-defined** optimizations
   - By-Copy otherwise

* :ada:`private` depends on its full definition

* Note that the parameter mode :ada:`aliased` will force pass-by-reference

  * This mode is discussed in the **Access Types** module

------------------------------------------
Unconstrained Formal Parameters or Return
------------------------------------------

* Unconstrained **formals** are allowed

    - Constrained by **actual**

* Unconstrained :ada:`return` is allowed too

    + Constrained by the **returned object**

.. code:: Ada

   type Vector is array (Positive range <>) of Float;
   procedure Print (Formal : Vector);

   Phase : Vector (X .. Y);
   State : Vector (1 .. 4);
   ...
   begin
     Print (Phase);          -- Formal'Range is X .. Y
     Print (State);          -- Formal'Range is 1 .. 4
     Print (State (3 .. 4)); -- Formal'Range is 3 .. 4

-----------------------------------
Unconstrained Parameters Surprise
-----------------------------------

* Assumptions about formal bounds may be **wrong**

.. code:: Ada

  type Vector is array (Positive range <>) of Float;
  function Subtract (Left, Right : Vector) return Vector;

  V1 : Vector (1 .. 10); -- length = 10
  V2 : Vector (15 .. 24); -- length = 10
  R : Vector (1 .. 10); -- length = 10
  ...
  -- What are the indices returned by Subtract?
  R := Subtract (V2, V1);

----------------------
Naive Implementation
----------------------

* **Assumes** bounds are the same everywhere
* Fails when :ada:`Left'First /= Right'First`
* Fails when :ada:`Left'Length /= Right'Length`
* Fails when :ada:`Left'First /= 1`

  .. code:: Ada

   function Subtract (Left, Right : Vector)
     return Vector is
      Result : Vector (1 .. Left'Length);
   begin
      ...
      for K in Result'Range loop
        Result (K) := Left (K) - Right (K);
      end loop;

------------------------
Correct Implementation
------------------------

* Covers **all** bounds
* :ada:`return` indexed by :ada:`Left'Range`

.. code:: Ada

   function Subtract (Left, Right : Vector) return Vector is
      pragma Assert (Left'Length = Right'Length);

      Result : Vector (Left'Range);
      Offset : constant Integer := Right'First - Result'First;
   begin
      for K in Result'Range loop
        Result (K) := Left (K) - Right (K + Offset);
      end loop;

      return Result;
   end Subtract;

------
Quiz
------

.. code:: Ada

   function F (P1 : in     Integer   := 0;
               P2 : in out Integer;
               P3 : in     Character := ' ';
               P4 :    out Character)
      return Integer;
   J1, J2 : Integer;
   C : Character;

Which call(s) is (are) legal?

   A. ``J1 := F (P1 => 1, P2 => J2, P3 => '3', P4 => '4');``
   B. ``J1 := F (P1 => 1, P3 => '3', P4 => C);``
   C. :answermono:`J1 := F (1, J2, '3', C);`
   D. ``F (J1, J2, '3', C);``

.. container:: animate

   Explanations

   A. :ada:`P4` is :ada:`out`, it **must** be a variable
   B. :ada:`P2` has no default value, it **must** be specified
   C. Correct
   D. :ada:`F` is a function, its :ada:`return` **must** be handled

