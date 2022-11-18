
****************
Type Contracts
****************
.. |rightarrow| replace:: :math:`\rightarrow`

==============
Introduction
==============

----------------
Type Contracts
----------------

* Scalar ranges gives tighter bounds to scalar types

  - This applies to array index types as well

* Record discriminants can be specialized to specific values

* Not null access types cannot be null

* **Subtype predicates** generalize all these

  - Greater expressivity by using arbitrary boolean expressions
  - Like type constraints, they should always hold

* **Type invariants** allow temporary violations

  - Applicable to private types
  - They should hold outside of the unit

-------------------------------------
Where are Type Constraints Checked?
-------------------------------------

* On an assignment / explicit initialization

   .. code:: Ada

      X : Integer := 2;

* On a conversion / qualification

   .. code:: Ada

      X : Integer := Integer (1 + Natural'(2));

* On a parameter passing

   .. code:: Ada

      procedure P (N : in out Natural);

* Intermediate expressions are computed using the base type, with no range check

   .. code:: Ada

      X : Natural := 4;
      Y : Natural := 1 + X + 1;

.. container:: speakernote

   For intermediate operations, such as "+" of "Integer", parameters and result of the op are of Integer'Base.
   There are no range checks on these operations (just overflow checks).
   There will be a range check on the result to be assigned before assignment though.

============
Predicates
============

---------------------
What is a Predicate?
---------------------

* Boolean property that should always hold of objects of the type

* Can be specified on a type or subtype

  .. code::Ada

  type Non_Zero is new Integer
    with Predicate => Non_Zero /= 0;

  subtype Even is Integer
    with Predicate => Even mod 2 = 0;

* Predicate can be Static_Predicate or Dynamic_Predicate

  .. code::Ada

  type Non_Zero is new Integer
    with Static_Predicate => Non_Zero /= 0;

  subtype Even is Integer
    with Dynamic_Predicate => Even mod 2 = 0;

.. container:: speakernote

   Caveats include use of invalid values, e.g., via uninitialized variables or unchecked conversion.
   Otherwise Inexpressible Constraints
   Discontinuous ranges cannot be expressed with range constraints.

-------------------------------
Examples of Static Predicates
-------------------------------

.. code:: Ada

   type Serial_Baud_Rate is range 110 .. 115200
     with Static_Predicate =>
       Serial_Baud_Rate in
         110   | 300   | 600   | 1200  | 2400  |
         4800  | 9600  | 14400 | 19200 | 28800 |
         38400 | 56000 | 57600 | 115200;

   subtype Normal_Float is Float with
     with Static_Predicate =>
       Normal_Float <= -2.0**(-126) or
       Normal_Float = 0.0 or
       Normal_Float >= 2.0**(-126);

--------------------------------
Examples of Dynamic Predicates
--------------------------------

.. code:: Ada

   type Prime is new Positive
     with Dynamic_Predicate =>
       (for all Divisor in 2 .. Prime / 2 =>
         Prime mod Divisor /= 0);

   type Bundle is record
      X, Y : Integer;
      CRC  : Unsigned_32;
   end record
     with Dynamic_Predicate => CRC = Math.CRC32 (X, Y);

   type Table is array (M .. N) of Integer
     with Dynamic_Predicate =>
       (for all K in Table'Range =>
         (K = Table'First or else Table(K-1) <= Table(K)));

.. container:: speakernote

   Prime will always be a prime number
   Bundle will always have a valid CRC
   Table will always be sorted

------------------------------------
Discriminant in a Range Constraint
------------------------------------

* The following is illegal:

.. code:: Ada

   type Bounded_String (Capacity : Positive) is
      record
         Value  : String (1 .. Capacity);
         Length : Natural range 0 .. Capacity := 0
      end record;

* Here is a legal alternative:

.. code:: Ada

   type Bounded_String (Capacity : Positive) is
      record
         Value  : String (1 .. Capacity);
         Length : Natural := 0;
      end record
         with Dynamic_Predicate => Length in 0 .. Capacity;

.. container:: speakernote

   The use of the discriminant is illegal because, with mutable types, the discriminant can be changed at run-time.

--------------------------
Why Two Predicate Forms?
--------------------------

* Static predicates are more restricted

  - Boolean combination of comparisons with static values

  - That does not mean statically checked by the compiler

* Dynamic predicates are arbitrary boolean expressions

  - Applicable to array and record types

* Types with static predicates are allowed in more contexts

  - Used as range in a *for loop*

  - Used as choice in *case statement* or *case expression*

--------------------------------------
Allowed Static Predicate Content (1)
--------------------------------------

* Ordinary Ada static scalar or string expressions

   - See RM 4.9 for list

* Static membership test selected by current instance

.. code:: Ada

   type Days is (Sun, Mon, Tues, We, Thu, Fri, Sat);
   subtype Weekend is Days
      with Static_Predicate => Weekend in Sat | Sun;
      -- Given this order, no other way to express
      -- this constraint

.. container:: speakernote

   The order for the Days enumerals is important for this example because we don't want the range Sat .. Sun to be expressible.

--------------------------------------
Allowed Static Predicate Content (2)
--------------------------------------

* Case expressions in which dependent expressions are static and selected by the current instance

   .. code:: Ada

      type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      subtype Weekend is Days with Static_Predicate =>
         (case Weekend is
            when Sat | Sun => True,
            when Mon .. Fri => False);

* Note: if-expressions are disallowed, and not needed

   .. code:: Ada

      subtype Work is Days with Static_Predicate =>
        -- illegal:
        (if Work in Mon .. Fri then True else False);
      subtype Work is Days with Static_Predicate =>
        -- legal:
        Work in Mon .. Fri;

--------------------------------------
Allowed Static Predicate Content (3)
--------------------------------------

* A call to `=`, `/=`, `<`, `<=`, `>`, or `>=` where one operand is the current instance (and the other is static)
* Calls to operators `and`, `or`, `xor`, `not`

   - Only for predefined type Boolean
   - Only with operands of the above

* Short-circuit controls with operands of the above
* Any of the above in parentheses

------------------------------------
Dynamic Predicate Content In SPARK
------------------------------------

* Restrictions in Ada are on usage not content

   .. code:: Ada

      type T is ... with
         Dynamic_Predicate => Foo;
      Global : Integer := 0;
      function Foo return Boolean is
      begin
         Global := Global + 1;  -- function with side effects
         return Global = 42;
      end Foo;

* SPARK adds restrictions on content

   - Cannot read global variables or have side-effects
   - Thus possible to prove predicates

-----------------------------
Types Controlling For-Loops
-----------------------------

* Those with dynamic predicates cannot be used

   - Too expensive to implement loop

   .. code:: Ada

      subtype Even is Integer
         with Dynamic_Predicate => Even mod 2 = 0;
      for K in Even loop -- not legal (how many iterations?)
         ...
      end loop;

* Those with static predicates can be used

   .. code:: Ada

      type Days is (Sun, Mon, Tues, We, Thu, Fri, Sat);
      subtype Weekend is Days
         with Static_Predicate => Weekend in Sat | Sun;
      for K in Weekend loop -- legal ("Sun" comes first)
         ...
      end loop;

---------------------------------------
In Some Cases Neither Kind Is Allowed
---------------------------------------

* An array index or slice specification usage

  - Where gaps in values would be problematic

.. code:: Ada

   type Days is (Sun, Mon, Tues, We, Thu, Fri, Sat);
   subtype Weekend is Days
      with Static_Predicate => Weekend in Sat | Sun;
   type Play is array (Weekend) of Integer; -- illegal
   type List is array (Days range <>) of Integer;
   L : List (Weekend); -- illegal
   M : List (Days); -- legal
   ...
   M (Weekend) := (...); -- illegal

.. container:: speakernote

   An index subtype, discrete range of an index constraint or slice
   A discrete subtype definition of a constrained array definition
   A discrete subtype definition of an entry declaration or entry index specification

-----------------------------------------
Special Attributes for Predicated Types
-----------------------------------------

* `'Range`, `'First`, and `'Last` are not allowed

   - Because they reflect only range constraints, not predicates

* `'First_Valid` and `'Last_Valid`

   - `'First_Valid` returns smallest valid value, taking any range or predicate into account
   - `'Last_Valid` returns largest valid value, taking any range or predicate into account

* Can be used for any static subtype (no dynamic predicate)

* Especially useful for types with static predicates
* `'Succ` and `'Pred` are allowed since they always reflect underlying type anyway

------------------------------------------
Predicate Run-Time Checking (If Enabled)
------------------------------------------

* Inserted automatically by compiler

   - Where a value would be assigned that may violate the constraint of the predicated target

* Violations raise exception `Assertion_Error`

* Performed before value change, like language-defined constraint checks

   - Associated variable is therefore unchanged

* Predicate of `T` also checked for membership test `V in T`

  - Reason for which predicate cannot be ghost

--------------------------------
Dynamic Checking of Predicates
--------------------------------

* Inserted by the compiler on every assignment, parameter passing and type
  conversion/qualification like type constraints

  - No dynamic checking on an uninitialized object

  - No dynamic checking on a reference to the object

  - No dynamic checking on assigning a part of the object

.. code:: Ada

   with Ada.Text_IO;   use Ada.Text_IO;
   procedure Test is
      subtype Even is Integer
         with Dynamic_Predicate => Even mod 2 = 0;
      J, K : Even;
   begin
     Put_Line ("K is" & Integer'Image (K)); -- not checked
     Put_Line ("J is" & Integer'Image (J)); -- not checked
     K := J; -- predicate check here
   end Test;

.. code:: console

   K is 1969492223
   J is 4220029
   raised SYSTEM.ASSERTIONS.ASSERT_FAILURE :
      Dynamic_Predicate failed at test.adb:9

-------------------------------
Static Checking of Predicates
-------------------------------

* :toolname:`GNATprove` verifies predicates are always satisfied on initialized
  objects

  - No static checking on an uninitialized object

  - No static checking on a reference to the object

  - Static checking on assigning a part of the object

* :toolname:`GNATprove` can assume that all initialized objects satisfy their
  predicates

  - Types with predicates cannot be used in objects with relaxed initialization

--------------------------------
Beware Recursion In Predicates
--------------------------------

* Infinite recursion when calling inside the predicate a function taking the
  type with predicate as parameter type

  .. code:: Ada

     type Nat is new Integer with Predicate => Above_Zero (Nat);
     function Above_Zero (X : Nat) return Boolean is (X >= 0);

  .. code:: console

     warning: predicate check includes a call to "Above_Zero" that requires a predicate check
     warning: this will result in infinite recursion
     warning: use an explicit subtype of "Nat" to carry the predicate

* Fix by inlining the property or introducing a subtype

  .. code:: Ada

     type Int is new Integer;
     function Above_Zero (X : Int) return Boolean is (X >= 0);
     subtype Nat is Int with Predicate => Above_Zero (Nat);

=================
Type Invariants
=================

---------------------------
What is a Type Invariant?
---------------------------

* Boolean property that should always hold of objects of the type outside of
  its unit

* Can only be specified on the completion of a private type

  .. code:: Ada

     package Bank is
       type Account is private;
       type Currency is delta 0.01 digits 12;
       ...
     private
       type Account is ... with
         Type_Invariant => Consistent_Balance (Account);

-------------------------------------
Dynamic Checking of Type Invariants
-------------------------------------

* Inserted by the compiler when creating values of type `T`

  - Default initial value of an object of type `T`
  - Type conversion to `T`
  - After calls to *boundary subprograms* that may modify a value of the type
    `T` (subprograms in the public package spec)

* Dynamic checks also introduced for parts of objects of type `T`

* No similar dynamic checks for global variables

* No checking for internal modifications: assignment or call

----------------------------------------
Invariant Over Object Lifetime (Calls)
----------------------------------------

.. image:: type_invariant_check_flow.png

------------------------------------
Static Checking of Type Invariants
------------------------------------

* Inserted by the :toolname:`GNATprove` when exposing values of type `T`

  - Default initial value of an object of type `T`
  - Type conversion to `T`
  - After calls to *boundary subprograms* that may modify a value of the type
    `T` (subprograms in the public package spec)
  - Before internal calls to *external subprograms* (subprograms not internal
    to the unit)
    - Avoid reentrancy problems

* Static checks also introduced for parts of objects of type `T`

* Similar static checks for global variables

* No checking for internal modifications: assignment or call

------------------------
Example Type Invariant
------------------------

* A bank account balance must always be consistent

   - Consistent Balance:  Total Deposits  - Total Withdrawals  =  Balance

.. code:: Ada

   package Bank is
     type Account is private;
     type Currency is delta 0.01 digits 12;
     ...
   private
     type Account is ... with
       Type_Invariant => Consistent_Balance (Account);
     ...
     -- Called automatically for all Account objects
     function Consistent_Balance (This : Account)
       return Boolean;
   end Bank;

-------------------------------------------
Example Type Invariant Realization (Spec)
-------------------------------------------

.. code:: Ada

   package Bank is
     type Account is private;
     type Currency is delta 0.01 digits 12;
     ...
   private
     -- initial state MUST satisfy invariant
     type Account is record
       Owner : Unbounded_String;
       Current_Balance : Currency := 0.0;
       Withdrawals : Transaction_List;
       Deposits : Transaction_List;
     end record with
       Type_Invariant => Consistent_Balance (Account);
     function Consistent_Balance (This : Account) return Boolean;
     function Total (This : Transactions_List) return Currency;
   end Bank;

-------------------------------------------
Example Type Invariant Realization (Body)
-------------------------------------------

.. code:: Ada

   package body Bank is
     function Total (This : Transactions_List) return Currency is
       Result : Currency := 0.0;
     begin
       for Value of This loop -- no iteration if list empty
         Result := Result + Value;
       end loop;
       return Result;
     end Total;
   ...
     function Consistent_Balance (This : Account) return Boolean is
     begin
       return Total (This.Deposits) - Total (This.Withdrawals) =
              This.Current_Balance;
     end Consistent_Balance;
   end Bank;

-----------------------------------
Invariants Don't Apply Internally
-----------------------------------

* Within the package there is no checking

   - Otherwise there would be no way to implement anything!

* Only matters when clients can observe state

.. code:: Ada

   procedure Open (This            : in out Account;
                   Name            : in String;
                   Initial_Deposit : in Currency) is
   begin
     This.Owner := To_Unbounded_String (Name);
     This.Current_Balance := Initial_Deposit;
     -- invariant would be false here!
     This.Withdrawals := Transactions.Empty_List;
     This.Deposits := Transactions.Empty_List;
     This.Deposits.Append (Initial_Deposit);
     -- invariant is now true
   end Open;

--------------------------------------------
Default Type Initialization for Invariants
--------------------------------------------

* The initial, whole value must support the invariant
* Some types may need default type initialization to satisfy the requirement

.. code:: Ada

   package P is
     type T is private;
     procedure Op (This : in out T);
   private
     type T is new Integer
     with
       Default_Value  => 0,
       Type_Invariant => Zero (T);
   end P;

---------------------------------
Type Invariant Clause Placement
---------------------------------

* In SPARK, type invariant must be placed on the type completion
* In Ada, type invariant can be placed on the initial type declaration

   .. code:: Ada

      package P is
        type T is private
          with Type_Invariant => Zero (T),
        procedure Op (This : in out T);
        function Zero (This : T) return Boolean;
      private
        type T is new Integer
          with Default_Value => 0;
        function Zero (This : T) return Boolean is (This = 0);
      end P;

-------------------------------------
Invariants Are Not Foolproof in Ada
-------------------------------------

* Access to ADT representation via pointer allows back door manipulation
* These are private types, so access to internals must be granted by the private type's code
* Granting internal representation access for an ADT is a highly questionable design!
* This is not possible in SPARK (invariants *are* foolproof in SPARK)

========
Lab
========

.. include:: labs/070_type_contracts.lab.rst

==============
Summary
==============

-------------------------------
Type Invariants vs Predicates
-------------------------------

* Type Invariants are valid at external boundary

   - Useful for complex types, as the type may not be consistent during an operation

* Predicates are like other constraint checks

   - Checked on declaration, assignment, calls, etc

---------------------------------------
GNAT-Specific Aspect Name "Predicate"
---------------------------------------

* Conflates the two language-defined names
* Takes on kind with widest applicability possible

   - Static if possible, based on predicate content
   - Dynamic if cannot be static

* Remember that static predicates are allowed anywhere that dynamic predicates are allowed
* The slight disadvantage is that you don't find out if your static predicate is not actually static

   - Until you try to use it where only static predicates allowed
