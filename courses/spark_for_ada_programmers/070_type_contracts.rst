
*************************
Type-Oriented Contracts
*************************
.. |rightarrow| replace:: :math:`\rightarrow`

==============
Introduction
==============

-------------------------
Type-Oriented Contracts
-------------------------

* Ada defines support for various kinds of constraints

   - Range constraints, index constraints, etc.

* The language defines the rules for these constraints

   - All range constraints are continuous, etc.

* **Subtype predicates** generalize possibilities

   - You can define new constraints
   - You can define constraints not otherwise expressible!

* **Type invariants** ensure properties of ADT objects

============
Predicates
============

---------------------
What is a Predicate?
---------------------

* Something asserted to be true about some subject

   - When true, said to "hold"

   - Thus boolean expressions

* Can be specified for types, thus values of the types
* **Static Predicates**

   - Content can be static expressions but not required to be
   - Expected to hold all the time, like language-defined constraints (and with same caveats)

* **Dynamic Predicates**

   - Not required to be static expressions
   - Also expected to hold all the time (but not fully enforced by the compiler)

.. container:: speakernote

   Caveats include use of invalid values, e.g., via uninitialized variables or unchecked conversion.

-----------------------------------------
Really, `type` and `subtype` Predicates
-----------------------------------------

* Applicable to both
* Applied via aspect clauses in both cases
* `aspect_mark` can be either `Dynamic_Predicate` or `Static_Predicate`

.. code:: Ada

   type name is type_definition
      with aspect_mark [ => expression]
        {, aspect_mark [ => expression] }

   subtype defining_identifier is subtype_indication
      with aspect_mark [ => expression]
        {, aspect_mark [ => expression] }

-------------------------------------
Otherwise Inexpressible Constraints
-------------------------------------

.. code:: Ada

   subtype Even is Integer
      with Dynamic_Predicate => Even mod 2 = 0;

   type Serial_Baud_Rate is range 110 .. 115200
      with Static_Predicate =>
         Serial_Baud_Rate in
            110 | 300 | 600 | 1200 | 2400 | 4800 | 9600 |
            14400 | 19200 | 28800 | 38400 | 56000 |
            57600 | 115200;

.. container:: speakernote

   Discontinuous ranges cannot be expressed with range constraints.

-------------------------
New Kinds of Constraints
-------------------------

.. code:: Ada

   type Prime is new Positive with
      Dynamic_Predicate =>
         (for all Divisor in 2 .. Prime / 2 =>
            Prime mod Divisor /= 0);

   type Bundle is record
      X, Y : Integer;
      CRC  : Unsigned_32;
   end record with
      Dynamic_Predicate => CRC = Math.CRC32 (X, Y);

   type Table is array (M .. N) of Integer with
      Dynamic_Predicate =>
         (for all K in Table'Range =>
            (K = Table'First or else Table(K-1) <= Table(K)));

.. container:: speakernote

   Prime will always be a prime number
   Bundle will always have a valid CRC
   Table will always be sorted

------------------------------------
Discriminant In A Range Constraint
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

* Static predicates can be used in more contexts

   - But with restrictions on content to make that possible

* Dynamic predicates have more expressive power

   - Fewer restrictions on expression content
   - But cannot be used in all possible contexts

* Static predicates are allowed anywhere that dynamic predicates are allowed

   - But not the reverse

------------------------------------------
Predicate Run-Time Checking (If Enabled)
------------------------------------------

* Inserted automatically by compiler

   - Where a value would be assigned that may violate the constraint of the predicated target

* Violations raise exception `Assertion_Error`
* Performed before value change, like language-defined constraint checks

   - Associated variable is therefore unchanged

.. code:: Ada

   type Prime is new Positive with
      Dynamic_Predicate =>
         (for all Divisor in 2 .. Prime / 2 =>
             Prime mod Divisor /= 0);

   P : constant Prime := 6; -- assertion error

-----------------------------------
The "Static" In Static Predicates
-----------------------------------

* Allows more than ordinary static expressions that are calculable at compile-time
* Does not imply compile-time verification

   - Violations raise `Assertion_Error`

.. code:: Ada

   type Serial_Baud_Rate is range 110 .. 115200
      with Static_Predicate =>
         Serial_Baud_Rate  in
            110 | 300 | 600 | 1200 | 2400 | 4800 | 9600 |
            14400 | 19200 | 28800 | 38400 | 56000 |
            57600 | 115200;
   Rate : Serial_Baud_Rate :=
          Ada.Calendar.Day (Clock) * 100;

.. container:: speakernote

   Works 4 days of each month - Assertion Error on the others

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

--------------------------------------
Dynamic Predicate Expression Content
--------------------------------------

* Any arbitrary boolean expression

   - Hence all allowed static predicates' content

* Plus additional operators, etc.

   .. code:: Ada

      subtype Even is Integer
         with Dynamic_Predicate => Even mod 2 = 0;
      subtype Vowel is Character with Dynamic_Predicate =>
           (case Vowel is
               when 'A' | 'E' | 'I' | 'O' | 'U' => True,
               when others => False);

* Plus calls to functions

   - User-defined
   - Language-defined

.. container:: speakernote

   The predicate on type Vowels could also be a Static Predicate.

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

* Where gaps in values would be problematic

   - A choice in a named array aggregate (is it overlapping, etc?)
   - An array index or slice specification usage

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

* **NOTE** See Annotated Ada RM 3.2.4/25

.. container:: speakernote

   An index subtype, discrete range of an index constraint or slice
   A discrete subtype definition of a constrained array definition
   A discrete subtype definition of an entry declaration or entry index specification
   The discrete choice of a named array aggregate

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

-------------------------------------------
Lack of Initial Values Can Be Problematic
-------------------------------------------

* Users might not initialize when declaring objects
* Most types do not define automatic initialization

   - No language guarantee of any specific value (random bits)

   .. code:: Ada

         subtype Even is Integer
            with Dynamic_Predicate => Even mod 2 = 0;
         K : Even;  -- unknown initial value

* Since value is otherwise undefined the predicate is not checked when no initial value is given
* In Ada one can reference such junk values
* SPARK prevents reading an uninitialized variable

----------------------------
References Are Not Checked
----------------------------

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
     Put_Line ("K is" & Integer'Image (K));
     Put_Line ("J is" & Integer'Image (j));
   end Test;

.. code:: console

   K is 1969492223
   J is 4220029
   raised SYSTEM.ASSERTIONS.ASSERT_FAILURE :
      Dynamic_Predicate failed at test.adb:9

----------------------------------------
Predicate Checking In Ada versus SPARK
----------------------------------------

* :toolname:`GNATprove` verifies predicates are always satisfied
* In Ada, not every situation is checked

.. code:: Ada

   procedure Demo is
      type Table is array (1 .. 5) of Integer
          with Dynamic_Predicate =>
             (for all K in Table'Range =>
               (K = Table'First or else
                Table(K-1) <= Table(K)));
      Values : Table := (1,3,5,7,9);
   begin
      ...
      Values (3) := 0;
      ...
      Values := (1, 3, 0, 7, 9);
      ...
   end Demo;

.. container:: speakernote

   element assignment - no exception
   object assignment - exception

--------------------------------
Beware Recursion In Predicates
--------------------------------

* Necessarily involves functions because predicates are expressions
* Caused by checks on function arguments
* Predicate is checked recursively!

   .. code:: Ada

      type Table is array (1 .. N) of Integer
         with Dynamic_Predicate => Sorted (Table);
      function Sorted (T : Table) return Boolean is
         (for all K in T'Range =>
            (K = T'First or else T(K-1) <= T(K)));

* Not recursive

   .. code:: Ada

      type Table is array (1 .. N) of Integer
         with Dynamic_Predicate =>
            (for all K in Table'Range =>
               (K = Table'First or else Table(K-1) <= Table(K)));

----------------------------------------
"Safe" Functions In Subtype Predicates
----------------------------------------

.. container:: columns

 .. container:: column

    * Those that will not recurse...
    * No formal parameter of the type with the predicate being checked

 .. container:: column

    .. code:: Ada

       type Foo is
          record
             A : Integer;
             B : Float;
          end record
          with Dynamic_Predicate =>
             Bar (Foo.A) and
             Baz (Foo.B);
       function Bar (This : Integer)
          return Boolean is (...);
       function Baz (This : Float)
          return Boolean is (...);

.. container:: speakernote

   Do not use type foo for dynamic predicate functions

=================
Type Invariants
=================

-----------------
Type Invariants
-----------------

* There may be conditions that must hold over the entire lifetime of objects

   - Pre/postconditions apply only to subprogram calls

* Sometimes low-level facilities can express it

   .. code:: Ada

      subtype Weekdays is Days range Mon .. Fri;

      -- Guaranteed, absent unchecked conversion
      Workday : Weekdays := Mon;

* Type invariants apply across entire lifetime for complex abstract data types
* Part of ADT concept, hence only for private types

------------------------------
Type Invariant Verifications
------------------------------

.. container:: columns

 .. container:: column

    * Automatically inserted by compiler
    * Evaluated as a postcondition of any operation that creates, evaluates or returns a value of the type

       - Creation of objects
       - Assignment by clients
       - Type conversions (as that creates new instances)

    * Not evaluated on internal state changes

       - Internal routine calls

       - Internal assignments
       - Remember these are abstract data types

 .. container:: column

    .. image:: black_box_flow.png
       :width: 100%

----------------------------------------
Invariant Over Object Lifetime (Calls)
----------------------------------------

.. image:: type_invariant_check_flow.png

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
