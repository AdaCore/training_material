
****************
Type Contracts
****************

.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

---------------
Strong Typing
---------------

* We know Ada supports strong typing

   .. code:: Ada

      type Small_Integer_T is range -1_000 .. 1_000;
      type Enumerated_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      type Array_T is array (1 .. 3) of Boolean;

* But what if we need stronger enforcement?

   * Number must be even
   * Subet of non-consecutive enumerals
   * Array should always be sorted

* **Type Invariant**

   * Property of type that is always true on external reference
   * *Guarantee* to client, similar to subprogram postcondition

* **Subtype Predicate**

   * Add more complicated constraints to a type
   * Always enforced, just like other constraints

=================
Type Invariants
=================

-----------------
Type Invariants
-----------------

* There may be conditions that must hold over entire lifetime of objects

   - Pre/postconditions apply only to subprogram calls

* Sometimes low-level facilities can express it

   .. code:: Ada

      subtype Weekdays is Days range Mon .. Fri;

      -- Guaranteed (absent unchecked conversion)
      Workday : Weekdays := Mon;
 
* Type invariants apply across entire lifetime for complex abstract data types
* Part of ADT concept, so only for private types

------------------------------
Type Invariant Verifications
------------------------------

* Automatically inserted by compiler
* Evaluated as postcondition of creation, evaluation, or return object

   - When objects first created
   - Assignment by clients
   - Type conversions

      * Creates new instances

* Not evaluated on internal state changes

   - Internal routine calls 
   - Internal assignments

* Remember - these are abstract data types

.. image:: ../../images/black_box_flow.png
    
----------------------------------------
Invariant Over Object Lifetime (Calls)
----------------------------------------

.. image:: ../../images/type_invariant_check_flow.png

.. container:: speakernote

   Note that other actions also invoke the checks!

------------------------
Example Type Invariant
------------------------

* A bank account balance must always be consistent

   - Consistent Balance:  Total Deposits  - Total Withdrawals  =  Balance

.. code:: Ada

   package Bank is
     type Account is private with
       Type_Invariant => Consistent_Balance (Account);   
     ...
     -- Called automatically for all Account objects
     function Consistent_Balance (This : Account)
       return Boolean;
     ...
   private
     ...
   end Bank;
 
-------------------------------------------
Example Type Invariant Implementation
-------------------------------------------

.. code:: Ada

   package body Bank is
   ...
     function Total (This : Transaction_List)
         return Currency is
       Result : Currency := 0.0;
     begin
       for Value of This loop -- no iteration if list empty
         Result := Result + Value;
       end loop;
       return Result;
     end Total;
     function Consistent_Balance (This : Account)
         return Boolean is
     begin
       return Total (This.Deposits) - Total (This.Withdrawals)
              = This.Current_Balance;
     end Consistent_Balance;
   end Bank;
 
-----------------------------------
Invariants Don't Apply Internally
-----------------------------------

* No checking within supplier package

   - Otherwise there would be no way to implement anything!

* Only matters when clients can observe state

.. code:: Ada

   procedure Open (This : in out Account;
                   Name : in String;
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

* Invariant must hold for initial value
* May need default type initialization to satisfy requirement

.. code:: Ada

   package P is
     -- Type is private, so we can't use Default_Value here
     type T is private with Type_Invariant => Zero (T);
     procedure Op (This : in out T);
     function Zero (This : T) return Boolean;
   private
     -- Type is not a record, so we need to use aspect
     -- (A record could use default values for its components)
     type T is new Integer with Default_Value => 0; 
     function Zero (This : T) return Boolean is
     begin
        return (This = 0);
     end Zero;
   end P;
 
---------------------------------
Type Invariant Clause Placement
---------------------------------

* Can move aspect clause to private part

   .. code:: Ada

      package P is
        type T is private;
        procedure Op (This : in out T);
      private
        type T is new Integer with
          Type_Invariant => T = 0,
          Default_Value => 0;
      end P;
 
* It is really an implementation aspect

   * Client shouldn't care!

.. container:: speakernote

   Alternatively, declaring the 'Zero' predicate function and making it visible to clients will allow them to re-state the invariant for subclasses.
   That's useful because new, added primitive operations do not inherit the parent's type invariant.
   In other words the invariant isn't really inherited, it just comes for free with those primitives that are inherited (and not overridden).

------------------------------
Invariants Are Not Foolproof
------------------------------

* Access to ADT representation via pointer could allow back door manipulation
* These are private types, so access to internals must be granted by the private type's code
* Granting internal representation access for an ADT is a highly questionable design!

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment tiny

   .. code:: Ada

      package P is
         type Some_T is private;
         procedure Do_Something (X : in out Some_T);
      private
         function Counter (I : Integer) return Boolean;
      type Some_T is new Integer with
         Type_Invariant => Counter (Integer (Some_T));
      end P;

      package body P is
         function Local_Do_Something (X : Some_T)
                                      return Some_T is
            Z : Some_T := X + 1;
         begin
            return Z;
         end Local_Do_Something;
         procedure Do_Something (X : in out Some_T) is
         begin
            X := X + 1;
            X := Local_Do_Something (X);
         end Do_Something;
         function Counter (I : Integer)
                           return Boolean is
            ( True );
      end P;

 .. container:: column

    If `Do_Something` is called from outside of P, how many times is `Counter` called?

       A. 1
       B. :answer:`2`
       C. 3
       D. 4

    .. container:: animate

       Type Invariants are only evaluated on entry into and exit from
       externally visible subprograms. So :ada:`Counter` is called when
       entering and exiting :ada:`Do_Something` - not :ada:`Local_Do_Something`,
       even though a new instance of :ada:`Some_T` is created

====================
Subtype Predicates
====================

----------------------------
Subtype Predicates Concept
----------------------------

* Ada defines support for various kinds of constraints

   - Range constraints
   - Index constraints
   - Others...

* Language defines rules for these constraints

   - All range constraints are contiguous
   - Matter of efficiency

* **Subtype predicates** generalize possibilities

   - Define new kinds of constraints

----------------
**Predicates**
----------------

* Something asserted to be true about some subject

   - When true, said to "hold" 

* Expressed as any legal boolean expression in Ada

   - Quantified and conditional expressions
   - Boolean function calls

* Two forms in Ada

   - **Static Predicates**

      + Specified via aspect named `Static_Predicate`

   - **Dynamic Predicates**

      + Specified via aspect named `Dynamic_Predicate`

---------------------------------------------
Really, ``type`` and ``subtype`` Predicates
---------------------------------------------

* Applicable to both
* Applied via aspect clauses in both cases
* Syntax

   .. code:: Ada

      type name is type_definition
         with aspect_mark [ => expression] { ,
                   aspect_mark [ => expression] }
      subtype defining_identifier is subtype_indication
         with aspect_mark [ => expression] { ,
                   aspect_mark [ => expression] }
 
--------------------------
Why Two Predicate Forms?
--------------------------

 .. list-table::
   :header-rows: 1
   :stub-columns: 1
   :width: 90%
    
   * - 

     - Static
     - Dynamic

   * - Content

     - More Restricted
     - Less Restricted

   * - Placement

     - Less Restricted
     - More Restricted

* Static predicates can be used in more contexts

   - More restrictions on content
   - Can be used in places Dynamic Predicates cannot

* Dynamic predicates have more expressive power

   - Fewer restrictions on content
   - Not as widely available

----------------------------
Subtype Predicate Examples
----------------------------

* Dynamic Predicate

   .. code:: Ada

      subtype Even is Integer with Dynamic_Predicate =>
         Even mod 2 = 0; -- Boolean expression
         -- (Even indicates "current instance")
 
* Static Predicate

   .. code:: Ada

      type Serial_Baud_Rate is range 110 .. 115200
        with Static_Predicate => Serial_Baud_Rate  in
          -- Non-contiguous range
          110  | 300  | 600 | 1200 | 2400 | 4800 |
          9600 | 14400 | 19200 | 28800 | 38400 | 56000 |
          57600 | 115200;
 
--------------------
Predicate Checking
--------------------

* Calls inserted automatically by compiler
* Violations raise exception `Assertion_Error`

   - When predicate does not hold (evaluates to False)

* Checks are done before value change

   - Same as language-defined constraint checks

* Associated variable is unchanged when violation is detected

----------------------------
Predicate Checks Placement
----------------------------

* Anywhere value assigned that may violate target constraint
* Assignment statements
* Explicit initialization as part of object declaration
* Subtype conversion
* Parameter passing

   - All modes when passed by copy
   - Modes `in out` and `out` when passed by reference

* Implicit default initialization for record components
* On default type initialization values, when taken

----------------------------
References Are Not Checked
----------------------------

.. code:: Ada
    
   with Ada.Text_IO;   use Ada.Text_IO;
   procedure Test is   
     subtype Even is Integer with Dynamic_Predicate => Even mod 2 = 0;
     J, K : Even;
   begin
     -- predicates are not checked here
     Put_Line ("K is" & K'Img);
     Put_Line ("J is" & J'Img);
     -- predicate is checked here
     K := J; -- assertion failure here
     Put_Line ("K is" & K'Img);
     Put_Line ("J is" & J'Img);
   end Test;
     
* Output would look like
    
    .. code:: Ada
    
       K is 1969492223
       J is 4220029
       
       raised SYSTEM.ASSERTIONS.ASSERT_FAILURE:  
       Dynamic_Predicate failed at test.adb:9
     
------------------------------
Predicate Expression Content
------------------------------

* Reference to value of type itself, i.e., "current instance"

   .. code:: Ada

      subtype Even is Integer 
        with Dynamic_Predicate => Even mod 2 = 0;
      J, K : Even := 42;
 
* Any visible object or function in scope

   - Does not have to be defined before use
   - Relaxation of "declared before referenced" rule of linear elaboration
   - Intended especially for (expression) functions declared in same package spec

-------------------
Static Predicates
-------------------

* *Static* means known at compile-time, informally

   - Language defines meaning formally (RM 3.2.4)

* Allowed in contexts in which compiler must be able to verify properties
* Content restrictions on predicate are necessary

--------------------------------------
Allowed Static Predicate Content (1)
--------------------------------------

* Ordinary Ada static expressions
* Static membership test selected by current instance
* Example 1

   .. code:: Ada

      type Serial_Baud_Rate is range 110 .. 115200
        with Static_Predicate => Serial_Baud_Rate in
          -- Non-contiguous range
          110   | 300   | 600   | 1200  | 2400  | 4800  | 9600 |
          14400 | 19200 | 28800 | 38400 | 56000 | 57600 | 115200;
 
* Example 2

   .. code:: Ada

      type Days is (Sun, Mon, Tues, We, Thu, Fri, Sat);
       -- only way to create subtype of non-contiguous values
      subtype Weekend is Days
        with Static_Predicate => Weekend in Sat | Sun;
 
--------------------------------------
Allowed Static Predicate Content (2)
--------------------------------------

* Case expressions in which dependent expressions are static and selected by current instance

   .. code:: Ada

      type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      subtype Weekend is Days with Static_Predicate =>
        (case Weekend is
         when Sat | Sun => True,
         when Mon .. Fri => False);
 
* Note: if-expressions are disallowed, and not needed

   .. code:: Ada

      subtype Drudge is Days with Static_Predicate =>
        -- not legal
        (if Drudge in Mon .. Fri then True else False);
      -- should be
      subtype Drudge is Days with Static_Predicate =>
        Drudge in Mon .. Fri;
 
--------------------------------------
Allowed Static Predicate Content (3)
--------------------------------------

* A call to `=`, `/=`, `<`, `<=`, `>`, or `>=` where one operand is the current instance (and the other is static)
* Calls to operators `and`, `or`, `xor`, `not`

   - Only for pre-defined type `Boolean`
   - Only with operands of the above

* Short-circuit controls with operands of above

* Any of above in parentheses

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
         when others => False); -- evaluated at run-time
 
* Plus calls to functions

   - User-defined
   - Language-defined

-----------------------------
Types Controlling For-Loops
-----------------------------

* Types with dynamic predicates cannot be used

   - Too expensive to implement

      .. code:: Ada

         subtype Even is Integer 
           with Dynamic_Predicate => Even mod 2 = 0;
         ...
         -- not legal - how many iterations?
         for K in Even loop
           ...
         end loop;
 
* Types with static predicates can be used

   .. code:: Ada

      type Days is (Sun, Mon, Tues, We, Thu, Fri, Sat);   
      subtype Weekend is Days
        with Static_Predicate => Weekend in Sat | Sun;
      -- Loop uses "Days", and only enters loop when in Weekend
      -- So "Sun" is first value for K
      for K in Weekend loop
         ...
      end loop;
 
-----------------------------------------
Why Allow Types with Static Predicates?
-----------------------------------------

* Efficient code can be generated for usage
    
   .. code:: Ada
    
      type Days is (Sun, Mon, Tues, We, Thu, Fri, Sat);   
      subtype Weekend is Days with Static_Predicate => Weekend in Sat | Sun;
      ...
      for W in Weekend loop
        GNAT.IO.Put_Line (W'Img);
      end loop;
     
* `for` loop generates code like
    
   .. code:: Ada
    
      declare
        w : weekend := sun;
      begin
        loop
          gnat__io__put_line__2 (w'Img);
          case w is
            when sun =>
              w := sat;
            when sat =>
              exit;
            when others =>
              w := weekend'succ(w);
          end case;
        end loop;
      end;
     
---------------------------------------
In Some Cases Neither Kind Is Allowed
---------------------------------------

* No predicates can be used in cases where contiguous layout required

   - Efficient access and representation would be impossible

* Hence no array index or slice specification usage

.. code:: Ada

   type Play is array (Weekend) of Integer; -- illegal
   type List is array (Days range <>) of Integer;
   L : List (Weekend); -- not legal
 
-----------------------------------------
Special Attributes for Predicated Types
-----------------------------------------

* Attributes `'First_Valid` and `'Last_Valid`

   - Can be used for any static subtype 
   - Especially useful with static predicates
   - `'First_Valid` returns smallest valid value, taking any range or predicate into account
   - `'Last_Valid` returns largest valid value, taking any range or predicate into account

* Attributes `'Range`, `'First` and `'Last` are not allowed 

   - Reflect non-predicate constraints so not valid
   - `'Range` is just a shorthand for `'First` .. `'Last`

* `'Succ` and `'Pred` are allowed since work on underlying type

-----------------------------------
Initial Values Can Be Problematic
-----------------------------------

* Users might not initialize when declaring objects

   - Most predefined types do not define automatic initialization
   - No language guarantee of any specific value (random bits)
   - Example

      .. code:: Ada

         subtype Even is Integer 
           with Dynamic_Predicate => Even mod 2 = 0;
         K : Even;  -- unknown (invalid?) initial value
 
* The predicate is not checked on a declaration when no initial value is given
* So can reference such junk values before assigned

   - This is not illegal (but is a bounded error)

----------------------------------------
Subtype Predicates Aren't Bullet-Proof
----------------------------------------

* For composite types, predicate checks apply to whole object values, not individual components

.. code:: Ada

   procedure Demo is
     type Table is array (1 .. 5) of Integer
       -- array should always be sorted
       with Dynamic_Predicate =>
         (for all K in Table'Range =>
           (K = Table'First or else Table(K-1) <= Table(K)));
     Values : Table := (1, 3, 5, 7, 9);
   begin
     ...   
     Values (3) := 0; -- does not generate an exception!
     ...
     Values := (1, 3, 0, 7, 9); -- does generate an exception
     ...
   end Demo;
 
------------------------------------------
Beware Accidental Recursion In Predicate
------------------------------------------

* Involves functions because predicates are expressions
* Caused by checks on function arguments
* Infinitely recursive example

   .. code:: Ada

      type Sorted_Table is array (1 .. N) of Integer with
         Dynamic_Predicate => Sorted (Sorted_Table);
      -- on call, predicate is checked!
      function Sorted (T : Sorted_Table) return Boolean;

* Non-recursive example

   .. code:: Ada

      type Sorted_Table is array (1 .. N) of Integer with
         Dynamic_Predicate =>
         (for all K in Sorted_Table'Range =>
            (K = Sorted_Table'First
             or else Sorted_Table (K - 1) <= Sorted_Table (K)));
 
* Type-based example

   .. code:: Ada

      type Table is array (1 .. N) of Integer;
      subtype Sorted_Table is Table with
           Dynamic_Predicate => Sorted (Sorted_Table);
      function Sorted (T : Table) return Boolean;
 
---------------------------------------
GNAT-Specific Aspect Name *Predicate*
---------------------------------------

* Conflates two language-defined names
* Takes on kind with widest applicability possible

   - Static if possible, based on predicate expression content
   - Dynamic if cannot be static

* Remember: static predicates allowed anywhere that dynamic predicates allowed

   - But not inverse

* Slight disadvantage: you don't find out if your predicate is not actually static

   - Until you use it where only static predicates are allowed

------------------------------------------
Enabling/Disabling Contract Verification
------------------------------------------

* Corresponds to controlling specific run-time checks

   - Syntax

      .. code:: Ada

         pragma Assertion_Policy (policy_name);
         pragma Assertion_Policy (
            assertion_name => policy_name
            {, assertion_name => policy_name} );
 
* Vendors may define additional policies (GNAT does)
* Default, without pragma, is implementation-defined
* Vendors almost certainly offer compiler switch

   - GNAT uses same switch as for pragma Assert: ``-gnata``

.. container:: speakernote

   The simple form of Assertion Policy just applies the specified policy to all forms of assertion.
   Note that the Assert procedures in Ada.Assertions are not controlled by the pragma.  They are procedures like any other.
   A switch is likely offered because otherwise one must edit the source code to change settings, like the situation with pragma Inline.
   Pragma Suppress can also be applied.

------
Quiz
------

.. code:: Ada

   type Days_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   function Is_Weekday (D : Days_T) return Boolean is
      (D /= Sun and then D /= Sat);

Which of the following is a valid subtype predicate?

A. | :answermono:`subtype T is Days_T with`
   |    :answermono:`Static_Predicate => T in Sun | Sat;`
B. | ``subtype T is Days_T with Static_Predicate =>``
   |    ``(if T = Sun or else T = Sat then True else False);``
C. | ``subtype T is Days_T with``
   |    ``Static_Predicate => not Is_Weekday (T);``
D. | ``subtype T is Days_T with``
   |    ``Static_Predicate =>``
   |       ``case T is when Sat | Sun => True,``
   |                 ``when others => False;``

.. container:: animate

   Explanations

   A. Correct
   B. :ada:`If` statement not allowed in a predicate
   C. Function call not allowed in :ada:`Static_Predicate` (this would be OK for :ada:`Dynamic_Predicate`)
   D. Missing parentheses around :ada:`case` expression

=========
Summary
=========

------------------------------
Working with Type Invariants
------------------------------

* They are not fully foolproof

   - External corruption is possible 

   - Requires dubious usage

* Violations are intended to be supplier bugs

   - But not necessarily so, since not always bullet-proof

* However, reasonable designs will be foolproof

-------------------------------
Type Invariants vs Predicates
-------------------------------

* Type Invariants are valid at external boundary

   - Useful for complex types - type may not be consistent during an operation

* Predicates are like other constraint checks

   - Checked on declaration, assignment, calls, etc
