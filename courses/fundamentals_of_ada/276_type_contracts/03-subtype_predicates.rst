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

* Expressed as any legal Boolean expression in Ada

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
(Sub)Type Predicate Examples
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
* Violations raise exception :ada:`Assertion_Error`

   - When predicate does not hold (evaluates to False)

* Checks are done before value change

   - Same as language-defined constraint checks

   - Associated variable is unchanged when violation is detected

----------------------------
Predicate Checks Placement
----------------------------

* Anywhere value assigned that may violate target constraint
* Assignment statements
* Explicit initialization as part of object declaration
* Subtype conversion
* Parameter passing

   - All modes when passed by copy
   - Modes :ada:`in out` and :ada:`out` when passed by reference

* Implicit default initialization for record components
* On default type initialization values, when taken

----------------------------
References Are Not Checked
----------------------------

.. code:: Ada

   with Ada.Text_IO;   use Ada.Text_IO;
   procedure Even_Number_Test is
     subtype Even is Integer with Dynamic_Predicate => Even mod 2 = 0;
     Current_Value, Next_Value : Even;
   begin
     -- predicates are not checked here
     Put_Line ("Current_Value is" & Current_Value'Image);
     Put_Line ("Next_Value is" & Next_Value'Image);
     -- predicate is checked here
     Current_Value := Next_Value; -- assertion failure here
     Put_Line ("Current_Value is" & Current_Value'Image);
     Put_Line ("Next_Value is" & Next_Value'Image);
   end Even_Number_Test;

* Output would look like

    .. code:: Ada

       Current_Value is 1969492223
       Next_Value is 4220029

       raised SYSTEM.ASSERTIONS.ASSERT_FAILURE:
       Dynamic_Predicate failed at even_number_test.adb:9

------------------------------
Predicate Expression Content
------------------------------

* Reference to value of type itself, i.e., "current instance"

   .. code:: Ada

      subtype Even is Integer
        with Dynamic_Predicate => Even mod 2 = 0;
      Current_Value, Next_Value : Even := 42;

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

      type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
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
* Calls to operators :ada:`and`, :ada:`or`, :ada:`xor`, :ada:`not`

   - Only for pre-defined type `Boolean`
   - Only with operands of the above

* Short-circuit controls with operands of above
* Any of above in parentheses

--------------------------------------
Dynamic Predicate Expression Content
--------------------------------------

* Any arbitrary Boolean expression

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
         for A_Number in Even loop
           ...
         end loop;

* Types with static predicates can be used

   .. code:: Ada

      type Days is (Sun, Mon, Tues, Wed, Thu, Fri, Sat);
      subtype Weekend is Days
        with Static_Predicate => Weekend in Sat | Sun;
      -- Loop uses "Days", and only enters loop when in Weekend
      -- So "Sun" is first value for A_Day
      for A_Day in Weekend loop
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
      for A_Day in Weekend loop
        GNAT.IO.Put_Line (A_Day'Image);
      end loop;

* :ada:`for` loop generates code like

   .. code:: Ada

      declare
        a_day : weekend := sun;
      begin
        loop
          gnat__io__put_line__2 (a_day'Image);
          case a_day is
            when sun =>
              a_day := sat;
            when sat =>
              exit;
            when others =>
              a_day := weekend'succ (a_day);
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
   type Vector is array (Days range <>) of Integer;
   Not_Legal : Vector (Weekend); -- not legal

-----------------------------------------
Special Attributes for Predicated Types
-----------------------------------------

* Attributes `'First_Valid` and `'Last_Valid`

   - Can be used for any static subtype
   - Especially useful with static predicates
   - `'First_Valid` returns smallest valid value, taking any range or predicate into account
   - `'Last_Valid` returns largest valid value, taking any range or predicate into account

* Attributes :ada:`'Range`, `'First` and `'Last` are not allowed

   - Reflect non-predicate constraints so not valid
   - :ada:`'Range` is just a shorthand for `'First` .. `'Last`

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
         Some_Number : Even;  -- unknown (invalid?) initial value

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
         (for all Idx in Table'Range =>
           (Idx = Table'First or else Table (Idx-1) <= Table (Idx)));
     Values : Table := (1, 3, 5, 7, 9);
   begin
     ...
     Values (3) := 0; -- does not generate an exception!
     ...
     Values := (1, 3, 0, 7, 9); -- does generate an exception
     ...
   end Demo;

------------------------------------------
Beware Accidental Recursion in Predicate
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
         (for all Index in Sorted_Table'Range =>
            (Index = Sorted_Table'First
             or else Sorted_Table (Index - 1) <= Sorted_Table (Index)));

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
            {, assertion_name => policy_name});

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
   function Is_Weekday (Day : Days_T) return Boolean is
      (Day /= Sun and then Day /= Sat);

Which of the following is a valid subtype predicate?

A. | :answermono:`subtype Sub_Day is Days_T with`
   |    :answermono:`Static_Predicate => Sub_Day in Sun | Sat;`
B. | ``subtype Sub_Day is Days_T with Static_Predicate =>``
   |    ``(if Sub_Day = Sun or else Sub_Day = Sat then True else False);``
C. | ``subtype Sub_Day is Days_T with``
   |    ``Static_Predicate => not Is_Weekday (Sub_Day);``
D. | ``subtype Sub_Day is Days_T with``
   |    ``Static_Predicate =>``
   |       ``case Sub_Day is when Sat | Sun => True,``
   |                 ``when others => False;``

.. container:: animate

   Explanations

   A. Correct
   B. :ada:`If` statement not allowed in a predicate
   C. Function call not allowed in :ada:`Static_Predicate` (this would be OK for :ada:`Dynamic_Predicate`)
   D. Missing parentheses around :ada:`case` expression

