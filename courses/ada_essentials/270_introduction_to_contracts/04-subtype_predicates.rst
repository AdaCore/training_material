====================
Subtype Predicates
====================

----------
Examples
----------

.. include:: ../examples/adv_275_type_contracts/subtype_predicates.rst

----------------
**Predicates**
----------------

* Assertion expected to hold for all objects of given type
* Expressed as any legal boolean expression in Ada

   - Quantified and conditional expressions
   - Boolean function calls

* Two forms in Ada

   - **Static Predicates**

      + Specified via aspect named :ada:`Static_Predicate`

   - **Dynamic Predicates**

      + Specified via aspect named :ada:`Dynamic_Predicate`

* Can apply to :ada:`type` or :ada:`subtype`

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
* Violations raise exception :ada:`Assertion_Error`

   - When predicate does not hold (evaluates to False)

* Checks are done before value change

   - Same as language-defined constraint checks

* Associated variable is unchanged when violation is detected

------------------------------
Predicate Expression Content
------------------------------

* Reference to value of type itself, i.e., "current instance"

   .. code:: Ada

      subtype Even is Integer
        with Dynamic_Predicate => Even mod 2 = 0;
      Even_Num, Also_Even_Num : Even := 42;

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
* Ordinary Ada static expressions
* Static membership test selected by current instance

* Example

   .. code:: Ada

      type Serial_Baud_Rate is range 110 .. 115200
        with Static_Predicate => Serial_Baud_Rate in
          -- Non-contiguous range
          110   | 300   | 600   | 1200  | 2400  | 4800  | 9600 |
          14400 | 19200 | 28800 | 38400 | 56000 | 57600 | 115200;

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
      function Sorted (A_Table : Sorted_Table) return Boolean;

* Non-recursive example

   .. code:: Ada

      type Sorted_Table is array (1 .. N) of Integer with
         Dynamic_Predicate =>
         (for all Rows in Sorted_Table'Range =>
            (Row = Sorted_Table'First
             or else Sorted_Table (Row - 1) <= Sorted_Table (Row)));

* Type-based example

   .. code:: Ada

      type Table_Type is array (1 .. N) of Integer;
      subtype Sorted_Table is Table_Type with
           Dynamic_Predicate => Sorted (Sorted_Table);
      function Sorted (A_Table : Table_Type) return Boolean;

------
Quiz
------

.. code:: Ada

   type Days_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   function Is_Weekday (The_Day : Days_T) return Boolean is
      (The_Day /= Sun and then The_Day /= Sat);

Which of the following is a valid subtype predicate?

A. | :answermono:`subtype Wknd is Days_T with`
   |    :answermono:`Static_Predicate => Wknd in Sun | Sat;`
B. | ``subtype Wknd is Days_T with Static_Predicate =>``
   |    ``(if Wknd = Sun or else Wknd = Sat then True else False);``
C. | ``subtype Wknd is Days_T with``
   |    ``Static_Predicate => not Is_Weekday (Wknd);``
D. | ``subtype Wknd is Days_T with``
   |    ``Static_Predicate =>``
   |       ``case Wknd is when Sat | Sun => True,``
   |                 ``when others => False;``

.. container:: animate

   Explanations

   A. Correct
   B. :ada:`If` statement not allowed in a predicate
   C. Function call not allowed in :ada:`Static_Predicate` (this would be OK for :ada:`Dynamic_Predicate`)
   D. Missing parentheses around :ada:`case` expression

