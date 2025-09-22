========================
Conditional Statements
========================

-------------------------
If-then-else Statements
-------------------------

* Control flow using Boolean expressions
* Syntax

.. container:: source_include 040_statements/syntax.bnf :start-after:if_then_else_statements_begin :end-before:if_then_else_statements_end :code:bnf

* At least one statement must be supplied

    - :ada:`null` for explicit no-op

--------------------------
If-then-elsif Statements
--------------------------

* Sequential choice with alternatives
* Avoids :ada:`if` nesting
* :ada:`elsif` alternatives, tested in textual order
* :ada:`else` part still optional

.. container:: columns

 .. container:: column

   .. code:: Ada
      :number-lines: 1

      if Valve (N) /= Closed then
        Isolate (Valve (N));
        Failure (Valve (N));
      else
        if System = Off then
          Failure (Valve (N));
        end if;
      end if;

 .. container:: column

   .. code:: Ada
      :number-lines: 1

      if Valve (N) /= Closed then
        Isolate (Valve (N));
        Failure (Valve (N));
      elsif System = Off then
        Failure (Valve (N));
      end if;

.. container:: speakernote

   Spelled that way on purpose, as was done in Python for example (differently, "elif")

-----------------
Case Statements
-----------------

* Exclusionary choice among alternatives
* Syntax

.. container:: source_include 040_statements/syntax.bnf :start-after:case_statements_begin :end-before:case_statements_end :code:bnf

------------------------
Simple "case" Statements
------------------------

.. code:: Ada

   type Directions is  (Forward, Backward, Left, Right);
   Direction : Directions;
   ...
   case Direction is
     when Forward =>
       Set_Mode (Forward);
       Move (1);
     when Backward =>
       Set_Mode (Backup);
       Move (-1);
     when Left =>
       Turn (1);
     when Right =>
       Turn (-1);
   end case;

*Note*: No fall-through between cases

----------------------
Case Statement Rules
----------------------

* More constrained than a if-elsif structure
* **All** possible values must be covered

   - Explicitly
   - ... or with :ada:`others` keyword

* Choice values cannot be given more than once (exclusive)

    - Must be known at **compile** time

---------------------------
"When" Block Alternatives
---------------------------

* Single value: :ada:`when Tuesday =>`

   * Block is entered when :ada:`case` value is :ada:`Tuesday`

* Set of values: :ada:`when Saturday | Sunday =>`

   * Block is entered when :ada:`case` value is either :ada:`Saturday` or :ada:`Sunday`

* Range of values: :ada:`when Tuesday .. Thursday =>`

   * Block is entered when :ada:`case` value is between :ada:`Tuesday` and :ada:`Thursday` inclusive

------------------
"Others" Choice
------------------

* Choice by default

    - "everything not specified so far"

* Must be in last position

.. code:: Ada

   case Today is   -- work schedule
     when Monday =>
       Go_To (Work, Arrive=>Late, Leave=>Early);
     when Tuesday | Wednesday | Thursday =>
       Go_To (Work, Arrive=>Early, Leave=>Late);
     when Friday =>
       Go_To (Work, Arrive=>Early, Leave=>Early);
     when others => -- weekend
       Go_To (Home, Arrive=>Day_Before, Leave=>Day_After);
   end case;

------------------------------------
Dangers of "Others" Case Alternative
------------------------------------

* Maintenance issue: new value requiring a new alternative?

    - Compiler won't warn: :ada:`others` hides it

.. code:: Ada

   type Agencies_T is (NASA, ESA, RFSA); -- could easily grow
   Bureau : Agencies_T;
   ...
   case Bureau is
     when ESA =>
        Set_Region (Europe);
     when NASA =>
        Set_Region (America);
     when others =>
        Set_Region (Russia); -- New agencies will be Russian!
   end case;

------
Quiz
------

.. code:: Ada

   A : Integer := 100;
   B : Integer := 200;

Which choice needs to be modified to make a valid :ada:`if` block

A. | :answermono:`if A == B and then A != 0 then`
   |    :answermono:`A := Integer'First;`
   |    :answermono:`B := Integer'Last;`
B. | ``elsif A < B then``
   |    ``A := B + 1;``
C. | ``elsif A > B then``
   |    ``B := A - 1;``
D. | ``end if;``

.. container:: animate

   Explanations

   * :ada:`A` uses the C-style equality/inequality operators
   * :ada:`D` is legal because :ada:`else` is not required for an :ada:`if` block

------
Quiz
------

.. code:: Ada

   type Enum_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   A : Enum_T;

Which choice needs to be modified to make a valid :ada:`case` block

.. code:: Ada

   case A is

A. | ``when Sun =>``
   |    ``Put_Line ("Day Off");``
B. | ``when Mon | Fri =>``
   |    ``Put_Line ("Short Day");``
C. | ``when Tue .. Thu =>``
   |    ``Put_Line ("Long Day");``
D. | :answermono:`end case;`

.. container:: animate

   Explanations

   * Ada requires all possibilities to be covered
   * Add :ada:`when others` or :ada:`when Sat`

