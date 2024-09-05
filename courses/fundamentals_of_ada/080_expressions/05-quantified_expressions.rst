========================
Quantified Expressions
========================

-------------
Introduction
-------------

* Expressions that have a Boolean value
* The value indicates something about a set of objects

   - In particular, whether something is True about that set

* That "something" is expressed as an arbitrary boolean expression

   - A so-called "predicate"

* "Universal" quantified expressions

   - Indicate whether predicate holds for all components

* "Existential" quantified expressions

   - Indicate whether predicate holds for at least one component

..
  language_version 2012

----------
Examples
----------

.. code:: ada

   with GNAT.Random_Numbers; use GNAT.Random_Numbers;
   with Ada.Text_IO;         use Ada.Text_IO;
   procedure Quantified_Expressions is
      Gen    : Generator;
      Values : constant array (1 .. 10) of Integer := (others => Random (Gen));
   
      Any_Even : constant Boolean := (for some N of Values => N mod 2 = 0);
      All_Odd  : constant Boolean := (for all N of reverse Values => N mod 2 = 1);
   
      function Is_Sorted return Boolean is
        (for all K in Values'Range =>
           K = Values'First or else Values (K - 1) <= Values (K));
   
      function Duplicate return Boolean is
        (for some I in Values'Range =>
           (for some J in I + 1 .. Values'Last => Values (I) = Values (J)));
   
   begin
      Put_Line ("Any Even: " & Boolean'Image (Any_Even));
      Put_Line ("All Odd: " & Boolean'Image (All_Odd));
      Put_Line ("Is_Sorted " & Boolean'Image (Is_Sorted));
      Put_Line ("Duplicate " & Boolean'Image (Duplicate));
   end Quantified_Expressions;
.. include:: ../examples/080_expressions/quantified_expressions.rst

-----------------------------------------
Semantics Are As If You Wrote This Code
-----------------------------------------

.. code:: Ada

   function Universal (Set : Components) return Boolean is
   begin
     for C of Set loop
       if not Predicate (C) then
         return False;  -- Predicate must be true for all
       end if;
     end loop;
     return True;
   end Universal;

   function Existential (Set : Components) return Boolean is
   begin
     for C of Set loop
       if Predicate (C) then
         return True;  -- Predicate need only be true for one
       end if;
     end loop;
     return False;
   end Existential;

..
  language_version 2012

-------------------------------
Quantified Expressions Syntax
-------------------------------

* Four :ada:`for` variants

    - Index-based :ada:`in` or component-based :ada:`of`
    - Existential :ada:`some` or universal :ada:`all`

* Using arrow ``=>`` to indicate :dfn:`predicate` expression

.. code:: Ada

    (for some Index in Subtype_T => Predicate (Index))
    (for all Index in Subtype_T => Predicate (Index))
    (for some Value of Container_Obj => Predicate (Value))
    (for all Value of Container_Obj => Predicate (Value))

..
  language_version 2012

-----------------
Simple Examples
-----------------

.. code:: Ada

   Values : constant array (1 .. 10) of Integer := (...);
   Is_Any_Even : constant Boolean :=
      (for some V of Values => V mod 2 = 0);
   Are_All_Even : constant Boolean :=
      (for all V of Values => V mod 2 = 0);

..
  language_version 2012

----------------------
Universal Quantifier
----------------------

* In logic, denoted by |forall| (inverted 'A', for "all")
* "There is no member of the set for which the predicate does not hold"

   - If predicate is False for any member, the whole is False

* Functional equivalent

   .. code:: Ada

      function Universal (Set : Components) return Boolean is
      begin
        for C of Set loop
          if not Predicate (C) then
             return False; -- Predicate must be true for all
          end if;
        end loop;
        return True;
      end Universal;

..
  language_version 2012

-----------------------------------
Universal Quantifier Illustration
-----------------------------------

* "There is no member of the set for which the predicate does not hold"
* Given a set of integer answers to a quiz, there are no answers that are not 42 (i.e., all are 42)

.. code:: Ada

   Ultimate_Answer : constant := 42; -- to everything...
   Answers : constant array (1 .. 10)
       of Integer := (...);
   All_Correct_1 : constant Boolean :=
      (for all Component of Answers =>
         Component = Ultimate_Answer);
   All_Correct_2 : constant Boolean :=
      (for all K in Answers'Range =>
         Answers (K) = Ultimate_Answer);

..
  language_version 2012

-----------------------------------------
Universal Quantifier Real-World Example
-----------------------------------------

.. code:: Ada

   type DMA_Status_Flag is (...);
   function Status_Indicated (
     Flag : DMA_Status_Flag)
     return Boolean;
   None_Set : constant Boolean := (
     for all Flag in DMA_Status_Flag =>
       not Status_Indicated (Flag));

..
  language_version 2012

------------------------
Existential Quantifier
------------------------

* In logic, denoted by |exists| (rotated 'E', for "exists")
* "There is at least one member of the set for which the predicate holds"

   - If predicate is True for any member, the whole is True

* Functional equivalent

   .. code:: Ada

      function Existential (Set : Components) return Boolean is
      begin
        for C of Set loop
          if Predicate (C) then
            return True; -- Need only be true for at least one
          end if;
        end loop;
        return False;
      end Existential;

..
  language_version 2012

-------------------------------------
Existential Quantifier Illustration
-------------------------------------

* "There is at least one member of the set for which the predicate holds"
* Given set of Integer answers to a quiz, there is at least one answer that is 42

.. code:: Ada

   Ultimate_Answer : constant := 42; -- to everything...
   Answers : constant array (1 .. 10)
       of Integer := (...);
   Any_Correct_1 : constant Boolean :=
      (for some Component of Answers =>
         Component = Ultimate_Answer);
   Any_Correct_2 : constant Boolean :=
      (for some K in Answers'Range =>
         Answers (K) = Ultimate_Answer);

..
  language_version 2012

-----------------------------------------
Index-Based Vs Component-Based Indexing
-----------------------------------------

* Given an array of Integers

   .. code:: Ada

      Values : constant array (1 .. 10) of Integer := (...);

* Component-based indexing is useful for checking individual values

   .. code:: Ada

      Contains_Negative_Number : constant Boolean :=
         (for some N of Values => N < 0);

* Index-based indexing is useful for comparing across values

   .. code:: Ada

      Is_Sorted : constant Boolean :=
        (for all I in Values'Range =>
          I = Values'First or else
          Values (I) >= Values (I-1));

..
  language_version 2012

---------------------------------------
"Pop Quiz" for Quantified Expressions
---------------------------------------

.. container:: latex_environment small

 * What will be the value of `Ascending_Order`?

   .. code:: Ada

      Table : constant array (1 .. 10) of Integer :=
            (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
      Ascending_Order : constant Boolean := (
        for all K in Table'Range =>
          K > Table'First and then Table (K - 1) <= Table (K));

   - Answer: **False**. Predicate fails when `K = Table'First`

      + First subcondition is False!
      + Condition should be

         .. code:: Ada

          Ascending_Order : constant Boolean := (
             for all K in Table'Range =>
                K = Table'First or else Table (K - 1) <= Table (K));

..
  language_version 2012

---------------------------
 When the Set Is Empty...
---------------------------

* Universally quantified expressions are True

   - Definition: there is no member of the set for which the predicate does not hold
   - If the set is empty, there is no such member, so True
   - "All people 12-feet tall will be given free chocolate."

* Existentially quantified expressions are False

   - Definition: there is at least one member of the set for which the predicate holds

* If the set is empty, there is no such member, so False
* Common convention in set theory, arbitrary but settled

..
  language_version 2012

-----------------------------------------
Not Just Arrays: Any "Iterable" Objects
-----------------------------------------

* Those that can be iterated over
* Language-defined, such as the containers
* User-defined too

.. code:: Ada

   package Characters is new
      Ada.Containers.Vectors (Positive, Character);
   use Characters;
   Alphabet  : constant Vector :=
               To_Vector ('A',1) & 'B' & 'C';
   Any_Zed   : constant Boolean :=
               (for some C of Alphabet => C = 'Z');
   All_Lower : constant Boolean :=
               (for all C of Alphabet => Is_Lower (C));

..
  language_version 2012

-------------------------------------------
Conditional / Quantified Expression Usage
-------------------------------------------

* Use them when a function would be too heavy
* Don't over-use them!

   .. code:: Ada

      if (for some Component of Answers =>
          Component = Ultimate_Answer)
      then

* Function names enhance readability

   - So put the quantified expression in a function

      .. code:: Ada

         if At_Least_One_Answered (Answers) then

* Even in pre/postconditions, use functions containing quantified expressions for abstraction

..
  language_version 2012

------
Quiz
------

.. include:: ../quiz/quantified_expr_syntax/quiz.rst

------
Quiz
------

.. include:: ../quiz/quantified_expr_equality/quiz.rst

------
Quiz
------

.. code:: Ada

   type Array1_T is array (1 .. 3) of Integer;
   type Array2_T is array (1 .. 3) of Array1_T;
   A : Array2_T;

The above describes an array A whose elements are arrays of three elements.
Which expression would one use to determine if at least one of A's elements are sorted?

A. | ``(for some El of A => (for some Idx in 2 .. 3 =>``
   |     ``El (Idx) >= El (Idx - 1)));``
B. | ``(for all El of A => for all Idx in 2 .. 3 =>``
   |     ``El (Idx) >= El (Idx - 1)));``
C. | :answermono:`(for some El of A => (for all Idx in 2 .. 3 =>`
   |     :answermono:`El (Idx) >= El (Idx - 1)));`
D. | ``(for all El of A => (for some Idx in 2 .. 3 =>``
   |      ``El (Idx) >= El (Idx - 1)));``

.. container:: animate

 A. Will be :ada:`True` if any element has two consecutive increasing values
 B. Will be :ada:`True` if every element is sorted
 C. Correct
 D. Will be :ada:`True` if every element has two consecutive increasing values

