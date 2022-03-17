
************************
Quantified Expressions
************************
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`

==============
Introduction
==============

----------------------------------
What Are Quantified Expressions?
----------------------------------

* Expressions that have a Boolean value
* The value indicates something about a set of objects

   - In particular, whether something is True about that set

* That "something" is expressed as an arbitrary boolean expression

   - A so-called "predicate"

* "Universal" quantified expressions

   - Indicate whether predicate holds for all components

* "Existential" quantified expressions

   - Indicate whether predicate holds for at least one component

-------------------------
Loop Iteration Controls
-------------------------

* *Component-based* form is convenient

   - Provides the components' values directly
   - Hides iteration controls

* *Index-based* form is expressive and flexible

   - Iteration controls are explicit, thus available to the predicate expression
   - Component values can be referenced via the indexes

* Most appropriate form depends on situation

   - If predicate expression just requires component values alone, use the more convenient form

* Iteration controls can be used in `for` loops as well as quantified expressions

------------------------------------
Component-Based Iteration Controls
------------------------------------

- Work in terms of components within some object
- Syntax hides indexing / iterator encoding

   .. code:: Ada

      for name of [reverse] object loop
         ...
      end loop;

- Object is an array or iterable (formal) container
- Starts with first element unless you reverse it

.. code:: Ada

   Values : constant array (1 .. 10) of Boolean := (...);

   for V of Values loop
      Put_Line ( Boolean'Image ( V ) );
   end loop;

------------------------------------
Index-Based Iteration Controls
------------------------------------

- Declares an object that takes on successive values of a discrete type
- Syntax indicates range of discrete indices

   .. code:: Ada

      for name in [reverse] range_def loop
         ...
      end loop;

- Starts with first discrete value you specify unless you reverse it

.. code:: Ada

   Values : constant array (1 .. 10) of Boolean := (...);

   for I in Values'Range loop
      Put_Line ( Integer'Image(I) & " => " &
                 Boolean'Image ( Values(I) ) );
   end loop;

========================
Quantified Expressions
========================

-----------------------------------------
Semantics: As If You Wrote This Spec...
-----------------------------------------

.. code:: Ada

   package Quantified_Expressions is
     type Set_Member is ...;
     type Set is array (Positive range <>) of Set_Member;
     function Predicate (Member : Set_Member) return Boolean;
     function Universal (Collection : Set) return Boolean;
     -- True if Predicate is True for all members of Collection
     function Existential (Collection : Set) return Boolean;
     -- True if Predicate is True for any member of Collection
   end Quantified_Expressions;

-----------------------------
...With This Implementation
-----------------------------

.. code:: Ada

   package body Quantified_Expressions is
      function Universal (Collection : Set) return Boolean is
      begin
         for Member of Collection loop
            if not Predicate (Member) then
               -- Predicate must be true for all
               return False;
            end if;
         end loop;
         return True;
      end Universal;

      function Existential (Collection : Set) return Boolean is
      begin
         for Member of Collection loop
            if Predicate (Member) then
               -- Predicate need be true for at least one
               return True;
            end if;
         end loop;
         return False;
      end Existential;
   end Quantified_Expressions;

-------------------------------
Quantified Expressions Syntax
-------------------------------

.. code:: Ada

   quantified_expression ::=
       (for quantifier in range_specification => predicate)
     | (for quantifier of array_expression => predicate)

   predicate ::= boolean_expression

   quantifier ::= all | some

.. container:: speakernote

   This is not comprehensive

----------------------
Universal Quantifier
----------------------

* In logic, denoted by |forall| (inverted 'A', for "all")
* "There is no member of the set for which the predicate does not hold"

   - If predicate is False for any element, the whole is False

* Given a set of answers to a quiz, there are no answers that are not 42 (i.e., all are 42)

.. code:: Ada

   Ultimate_Answer : constant := 42;
   Answers : constant array (1 .. 10) of Integer := ( ... );

   All_Correct_1 : constant Boolean :=
      (for all Component of Answers =>
          Component = Ultimate_Answer);
   All_Correct_2 : constant Boolean :=
      (for all K in Answers'Range =>
          Answers(K) = Ultimate_Answer);

.. container:: speakernote

   Each one will "return" True
   You'd have to use the lower-level indexing syntax if you didn't want to check the entire array or collection.

------------------------
Existential Quantifier
------------------------

* In logic, denoted by |exists| (rotated 'E', for "exists")
* "There is at least one member of the set for which the predicate holds"

   - If predicate is True for any element, the whole is True

* Given a set of answers to a quiz, there is at least one answer that is 42

.. code:: Ada

   Ultimate_Answer : constant := 42;
   Answers : constant array (1 .. 10) of Integer := ( ... );

   Any_Correct_1 : constant Boolean :=
      (for some Component of Answers =>
          Component = Ultimate_Answer);
   Any_Correct_2 : constant Boolean :=
      (for some K in Answers'Range =>
          Answers(K) = Ultimate_Answer);

.. container:: speakernote

   Each one will "return" True
   You'd have to use the lower-level indexing syntax if you didn't want to check the entire array or collection.

-------------------------------------
Why Index-Based Iteration Controls?
-------------------------------------

* Needed when expression requires more than the component value alone

   - E.g., when predicate must refer to the indexes

      .. code:: Ada

         Table : constant array (1 .. 10) of Integer := (...);
         Ascending_Order : constant Boolean :=
            (for all K in Table'Range =>
               K = Table'First or else Table (K - 1) <= Table (K));

   - E.g., when precise control over range required

      .. code:: Ada

         Answers : constant array (1 .. 10) of Integer := (...);
         Any_First_Half_Answer : constant Boolean :=
            (for some K in 1 .. 5 => Answers(K) = 42);

.. container:: speakernote

   Note we could not use "K `>` Table'First and then Table (K - 1) `<=` Table (K)" because we are using the universal quantifier and at the first index value the predicate would be false.

--------------------------
When The Set Is Empty...
--------------------------

* Universally quantified expressions are True

   - Definition: there is no member of the set for which the predicate does not hold
   - If the set is empty there is no such member, so True

      + "All people 12-feet tall will be given free chocolate."

* Existentially quantified expressions are False

   - Definition: there is at least one member of the set for which the predicate holds
   - If the set is empty there is no such member, so False

* An established convention in logic and set theory

=========
Summary
=========

---------------------------------------
"Pop Quiz" for Quantified Expressions
---------------------------------------

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
             for all K in Table'Range => K = Table'first or else
                                         Table (K - 1) <= Table (K));
