************************
Quantified Expressions
************************
.. |forall| replace:: :math:`\forall`
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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

----------------------------------
What Are Quantified Expressions?
----------------------------------

* Expressions that have a boolean value

* The value indicates whether a property is True about that set

* The property is expressed as an arbitrary boolean expression

* "Universal" quantified expressions

   - Indicate whether property holds for all components

* "Existential" quantified expressions

   - Indicate whether property holds for at least one component

-------------------------
Loop Iteration Controls
-------------------------

* Iteration controls can be used in `for` loops as well as quantified expressions

* *Component-based* form `for .. of` is convenient

   - Provides the components' values directly
   - Hides iteration controls

* *Index-based* form  `for .. in` is expressive and flexible

   - Iteration controls are explicit, thus available in expressions
   - Component values can be referenced via the indexes

* Most appropriate form depends on situation

   - If property or loop just requires component values alone, use the more convenient form

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

     Values : array (1 .. 10) of Boolean := (...);

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

     Values : array (1 .. 10) of Boolean := (...);

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
     function Property (Member : Set_Member) return Boolean;
     function Universal (Collection : Set) return Boolean;
     -- True if Property is True for all members of Collection
     function Existential (Collection : Set) return Boolean;
     -- True if Property is True for any member of Collection
   end Quantified_Expressions;

-----------------------------
...With This Implementation
-----------------------------

.. code:: Ada

   package body Quantified_Expressions is
      function Universal (Collection : Set) return Boolean is
      begin
         for Member of Collection loop
            if not Property (Member) then
               -- Property must be true for all
               return False;
            end if;
         end loop;
         return True;
      end Universal;

      function Existential (Collection : Set) return Boolean is
      begin
         for Member of Collection loop
            if Property (Member) then
               -- Property need be true for at least one
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
       (for quantifier name in range_specification => property)
     | (for quantifier name of array_expression => property)

   property ::= boolean_expression

   quantifier ::= all | some

.. container:: speakernote

   This is not comprehensive

----------------------
Universal Quantifier
----------------------

* In logic, denoted by |forall| (inverted 'A', for "all")
* "There is no member of the set for which the property does not hold"

   - If property is False for any element, the whole is False

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

* In logic, denoted by |exists| (inverted 'E', for "exists")
* "There is at least one member of the set for which the property holds"

   - If property is True for any element, the whole is True

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

   - E.g., when property must refer to the indexes

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

   Note we could not use "K `>` Table'First and then Table (K - 1) `<=` Table (K)" because we are using the universal quantifier and at the first index value the property would be false.

--------------------------
When The Set Is Empty...
--------------------------

* Universally quantified expressions are True

   - Definition: there is no member of the set for which the property does not hold
   - If the set is empty there is no such member, so True

      + "All people 12-feet tall will be given free chocolate."

* Existentially quantified expressions are False

   - Definition: there is at least one member of the set for which the property holds
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
      Ascending_Order : constant Boolean :=
        (for all K in Table'Range =>
          K > Table'First and then Table (K - 1) <= Table (K));

   - Answer: **False**. Property fails when `K = Table'First`

      + First subcondition is False!
      + Condition should be

         .. code:: Ada

          Ascending_Order : constant Boolean :=
            (for all K in Table'Range => K = Table'First or else
                                         Table (K - 1) <= Table (K));
