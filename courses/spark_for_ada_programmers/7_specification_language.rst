************************
Specification Language
************************

..
    Coding language

.. role:: ada(code)
    :language: Ada

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

--------------------
Simple Expressions
--------------------

* Simple specifications use simple expressions

  - Arithmetic operations and comparisons
  - Membership tests :code:`X in A .. B`

    .. code:: ada

       I in T'Range

    is better than:

    .. code:: ada

       I >= T'First and I <= T'Last

  - Conjunctions and disjunctions

    + Lazy operators :code:`and then`/:code:`or else` preferred in general to
      :code:`and`/:code:`or`

* But that's not sufficient to easily write all specifications

  .. code:: ada

     procedure Init_Table (T : out Table)
     with
       Pre  => T'Length > 0,
       Post => -- if T is of length 1 ...
               -- else if T is of length 2 ...
               -- else for all elements ...

--------------------
Richer Expressions
--------------------

* Counterparts of conditional statements

  - *if expressions* are the counterpart of *if statements*
  - *case expressions* are the counterpart of *case statements*

* Expressions over a collection (range or array or...)

  - *universally quantified expression* for properties over all elements
  - *existentially quantified expression* for properties over one element

* New forms of aggregates

  - *delta aggregates* express the value of an updated composite object
  - *iterated component associations* express array aggregates where the
    expression depends on the index

* Structuring expressions

  - *declare expressions* introduce names for local constants
  - *expression functions* introduce names for common expressions

=========================
Conditional Expressions
=========================

----------------
If Expressions
----------------

* :code:`(if Cond then A else B)` evaluates :code:`A` or :code:`B` depending on
  the value of :code:`Cond`

  - Note: always in parentheses!
  - :code:`A` and :code:`B` must have the same type
  - ...not always Boolean!

    .. code:: Ada

       A := (if Cond then 2 else 3);

* Frequent use with Boolean type in specifications

  - :code:`(if Cond then Property)` is shortcut for :code:`(if Cond then
    Property else True)`
  - This expresses a logical implication :code:`Cond` |rightarrow| :code:`Property`
  - Also equivalent to :code:`not Cond or else Property`

* Complete form has `elsif` parts

------------------
Case Expressions
------------------

* Extension of *if expressions* to non-Boolean discrete types

  .. code:: ada

     (case Day is
        when Monday
           | Friday
           | Sunday    => 6,
        when Tuesday   => 7,
        when Thursday
           | Saturday  => 8,
        when Wednesday => 9)

* Same choice expressions as in *case statements*

  - Can also use :code:`others` as last alternative
  - Note: always in parentheses!
  - Note: cases are separated by commas

--------------
Set Notation
--------------

* Usable in both *case expressions* / *case statements* and in membership tests

* Without set notation:

  .. code:: Ada

     if X = 'A' or else X = 'B' or else X = 'C' then

* With set notation:

  .. code:: Ada

     if X in 'A' | 'B' | 'C' then

* Also allowed for opposite membership test: `if X not in ...`

========================
Quantified Expressions
========================

------------------
Range-based Form
------------------

* Based on the usual *for loop* syntax over a range

  .. code:: ada

     for J in T'Range loop
        T (J) := 0;
     end loop;

     pragma Assert (for all J in T'Range => T(J) = 0);

* Universally quantified expression :code:`(for all J in A .. B => Property)`

  - Express that property holds for all values in the range
  - True if the range is empty (:math:`\forall` in logic)
  - At runtime, executed as a loop which stops at first value where the
    property is not satisfied

* Existentially quantified expression :code:`(for some J in A .. B => Property)`

  - Express that property holds for at least one value in the range
  - False if the range is empty (:math:`\exists` in logic)
  - At runtime, executed as a loop which stops at first value where the
    property is satisfied

------------------
Array-based Form
------------------

* Based on the *for loop* syntax over an array

  .. code:: ada

     for E of T loop
        E := 0;
     end loop;

     pragma Assert (for all E of T => E = 0);

* Counterparts of range-based forms

  - Universally quantified expression :code:`(for all E of T => Property)`
  - Existentially quantified expression :code:`(for some E of T => Property)`

* Note: always in parentheses!

----------------------------------
Range-based vs Array-based Forms
----------------------------------

* Array-based form only possible if :code:`Property` does not refer to the
  index

* Example: array :code:`T` is sorted

  .. code:: ada

     (for all J in T'Range =>
       (if J /= T'First then T(J-1) <= T(J)))

  or (better for proof to avoid the need for induction)

  .. code:: ada

     (for all J in T'Range =>
       (for all K in T'Range =>
         (if J < K then T(J) <= T(K))))

-----------------------------
General Iteration Mechanism
-----------------------------

* Based on the :code:`Iterable` aspect on a type

  - Not the same as the standard Ada mechanism!
  - Simpler mechanism adopted for the SPARK formal containers

  .. code:: ada

     type Container is private with
       (First       => First,
        Next        => Next,
        Has_Element => Has_Element
        Element     => Element);

* :dfn:`Iteration over positions` uses :code:`for .. in` syntax

  - Uses cursor type with :code:`First`, :code:`Next` and :code:`Has_Element`
  - Function :code:`Element` is not required

* :dfn:`Iteration over elements` uses :code:`for .. of` syntax

  - Based on the previous iteration
  - Function :code:`Element` retrieves the element for a given cursor

----------------------------------
Iteration Over Formal Containers
----------------------------------

* Generic units compatible with SPARK

  - The API is slightly different from standard Ada containers
  - Available in the SPARK Library

* Available for all formal containers:

  - vectors
  - doubly linked lists
  - sets (hashed and ordered)
  - maps (hashed and ordered)

* Iteration over positions

  - Access to element through function :code:`Element`
  - For maps, access to key through function :code:`Key`

* Iteration over elements

  - For maps, really an iteration over keys

    - Use another function :code:`Element` to get element

-------------------------------
Iteration Over Formal Vectors
-------------------------------

* Only formal container to have 3 iteration mechanisms

* Range-based iteration (using :command:`-gnatX` for dot-notation)

  .. code:: ada

     for J in V.First_Index .. V.Last_Index loop
        V.Replace_Element (J, 0);
     end loop;
     pragma Assert
       (for all J in V.First_Index .. V.Last_Index => V.Element (J) = 0);

* Iteration over positions

  .. code:: ada

     for J in V loop
        V.Replace_Element (J, 0);
     end loop;
     pragma Assert (for all J in V => V.Element (J) = 0);

* Iteration over elements (no update!)

  .. code:: ada

     for E of V loop
        pragma Assert (E = 0);
     end loop;
     pragma Assert (for all E of V => E = 0);

===========================
New Aggregate Expressions
===========================

------------------
Delta Aggregates
------------------

.. admonition:: Language Variant

   Ada 2022

* Express the value of a modified composite object (record or array)

  .. code:: Ada

     (Rec with delta Comp1 => Val1, Comp2 => Val2)

     (Arr with delta 1 => True, 42 => False)

* Typically used to relate input and output values of parameters

  - Combines delta aggregate with use of attribute `'Old`

  .. code:: Ada

     procedure P (Rec : in out T)
       with Post => Rec = (Rec'Old with delta Comp1 => Val1,
                                              Comp2 => Val2);

* With array object:

  - Avoids the introduction of explicit quantifiers
  - Can have overlapping and dynamic choices (values or ranges)

---------------------------------
Iterated Component Associations
---------------------------------

.. admonition:: Language Variant

   Ada 2022

* Express the value of an array aggregate depending on index

* Example: the identity function

  .. code:: Ada

     (for J in T'Range => J)

* This is a :dfn:`component association`

  - Can be used in any aggregate
  - Can be mixed with regular component associations :code:`Idx => Val`

=========================
Structuring Expressions
=========================

---------------------
Declare Expressions
---------------------

.. admonition:: Language Variant

   Ada 2022

* Convenient shorthand for repeated subexpression

  - Only constants and renamings allowed
  - Typically used in postconditions

  .. code:: Ada

     function Find (T : Table; R : Integer) return Integer
       with Post =>
         (declare
            Res : constant Integer := Find'Result;
          begin
            Res >= 0 and then
            (if Res /= 0 then T (Res) = R));

----------------------
Expression Functions
----------------------

* Convenient shorthand for repeated subexpression

  - Somewhat similar goal as delta expressions
  - But visible in a larger scope

* Simple query functions used in contracts

  .. code:: Ada

     function Increment (X : Integer) return Integer is
       (X + 1);

* Above is equivalent to having a postcondition

  - But no subprogram body to add in the body unit

  .. code:: Ada

     function Increment (X : Integer) return Integer
       with Post => Increment'Result = X + 1;

* Precondition can be specified after the expression

  .. code:: Ada

     function Increment (X : Integer) return Integer is
       (X + 1)
       with Pre => X < Integer'Last;

-----------------------------
Use of Expression Functions
-----------------------------

* Expression functions can be declared in a package spec and used in contracts

* For queries over objects of a private type

  - Function spec is declared in the public part

  - Expression function is declared in the private part

  .. code:: Ada

     package P is
       type T is private;
       function Value (X : T) return Integer;
     private
       type T is new Integer;
       function Value (X : T) return Integer is (Integer (X));
     end;

  - :toolname:`GNATprove` uses the implicit postcondition to prove client units

=====
Lab
=====

.. include:: labs/7_specification_language.lab.rst

=========
Summary
=========

------------------------
Specification Language
------------------------

* Rich specification language in SPARK

  - Conditional expressions
  - Quantified expressions
  - New forms of aggregates
  - Structuring expressions

* Expression functions are handled specially in proof

  - Implicit postcondition given by their expression

* Expression functions define queries on private types

  - Function spec declared in the visible part
  - Expression function given in the private part
  - Preserves abstraction for user
  - Gives enough details for proof
