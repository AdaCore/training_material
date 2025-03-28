************************
Specification Language
************************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. role:: rust(code)
    :language: Rust

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==============
Introduction
==============

--------------------
Simple Expressions
--------------------

* Simple specifications use **simple** expressions

  - Arithmetic operations and comparisons
  - Membership tests :ada:`X in A .. B`

    .. code:: ada

          I in T'Range

    is better than:

    .. code:: ada

          I >= T'First and I <= T'Last

  - Conjunctions and disjunctions

    + Lazy operators :ada:`and then`/:ada:`or else` preferred in general to
      :ada:`and`/:ada:`or`

* But that's not sufficient to easily write **all** specifications

  .. code:: ada

     procedure Init_Table (T : out Table)
     with
       Pre  => T'Length > 0,
       Post => -- if T is of length 1 ...
               -- else if T is of length 2 ...
               -- else for all components ...

--------------------
Richer Expressions
--------------------

* Counterparts of **conditional** statements

  - *if expressions* are the counterpart of *if statements*
  - *case expressions* are the counterpart of *case statements*

* Expressions over a **collection** (range or array or...)

  - *universally quantified expression* for properties over **all** components
  - *existentially quantified expression* for properties over **one** component

* New forms of **aggregates**

  - *delta aggregates* express the value of an updated composite object
  - *iterated component associations* express array aggregates where the
    expression depends on the **index**
  - *container aggregates* give the value of a container

* Structuring expressions

  - *declare expressions* introduce **names** for local constants
  - *expression functions* introduce **names** for common expressions

=========================
Conditional Expressions
=========================

----------------
If Expressions
----------------

* :ada:`(if Cond then A else B)` evaluates :ada:`A` or :ada:`B` depending on
  the value of :ada:`Cond`

  - Note: always in **parentheses**!
  - :ada:`A` and :ada:`B` must have the same type
  - ...not always :ada:`Boolean`!

    .. code:: Ada

       A := (if Cond then 2 else 3);

* Frequent use with :ada:`Boolean` type in specifications

  - :ada:`(if Cond then Property)` is shortcut for :ada:`(if Cond then Property else True)`
  - This expresses a **logical implication** :ada:`Cond` |rightarrow| :ada:`Property`
  - Also equivalent to :ada:`not Cond or else Property`

* Complete form has :ada:`elsif` parts

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

* Same **choice expressions** as in *case statements*

  - Can also use :ada:`others` as last alternative
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

* Also allowed for opposite membership test: :ada:`if X not in ...`

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

* Universally quantified expression :ada:`(for all J in A .. B => Property)`

  - Express that property holds for **all** values in the range
  - True if the range is empty (:math:`\forall` in logic)
  - At runtime, executed as a loop which stops at first value where the property is not satisfied

* Existentially quantified expression :ada:`(for some J in A .. B => Property)`

  - Express that property holds for **at least one** value in the range
  - False if the range is empty (:math:`\exists` in logic)
  - At runtime, executed as a loop which stops at first value where the property is satisfied

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

  - Universally quantified expression :ada:`(for all E of T => Property)`
  - Existentially quantified expression :ada:`(for some E of T => Property)`

* Note: always in **parentheses**!

----------------------------------
Range-based Vs Array-based Forms
----------------------------------

* Array-based form only possible if :ada:`Property` does **not** refer to the
  **index**

* Example: array :ada:`T` is sorted

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

* **Based** on the :ada:`Iterable` aspect on a type

  - **Not the same** as the standard Ada mechanism!
  - **Simpler** mechanism adopted for the SPARK formal containers

  .. code:: ada

     type Container is private with
       Iterable => (First       => First,
                    Next        => Next,
                    Has_Element => Element
                    Element     => Element);

* :dfn:`Iteration over positions` uses :ada:`for .. in` syntax

 - Uses cursor type with :ada:`First`, :ada:`Next` and :ada:`Has_Element`
 - Function :ada:`Element` is **not** required

* :dfn:`Iteration over components` uses :ada:`for .. of` syntax

  - Based on the previous iteration
  - Function :ada:`Element` retrieves the **component** for a given cursor

----------------------------------
Iteration Over Formal Containers
----------------------------------

* **Generic** units compatible with SPARK

  - The API is slightly different from standard Ada containers
  - Available in the SPARK Library

* Available for **all** formal containers:

  - vectors
  - doubly linked lists
  - sets (hashed and ordered)
  - maps (hashed and ordered)

* Iteration over positions

  - Access to **component** through function :ada:`Element`
  - For maps, access to **key** through function :ada:`Key`

* Iteration over components

  - For maps, really an iteration over **keys**

    - Use another function :ada:`Element` to get **component**

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
       (for all J in V.First_Index .. V.Last_Index => V.Component (J) = 0);

* Iteration over positions

  .. code:: ada

     for J in V loop
        V.Replace_Element (J, 0);
     end loop;
     pragma Assert (for all J in V => V.Element (J) = 0);

* Iteration over components (**no update**!)

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

* Express the value of a **modified** composite object (record or array)

  .. code:: Ada

     (Rec with delta Comp1 => Val1, Comp2 => Val2)
     (Arr with delta 1 => True, 42 => False)

* Typically used to relate input and output **values** of parameters

  - Combines delta aggregate with use of attribute :ada:`'Old`

  .. code:: Ada

     procedure P (Rec : in out T)
       with Post => Rec = (Rec'Old with delta Comp1 => Val1,
                                              Comp2 => Val2);

* With array object:

  - Avoids the introduction of **explicit** quantifiers
  - Can have **overlapping** and **dynamic** choices (values or ranges)

-----------------------------
Extension of Delta Aggregates
-----------------------------

.. admonition:: Language Variant

   GNAT Extension

* GNAT extension allowed using either

  - switch :command:`-gnatX0`, or
  - pragma :ada:`Extensions_Allowed (All)`
    
* Choice can be a subcomponent of the record or array

  .. code:: Ada

     (Rec with delta Comp1.Sub1 => Val1,
                     Comp2.Sub2.Subsub => Val2)
     (Arr with delta (1).Sub => True,
                     (42).Subarr(4) => False)

---------------------------------
Iterated Component Associations
---------------------------------

.. admonition:: Language Variant

   Ada 2022

* Express the **value** of an array aggregate depending on index
* Example: the *identity* function

  .. code:: Ada

     (for J in T'Range => J)

* This is a :dfn:`component association`

  - Can be used in **any** aggregate
  - Can be mixed with regular component associations :ada:`Idx => Val`

----------------------
Container Aggregates
----------------------

.. admonition:: Language Variant

   Ada 2022

* Available for all functional and formal containers

* Vectors, lists and sets use the positional syntax:

  .. code:: Ada

     V : Vector := [1, 2, 3];
     L : List := [1, 2, 3];
     S : Set := [1, 2, 3];

* Maps use the named syntax:

  .. code:: Ada

     M : Map := [1 => 8, 4 => 3, 42 => 127];

* General mechanism using the :ada:`Container_Aggregates` annotation

  - Three predefined patterns :ada:`Predefined_Sequences`,
    :ada:`Predefined_Sets` and :ada:`Predefined_Maps` require specific API
    (used for functional containers)

  - :ada:`From_Model` only requires ``Model`` function returning the above
    (used for formal containers)

  - consistency checked by :toolname:`GNATprove`

=========================
Structuring Expressions
=========================

---------------------
Declare Expressions
---------------------

.. admonition:: Language Variant

   Ada 2022

* Convenient shorthand for **repeated** subexpression

  - Only constants and renamings allowed
  - Typically used in **postconditions**

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

* Convenient shorthand for **repeated** subexpression

  - Somewhat similar goal as declare expressions
  - But visible in a **larger** scope

* Simple query functions used in contracts

  .. code:: Ada

     function Is_Sorted (T : Table) return Boolean is
       (for all J in T'Range =>
          (for all K in T'Range => (if J < K then T(J) <= T(K))));

* Above is equivalent to having a **postcondition**

  - But no subprogram body to add in the body unit

  .. code:: Ada

     function Is_Sorted (T : Table) return Boolean
       with Post => Is_Sorted'Result = (for all J in T'Range => ...);

* Pre and postconditions can be specified **after** the expression

  .. code:: Ada

     function Is_Sorted (T : Table) return Boolean is (...)
       with Pre => T'Length > 0;

-----------------------------
Use of Expression Functions
-----------------------------

* Expression functions can be declared in a package spec and used in **contracts**

  - It can even be declared **after** its use in contracts!

* For queries over objects of a :ada:`private` type

  - Function **spec** is declared in the **public** part
  - **Expression function** is declared in the **private** part

  .. code:: Ada

     package P is
       type T is private;
       function Value (X : T) return Integer;
     private
       type T is new Integer;
       function Value (X : T) return Integer is (Integer (X));
     end;

  - :toolname:`GNATprove` uses the **implicit postcondition** to prove client units

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

* Rich **specification language** in SPARK

  - Conditional expressions
  - Quantified expressions
  - New forms of aggregates
  - Structuring expressions

* Expression functions are handled **specially** in proof

  - Implicit postcondition given by their expression

* Expression functions define **queries** on private types

  - Function spec declared in the visible part
  - Expression function given in the private part
  - Preserves abstraction for user
  - Gives enough details for proof
