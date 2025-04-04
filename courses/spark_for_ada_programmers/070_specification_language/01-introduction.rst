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

