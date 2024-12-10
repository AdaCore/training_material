==============
Generic Data
==============

-------------
Definitions
-------------

* A formal generic parameter is a template
* Properties are either :dfn:`constraints` or :dfn:`capabilities`

    - Expressed from the **body** point of view
    - Constraints: e.g. unconstrained, :ada:`limited`
    - Capabilities: e.g. :ada:`tagged`, primitives

.. code:: Ada

   generic
      type Pv is private;           -- allocation, copy, assignment, "="
      with procedure Sort (T : Pv); -- primitive of Pv
      type Unc (<>) is private;     -- allocation require a value
      type Lim is limited private;  -- no copy or comparison
      type Disc is (<>);            -- 'First, ordering
   package Generic_Pkg is [...]

* Actual parameter **may** require constraints, and **must** provide capabilities

.. code:: Ada

   package Pkg is new Generic_Pkg (
      Pv => Integer, -- has capabilities of private
      Sort => Sort -- procedure Sort (T : Integer)
      Unc => String,  -- uses "unconstrained" constraint
      Lim => Float,   -- does not use "limited" constraint
      Disc => Boolean, -- has capability of discrete
  );

------------------
Syntax (partial)
------------------

.. code:: Ada

   type T1 is (<>); -- discrete
   type T2 is range <>; -- Integer
   type T3 is digits <>; -- float
   type T4 is private; -- indefinite
   type T5 (<>) is private; -- indefinite
   type T6 is tagged private;
   type T7 is array (Boolean) of Integer;
   type T8 is access Integer;
   type T9 is limited private;

* Not limited to those choices

.. code:: Ada

   type T is not null access all limited tagged private;

------
Quiz
------

Which of the following statement is true?

A. Generic contracts define new types
B. Generic contracts can express any type constraint
C. :answer:`Generic contracts can express inheritance constraint`
D. Generic contracts can require a type to be numeric (:ada:`Real` or :ada:`Integer`)

.. container:: animate

   A. No, the formal type and the actual type just have different views
   B. Counter-example: representation clauses

------
Quiz
------

.. include:: ../quiz/generic_subp_syntax/quiz.rst

------
Quiz
------

.. code:: Ada

   generic
      type T1 is (<>);
      type T2 (<>) is private;
   procedure G
     (A : T1;
      B : T2);

Which is (are) legal instantiation(s)?

   A. ``procedure A is new G (String, Character);``
   B. :answermono:`procedure B is new G (Character, Integer);`
   C. :answermono:`procedure C is new G (Integer, Boolean);`
   D. :answermono:`procedure D is new G (Boolean, String);`

.. container:: animate

   :ada:`T1` must be discrete - so an integer or an enumeration. :ada:`T2` can be any type

