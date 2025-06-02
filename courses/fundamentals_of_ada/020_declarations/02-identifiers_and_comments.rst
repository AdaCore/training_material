==========================
Identifiers and Comments
==========================

-----------
Identifiers
-----------

.. image:: identifier_flow.png
   :width: 60%

.. container:: columns

 .. container:: column

   * Legal identifiers

      .. code:: Ada

         Phase2
         A
         Space_Person

 .. container:: column

   * Not legal identifiers

      .. code:: Ada

         Phase2__1
         A_
         _space_person

.. warning:: Reserved words are **forbidden**

* Character set **Unicode** 4.0
* Case **not significant**

   - `SpacePerson` |equivalent| `SPACEPERSON`
   - ...but **different** from `Space_Person`

----------------------
Identifiers vs Names
----------------------

:dfn:`identifier`
  Syntactic form used typically to introduce entities when declared

:dfn:`name`
  Typically starts with an identifier and can be followed by one or more suffixes to help indicate something more specific, such as a record component or an array slice

.. tip::

  An **identifier** is used to *define* an entity, and a **name** is used to *refer to* an entity (or part of one)

----------------
Reserved Words
----------------

.. code:: Ada

   abort          else              null               reverse
   abs            elsif             of                 select
   abstract (95)  end               or                 separate
   accept         entry             others             some (2012)
   access         exception         out                subtype
   aliased (95)   exit              overriding (2005)  synchronized (2005)
   all            for               package            tagged (95)
   and            function          parallel (2022)    task
   array          generic           pragma             terminate
   at             goto              private            then
   begin          if                procedure          type
   body           in                protected (95)     until (95)
   case           interface (2005)  raise              use
   constant       is                range              when
   declare        limited           record             while
   delay          loop              rem                with
   delta          mod               renames            xor
   digits         new               requeue (95)
   do             not               return

----------
Comments
----------

* Terminate at end of line (i.e., no comment terminator sequence)

   .. code:: Ada

      -- This is a multi-
      -- line comment
      A : B; -- this is an end-of-line comment

----------------------------------------------
Declaring Constants / Variables (simplified)
----------------------------------------------

* An :dfn:`expression` is a piece of Ada code that returns a **value**.

.. code:: Ada

   <identifier> : constant := <expression>;
   <identifier> : <type> := <expression>;
   <identifier> : constant <type> := <expression>;

------
Quiz
------

Which statement(s) is (are) legal?

   A. ``Function : constant := 1;``
   B. :answermono:`Fun_ction : constant := 1;`
   C. ``Fun_ction : constant := --initial value-- 1;``
   D. ``Integer Fun_ction;``

.. container:: animate

   Explanations

   A. :ada:`function` is a reserved word
   B. Correct
   C. Cannot have inline comments
   D. C-style declaration not allowed

