==========================
Identifiers and Comments
==========================

-----------
Identifiers
-----------

.. image:: identifier_flow.svg

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

* **Identifier** 

  * Syntactic form used typically to introduce entities when declared

  .. image:: identifiers_vs_names_1.svg
     :align: center
     :height: 50%

* **Name** 

  * Starts with an identifier 
  * Can be followed by one or more suffixes

    * Indicate something specific, such as a record component or an array index

  .. image:: identifiers_vs_names_2.svg
     :align: center
     :height: 50%

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
