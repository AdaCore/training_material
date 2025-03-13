======================
Scope and Visibility
======================

----------------------
Scope and Visibility
----------------------

* :dfn:`Scope` of a name

   - Where the name is **potentially** available
   - Determines **lifetime**
   - Scopes can be **nested**

* :dfn:`Visibility` of a name

   - Where the name is **actually** available
   - Defined by **visibility rules**
   - **Hidden** |rightarrow| *in scope* but not **directly** visible

------------------------------
Introducing Block Statements
------------------------------

* **Sequence** of statements

   - Optional *declarative part*
   - Can be **nested**
   - Declarations **can hide** outer variables

.. container:: columns

 .. container:: column

    * Syntax

       .. code:: Ada

          [<block-name> :] declare
             <declarative part>
          begin
             <statements>
          end [block-name];

 .. container:: column

    * Example

       .. code:: Ada

          Swap: declare
            Temp : Integer;
          begin
            Temp := U;
            U := V;
            V := Temp;
          end Swap;

----------------------
Scope and "Lifetime"
----------------------

* Object in scope |rightarrow| exists

.. note:: No *scoping* keywords (C's :c:`static`, :c:`auto` etc...)

.. image:: block_scope_example.jpeg
    :height: 50%

-------------
Name Hiding
-------------

* Caused by **homographs**

    - **Identical** name
    - **Different** entity

   .. code:: Ada

      declare
        M : Integer;
      begin
        M := 123;
        declare
          M : Float;
        begin
          M := 12.34; -- OK
          M := 0;     -- compile error: M is a Float
        end;
        M := 0.0; -- compile error: M is an Integer
        M := 0;   -- OK
      end;

-------------------
Overcoming Hiding
-------------------

* Add a **prefix**

   - Needs named scope

.. warning::

    * Homographs are a *code smell*

        - May need **refactoring**...

.. code:: Ada

   Outer : declare
     M : Integer;
   begin
     M := 123;
     declare
       M : Float;
     begin
       M := 12.34;
       Outer.M := Integer (M);  -- reference "hidden" Integer M
     end;
   end Outer;

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment footnotesize

   What output does the following code produce? (Assume :code:`Print` prints the current value of its argument)

   .. code:: Ada
      :number-lines: 1

      declare
         M : Integer := 1;
      begin
         M := M + 1;
         declare
            M : Integer := 2;
         begin
            M := M + 2;
            Print (M);
         end;
         Print (M);
      end;

 .. container:: column

   A. 2, 2
   B. 2, 4
   C. 4, 4
   D. :answer:`4, 2`

   .. container:: animate

      Explanation

      * Inner :ada:`M` gets printed first. It is initialized to 2 and incremented by 2
      * Outer :ada:`M` gets printed second. It is initialized to 1 and incremented by 1

