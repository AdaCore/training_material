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

          [<block_name> :] declare
             <declarative_part>
          begin
             <statements>
          end [block_name];

       *where* **<block_name>** *is an identifier*

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

* Object in scope |rightarrow| exists while its enclosing block exists

.. note:: No *scoping* keywords (C's :c:`static`, :c:`auto` etc...)

.. image:: block_scope_example.jpeg
    :height: 50%

----------------------
Visibility in Action
----------------------

* **Name hiding**: a name used in an *inner scope* can hide the same name visible in the *outer scope*

   .. code:: Ada

      type Color is (Red, Green, Blue);
      type Size  is (Small, Medium, Large);

      declare
        My_Obj : Color;     -- outer My_Obj
      begin
        My_Obj := Green;    -- assigns to outer My_Obj (Color)
        declare
          My_Obj : Size;    -- hides outer My_Obj
        begin
          My_Obj := Medium; -- OK: inner My_Obj is Size
          My_Obj := Red;    -- compile error: inner My_Obj is not Color
        end;
        My_Obj := Blue;     -- OK: outer My_Obj is Color
        My_Obj := Small;    -- compile error: outer My_Obj is not Size
      end;

-------------------
Overcoming Hiding
-------------------

* Add a **prefix**

   - Needs named scope

.. warning::

    * Repeated name reuse is an indication of a *bigger problem*

        - May need refactoring...

.. code:: Ada

   type Color is (Red, Green, Blue);
   type Size  is (Small, Medium, Large);

   Outer : declare
     My_Obj : Color;
   begin
     My_Obj := Green;        -- outer (Color)
     declare
       My_Obj : Size;        -- inner (Size) hides the outer one
     begin
       My_Obj := Small;      -- inner Size
       Outer.My_Obj := Blue; -- apply prefix to use the hidden Color
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
         Value : Some_Type := 1;
      begin
         Value := Value + 1;
         declare
            Value : Some_Type := 2;
         begin
            Value := Value + 2;
            Print (Value);
         end;
         Print (Value);
      end;

 .. container:: column

   A. 2, 2
   B. 2, 4
   C. 4, 4
   D. :answer:`4, 2`

   .. container:: animate

      Explanation

      * Inner :ada:`Value` gets printed first. It is initialized to 2 and incremented by 2
      * Outer :ada:`Value` gets printed second. It is initialized to 1 and incremented by 1

