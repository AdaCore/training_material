=============
Propagation
=============

-------------
Propagation
-------------

* Control does not return to point of raising

   - Termination Model

* When a handler is not found in a block statement

   - Re-raised immediately after the block

* When a handler is not found in a subprogram

   - Propagated to caller at the point of call

* Propagation is dynamic, back up the call chain

   - Not based on textual layout or order of declarations

* Propagation stops at the main subprogram

   - Main completes abnormally unless handled

------------------
Propagation Demo
------------------

.. container:: columns

 .. container:: column

    .. code:: Ada
      :number-lines: 1

       procedure Do_Something is
         Error : exception;
         procedure Unhandled is
         begin
           Maybe_Raise (1);
         end Unhandled;
         procedure Handled is
         begin
           Unhandled;
           Maybe_Raise (2);
         exception
           when Error =>
             Print ("Handle 1 or 2");
         end Handled;

 .. container:: column

    .. code:: Ada
      :number-lines: 16

       begin -- Do_Something
         Maybe_Raise (3);
         Handled;
       exception
         when Error =>
           Print ("Handle 3");
       end Do_Something;

-------------------
Termination Model
-------------------

* When control goes to handler, it continues from here

.. code:: Ada

   procedure Joy_Ride is
   begin
      loop
          Steer_Aimlessly;

          -- If next line raises Fuel_Exhausted, go to handler
          Consume_Fuel;
      end loop;
   exception
     when Fuel_Exhausted => -- Handler
       Push_Home;
       -- Resume from here: loop has been exited
   end Joy_Ride;

------
Quiz
------

.. code:: Ada
  :number-lines: 2

    Main_Problem : exception;
    I : Integer;
    function F (P : Integer) return Integer is
    begin
      if P > 0 then
        return P + 1;
      elsif P = 0 then
        raise Main_Problem;
      end if;
    end F;
    begin
      I := F(Input_Value);
      Put_Line ("Success");
    exception
      when Constraint_Error => Put_Line ("Constraint Error");
      when Program_Error    => Put_Line ("Program Error");
      when others           => Put_Line ("Unknown problem");

What will get printed if :ada:`Input_Value` on line 13 is :ada:`Integer'Last`?

A. ``Unknown Problem``
B. ``Success``
C. :answermono:`Constraint Error`
D. ``Program Error``

.. container:: animate

  Explanations

  A. ``"Unknown Problem"`` is printed by the :ada:`when others` due to the raise on line 9 when :ada:`P` is 0
  B. ``"Success"`` is printed when  0 < :ada:`P` < :ada:`Integer'Last`
  C. Trying to add 1 to :ada:`P` on line 7 generates a :ada:`Constraint_Error`
  D. :ada:`Program_Error` will be raised by :ada:`F` if :ada:`P` < 0 (no :ada:`return` statement found)

