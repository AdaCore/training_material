=====================
Procedure Specifics
=====================

-----------------------------------
Return Statements in Procedures
-----------------------------------

.. container:: columns

 .. container:: column

    * Returns immediately to caller
    * Optional

       - Automatic at end of body execution

    * Fewer is traditionally considered better

 .. container:: column

    .. code:: Ada

       procedure P is
       begin
         ...
         if Some_Condition then
           return; -- early return
         end if;
         ...
       end P; -- automatic return

------------------
Main Subprograms
------------------

* Must be library subprograms

  * Not nested inside another subprogram

* No special subprogram unit name required
* Can be many per project
* Can always be procedures
* Can be functions if implementation allows it

   - Execution environment must know how to handle result

.. code:: Ada

   with Ada.Text_IO;
   procedure Hello is
   begin
     Ada.Text_IO.Put ("Hello World");
   end Hello;

