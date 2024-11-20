==================
Block Statements
==================

------------------
Block Statements
------------------

    * Local **scope**
    * Optional declarative part
    * Used for

       - Temporary declarations
       - Declarations as part of statement sequence
       - Local catching of exceptions

    * Syntax

       .. code:: Ada

          [block-name :]
          [declare <declarative part> ]
          begin
             <statements>
          end [block-name];

--------------------------
Block Statements Example
--------------------------

.. code:: Ada

   begin
      Get (V);
      Get (U);
      if U > V then -- swap them
         Swap: declare
            Temp : Integer;
         begin
            Temp := U;
            U := V;
            V := Temp;
         end Swap;
         -- Temp does not exist here
      end if;
      Print (U);
      Print (V);
   end;

