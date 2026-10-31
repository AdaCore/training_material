====================
Nested Subprograms
====================

---------------------------
Nested Subprogram Example
---------------------------

.. code:: Ada

   procedure Populate_Lines 
      (Lines : in out Types.Lines_T;
       Name  :        String) is

      function Read (Number : String) return Types.Line_T is
      begin
         Put (Name & " Line" & Number & "> ");
         return Types.Line_T'Value (Get_Line);
      end Read;

   begin
      for J in Lines'Range loop
         Lines (J) := Read (J'Image);
      end loop;
   end Populate_Lines;

