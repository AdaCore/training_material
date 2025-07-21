=====================
Nested Subprograms
=====================

--------------------------------
Subprograms Within Subprograms
--------------------------------

* Subprograms can be placed in any declarative block

   * So they can be nested inside another subprogram
   * Or even within a :ada:`declare` block

* Useful for performing sub-operations without passing parameter data

----------------------------
Nested Subprogram Example
----------------------------

.. code:: Ada
   :number-lines: 1

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

