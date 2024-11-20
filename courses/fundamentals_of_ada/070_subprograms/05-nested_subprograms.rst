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

   procedure Main is

      function Read (Prompt : String) return Types.Line_T is
      begin
         Put (Prompt & "> ");
         return Types.Line_T'Value (Get_Line);
      end Read;

      Lines : Types.Lines_T (1 .. 10);
   begin
      for J in Lines'Range loop
         Lines (J) := Read ("Line " & J'Image);
      end loop;

