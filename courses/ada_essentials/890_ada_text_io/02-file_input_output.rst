===================
File Input/Output
===================

-------------------------
Standard Input / Output
-------------------------

* `Ada.Text_IO` maintains *default* input and output files

   .. code:: Ada

      -- reads from default input file
      S : constant String := Get_Line;
      -- ...
      -- writes to default output file
      Put_Line (S);

* At initialization, default input and output refer to the console

   - Which is why all our previous usage was so simple!

-------
Files
-------

* Files can be created (new for writing) or opened (for reading, writing, or appending)

   - File modes:

      * `In_File` :math:`\rightarrow` Open for reading
      * `Out_File` :math:`\rightarrow` Reset file and open for writing
      * `Append_File` :math:`\rightarrow` Position file at end and open for writing

.. code:: Ada

   declare
      File : File_Type;
   begin
      Create (File => File,
              Mode => Out_File,
              Name => "foo.txt");
      Put_Line (File, "Line 1");
      Close (File);
      -- This "Open" is only legal because "foo.txt" already exists
      Open (File, Out_File, "foo.txt");
      Put_Line (File, "Line 2");
      Close (File);
      Open (File, Append_File, "foo.txt");
      Put_Line (File, "Line 3");
      Close (File);
      Open (File, In_File, "foo.txt");
      -- Read lines from file and print to standard output
      Put_Line (Get_Line (File));
      Put_Line (Get_Line (File));
   end;

-------------------
File Status Queries
-------------------

.. list-table::

   * - `End_Of_File`

     - Check if end of file has been reached

   * - `Is_Open`

     - Check if file has been opened (regardless of file mode)

   * - `Mode`

     - Return how file was opened

   * - `Name`

     - Name of open file

   * - `Col`

     - Current column in file

   * - `Line`

     - Current line in file

