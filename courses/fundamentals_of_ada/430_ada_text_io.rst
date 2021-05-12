
******************
Ada.Text_IO
******************

==============
Introduction
==============

-----------------
Ada.Text_IO
-----------------

* Most common I/O library unit - works with normal text I/O
* Works with `string` types

   - `Ada.Wide_Text_IO` for `wide_string`
   - `Ada.Wide_Wide_Text_IO` for `wide_wide_string`

* Other I/O packages (not discussed in this module):

   - `Ada.Sequential_IO` and `Ada.Direct_IO`

      * Operations on binary files for elements of a given type

   - `Ada.Storage_IO`

      * Operations on reading/writing to/from memory buffer

   - `Ada.Streams.Stream_IO`

      * Operations for streaming data to/from binary files

.. code:: Ada

   declare
      -- read from default input file
      From_Input : constant String := Ada.Text_IO.Get_Line;
   begin
      -- write to default output file
      Ada.Text_IO.Put_Line ("I just typed: " & From_Input);
   end;

------------------
Scalar Type I/O
------------------

* Child generic packages of `Ada.Text_IO` to read / write scalar types

   - `Ada.Text_IO.Integer_IO`
   - `Ada.Text_IO.Modular_IO`
   - `Ada.Text_IO.Float_IO`
   - `Ada.Text_IO.Fixed_IO`
   - `Ada.Text_IO.Decimal_IO`
   - `Ada.Text_IO.Enumeration_IO`

* Create instances of the generic package to read/write

.. code:: Ada

   declare
      type Float_T is digits 6;
      package Float_IO is new Ada.Text_IO.Float_IO (Float_T);
      F : Float_T;
   begin
      -- Read floating point number from default input file
      Float_IO.Get (F);
      -- Writing floating point number to default output file
      Float_IO.Put (F * 10.0, Fore => 1, Aft => 2, Exp => 3);
   end;

===================
File Input/Output
===================

-------------------------
Standard Input / Output
-------------------------

* `Ada.Text_IO` maintains *default* input and output files

   .. code:: Ada

      -- reads from default input file
      S : constant string := Get_Line;
      -- ...
      -- writes to default output file
      Put_Line ( S );

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

===================
Type-Specific I/O
===================

------------------------
Ada.Text_IO.Integer_IO
------------------------

.. code:: Ada

   declare
      type Integer_T is range -1_000 .. 1_000;
      package Io is new Ada.Text_IO.Integer_IO (Integer_T);
      I : Integer_T;
   begin
      Io.Get (I);
      Io.Put
        (Item  => I,
         Width => 10,  -- optional: minimum number of characters to print
         Base  => 16); -- optional: numeric base
   end;

* `Get` will read until a non-numeric character is encountered, ignoring leading or trailing whitespace

   - **123** will set I to 123
   - **45X67** will set I to 45

* :ada:`IO` has global objects :ada:`Default_Width` and :ada:`Default_Base` which can be modified to set default values for like-named parameters
* :ada:`Ada.Text_IO.Modular_IO` behaves the same

------------------------
Ada.Text_IO.Float_IO
------------------------

.. code:: Ada

   declare
      type Float_T is digits 6 range -100.0 .. 100.0;
      package Io is new Ada.Text_IO.Float_IO (Float_T);
      F : Float_T;
   begin
      Io.Get (F);
      Io.Put
        (Item => F,
         Fore => 1,   -- optional: number of digits before decimal point
         Aft  => 2,   -- optional: number of digits after decimal point
         Exp  => 3);  -- optional: numeric of characters for exponent
   end;

* `Get` will read until a non-numeric character is encountered, ignoring leading or trailing whitespace

   - **12** will set F to 12.0
   - **23.45.67** will set F to 23.45

* :ada:`IO` has global objects :ada:`Default_Fore`, :ada:`Default_Aft` and :ada:`Default_Exp` which can be modified to set default values for like-named parameters
* `Ada.Text_IO.Fixed_IO` and `Ada.Text_IO.Decimal_IO` behave the same

----------------------------
Ada.Text_IO.Enumeration_IO
----------------------------

.. code:: Ada

   declare
      type Enumeration_T is ( Red, Yellow, Green );
      package Io is new Ada.Text_IO.Enumeration_IO (Enumeration_T);
      E : Enumeration_T;
   begin
      Io.Get (E);
      Io.Put
        (Item => F,
         Width => 10,          -- optional: minimum number of characters to print
         Set   => Lower_Case); -- optional: flag for Upper_Case or Lower_Case
   end;

* `Get` will read until the end of the line or trailing whitespace, case-insensitive

   - **YelloW** will set `E` to `Yellow`
   - **Red Blue** will set `E` to `Red`

* :ada:`IO` has global objects :ada:`Default_Width` and :ada:`Default_Setting` which can be modified to set default values for like-named parameters

============
Exceptions
============

-------------------
Ada.IO_Exceptions
-------------------

* I/O Packages have common exceptions (defined in `Ada.IO_Exceptions` and renamed in `Ada.Text_IO` for easier reference)
* The most common Text I/O exceptions:

   * :ada:`Status_Error` :math:`\rightarrow` Raised on `Open`/`Create` if file being opened/created is already open. For any other operation, raised if file is not open
   * :ada:`Name_Error` :math:`\rightarrow` Raised if filename is invalid for `Open`/`Create`
   * :ada:`Use_Error` :math:`\rightarrow` Raised if unable to `Open`/`Create`
   * :ada:`Data_Error` :math:`\rightarrow` Failure of `Get` to read valid data

========
Lab
========

.. include:: labs/430_ada_text_io.lab.rst

=========
Summary
=========

---------
Summary
---------

* :ada:`Ada.Text_IO` is the most common text input/output processing process
* Text_IO has simple mechanisms to read scalar types

   - 'Image and 'Value work, but are simplistic

      * 'Image does not allow formatting of output
      * 'Value will fail if entire input cannot be converted
