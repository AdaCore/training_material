======
Logger
======

----------------
Public Interface
----------------

* :ada:`Logger` uses a file for writing
* :ada:`limited` cannot be copied, or compared
* :ada:`procedure Put_Line` for logging

.. code:: Ada

   type Logger (Filename : not null access constant String)
     is tagged limited private;

   procedure Put_Line
     (L : Logger; S : String);

----------------------------
Implementation: Private part
----------------------------

.. code:: Ada

   type Logger (Filename : not null access constant  String)
      is new Ada.Finalization.Limited_Controlled with

.. note::

    * :ada:`Limited_Controlled`
    * Maintains a handle to the log file

.. code:: Ada

   record
      Logfile : Ada.Text_IO.File_Type;
   end record;

   procedure Initialize (L : in out Logger);
   --  opens the file
   procedure Finalize (L : in out Logger);
   --  closes the file

--------------------
Implementation: Body
--------------------

* Trivial

.. tip::

   Once the hard part of designing the interface is done, implementation is trivial.

.. code:: Ada

    with Ada.Text_IO; use Ada.Text_IO;

    package body Loggers is
       procedure Initialize (L : in out Logger) is
       begin
            Create (L.Logfile, Out_File, L.Filename.all);
            Put_Line (L, "Starting");
       end Initialize;

       procedure Put_Line (L : Logger; S : String) is
       begin
            Put_Line ("Logger: " & S);
            Put_Line (L.Logfile, S);
       end Put_Line;

       procedure Finalize
            (L : in out Logger) is
       begin
            Put_Line (L, "Closing");
            Close (L.Logfile);
       end Finalize;
    end Loggers;
