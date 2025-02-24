==============
Introduction
==============

-----------------
Ada.Text_IO
-----------------

* Most common I/O library unit - works with normal text I/O
* Works with `string` types

   - `Ada.Wide_Text_IO` for `Wide_String`
   - `Ada.Wide_Wide_Text_IO` for `Wide_Wide_String`

* Other I/O packages (not discussed in this module):

   - `Ada.Sequential_IO` and `Ada.Direct_IO`

      * Operations on binary files for components of a given type

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

