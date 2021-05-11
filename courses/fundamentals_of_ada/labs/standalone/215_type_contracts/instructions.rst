--------------------
Type Contracts Lab
--------------------

* Overview

   - Create simplistic class scheduling system

      + Client will specify name, day of week, start time, end time
      + Supplier will add class to schedule
      + Supplier must also be able to print schedule

* Requirements

   - Monday, Wednesday, and/or Friday classes can only be 1 hour long
   - Tuesday and/or Thursday classes can only be 1.5 hours long
   - Classes that don't have a set day can meet for any non-negative length of time

* Hints

   - *Subtype Predicate* to create subtypes of day of week
   - *Type Invariant* to ensure that every class meets for correct length of time

------------------------------------
Type Contracts Lab Solution (Spec)
------------------------------------
.. code:: Ada

   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   package Schedule is
      Maximum_Classes : constant := 24;
      type Days_T is (Mon, Tue, Wed, Thu, Fri, None);
      type Time_T is delta 0.5 range 0.0 .. 23.5;
      type Classes_T is tagged private;
      procedure Add_Class (Classes    : in out Classes_T;
                           Name       :        String;
                           Day        :        Days_T;
                           Start_Time :        Time_T;
                           End_Time   :        Time_T) with
                           Pre => Count (Classes) < Maximum_Classes;
      procedure Print (Classes : Classes_T);
      function Count (Classes : Classes_T) return Natural;
   private
      subtype Short_Class_T is Days_T with Static_Predicate => Short_Class_T in Mon | Wed | Fri;
      subtype Long_Class_T is Days_T with Static_Predicate => Long_Class_T in Tue | Thu;
      type Class_T is tagged record
         Name       : Unbounded_String := Null_Unbounded_String;
         Day        : Days_T           := None;
         Start_Time : Time_T           := 0.0;
         End_Time   : Time_T           := 0.0;
      end record;
      function Valid_Times (Class : Class_T) return Boolean is
        (if Class.Day in Short_Class_T then
           Class.End_Time - Class.Start_Time = 1.0
         elsif Class.Day in Long_Class_T then
           Class.End_Time - Class.Start_Time = 1.5
         else Class.End_Time >= Class.Start_Time);
      subtype Class_Size_T is Natural range 0 .. Maximum_Classes;
      subtype Class_Index_T is Class_Size_T range 1 .. Class_Size_T'Last;
      type Class_Array_T is array (Class_Index_T range <>) of Class_T;
      type Classes_T is tagged record
         Size : Class_Size_T := 0;
         List : Class_Array_T (Class_Index_T);
      end record with
         Type_Invariant =>
         (for all Index in 1 .. Size => Valid_Times (Classes_T.List (Index)));
      function Count (Classes : Classes_T) return Natural is (Classes.Size);
   end Schedule;
   
------------------------------------
Type Contracts Lab Solution (Body)
------------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   package body Schedule is
   
      procedure Add_Class
        (Classes    : in out Classes_T;
         Name       :        String;
         Day        :        Days_T;
         Start_Time :        Time_T;
         End_Time   :        Time_T) is
      begin
         Classes.Size                := Classes.Size + 1;
         Classes.List (Classes.Size) :=
           (Name       => To_Unbounded_String (Name), Day => Day,
            Start_Time => Start_Time, End_Time => End_Time);
      end Add_Class;
   
      procedure Print (Classes : Classes_T) is
      begin
         for Index in 1 .. Classes.Size
         loop
            Put_Line
              (Days_T'Image (Classes.List (Index).Day) & ": " &
               To_String (Classes.List (Index).Name) & " (" &
               Time_T'Image (Classes.List (Index).Start_Time) & " -" &
               Time_T'Image (Classes.List (Index).End_Time) & " )");
         end loop;
      end Print;
   
   end Schedule;
   
------------------------------------
Type Contracts Lab Solution (Main)
------------------------------------

.. code:: Ada

   with Ada.Exceptions; use Ada.Exceptions;
   with Ada.Text_IO;    use Ada.Text_IO;
   with Schedule;       use Schedule;
   procedure Main is
      Classes : Classes_T;
   begin
      Classes.Add_Class (Name       => "Calculus",
                         Day        => Mon,
                         Start_Time => 10.0,
                         End_Time   => 11.0);
      Classes.Add_Class (Name       => "History",
                         Day        => Tue,
                         Start_Time => 11.0,
                         End_Time   => 12.5);
      Classes.Add_Class (Name       => "Biology",
                         Day        => Wed,
                         Start_Time => 13.0,
                         End_Time   => 14.0);
      Classes.Print;
      begin
         Classes.Add_Class (Name       => "Biology",
                            Day        => Thu,
                            Start_Time => 13.0,
                            End_Time   => 14.0);
      exception
         when The_Err : others =>
            Put_Line (Exception_Information (The_Err));
      end;
   end Main;
