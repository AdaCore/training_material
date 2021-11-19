with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Schedule is
   Maximum_Classes : constant := 24;

   type Days_T is (Mon, Tue, Wed, Thu, Fri, None);
   type Time_T is delta 0.5 range 0.0 .. 23.5;
   type Classes_T is tagged private;

   --  Question 1
   --  Implement this subprogram body
   procedure Add_Class (Classes    : in out Classes_T;
                        Name       :        String;
                        Day        :        Days_T;
                        Start_Time :        Time_T;
                        End_Time   :        Time_T) with
                        Pre => Count (Classes) < Maximum_Classes;

   procedure Print (Classes : Classes_T);
   function Count (Classes : Classes_T) return Natural;

private

   --  Question 2
   --  Define subtypes for short classes on Monday, Wednesday, Friday
   --  and long classes on Tuesday, Thursday
   --  Those types should be useable in a membership test.

   subtype Short_Class_T is Days_T
      with Static_Predicate => Short_Class_T in Mon | Wed | Fri;

   subtype Long_Class_T is Days_T
      with Static_Predicate => Long_Class_T in Tue | Thu;

   type Class_T is tagged record
      Name       : Unbounded_String := Null_Unbounded_String;
      Day        : Days_T           := None;
      Start_Time : Time_T           := 0.0;
      End_Time   : Time_T           := 0.0;
   end record;

   subtype Class_Size_T is Natural range 0 .. Maximum_Classes;
   subtype Class_Index_T is Class_Size_T range 1 .. Class_Size_T'Last;

   type Class_Array_T is array (Class_Index_T range <>) of Class_T;

   --  Question 3
   --  Implement the function
   --  Short classes should have a duration of 1.0 hr
   --  Long classes should have a duration of 1.5 hr
   --  In any case, the end time should be after the start time 
   function Valid_Times (Class : Class_T) return Boolean is
     (if Class.Day in Short_Class_T then Class.End_Time - Class.Start_Time = 1.0
      elsif Class.Day in Long_Class_T then Class.End_Time - Class.Start_Time = 1.5
      else Class.End_Time >= Class.Start_Time);

   --  Question 4
   --  Add a contract to guarantee that all classes have valid times
   type Classes_T is tagged record
      Size : Class_Size_T := 0;
      List : Class_Array_T (Class_Index_T);
   end record
     with Type_Invariant =>
        (for all Index in 1 .. Size => Valid_Times (Classes_T.List (Index)));

   function Count (Classes : Classes_T) return Natural is (Classes.Size);
end Schedule;
