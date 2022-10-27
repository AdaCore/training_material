with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
use all type Ada.Strings.Unbounded.Unbounded_String;

with Integer_Vectors;
with Named_Constants_Maps; use Named_Constants_Maps;
use all type Named_Constants_Maps.Pkg_Maps.Map;
use all type Named_Constants_Maps.Pkg_Maps.Cursor;
with String_Id_Sets; use String_Id_Sets;
use all type String_Id_Sets.Pkg_Sets.Set;
use all type String_Id_Sets.Pkg_Sets.Cursor;

procedure Main is

   package Integer_Vectors_By_Positive is new Integer_Vectors
     (Positive);
   package Pkg_Vectors renames
     Integer_Vectors_By_Positive.Pkg_Vectors;
   use all type Pkg_Vectors.Vector;
   use all type Pkg_Vectors.Cursor;

   --$ begin cut
   Student_Per_Day : Pkg_Vectors.Vector (5);
   -- Warning: initial size is 0, using an Empty_Vector as
   --          initial value would mean a *capacity* of 0!

   Received_Parcels : Pkg_Sets.Set := Pkg_Sets.Empty_Set;

   Math_Constants : Pkg_Maps.Map := Pkg_Maps.Empty_Map;
   --$ end cut

   Student_Per_Day2 : Pkg_Vectors.Vector (5);

   Received_Parcels2 : Pkg_Sets.Set := Pkg_Sets.Empty_Set;

   Math_Constants2 : Pkg_Maps.Map := Pkg_Maps.Empty_Map;

begin
   ----------------
   -- Insertions --
   ----------------

   --$ begin cut
   Student_Per_Day.Append (10);
   Student_Per_Day.Append (8);
   Student_Per_Day.Append (9);

   Received_Parcels.Insert ("FEDEX AX431661VD");
   Received_Parcels.Insert ("UPS ZZ-44-I12");

   Math_Constants.Insert
     (To_Unbounded_String ("Pi"), 3.141_59);
   Math_Constants.Insert (To_Unbounded_String ("e"), 2.718);
   --$ end cut

   ----------------
   -- Iterations --
   ----------------

   --$ begin cut
   for Student_Count of Student_Per_Day loop
      Put_Line (Integer'Image (Student_Count));
   end loop;

   for Parcel_Id of Received_Parcels loop
      Put_Line (Parcel_Id);
   end loop;

   -- We use the cursor to have both key and value
   for C in Math_Constants.Iterate loop
      Put_Line
        (To_String (Key (C)) & " = " &
         Float'Image (Element (C)));
   end loop;
   --$ end cut

   -----------------
   -- Comparisons --
   -----------------

   Student_Per_Day2.Append (10);
   Student_Per_Day2.Append (8);
   Student_Per_Day2.Append (9);

   Received_Parcels2.Insert ("FEDEX AX431661VD");
   Received_Parcels2.Insert ("UPS ZZ-44-I12");

   Math_Constants2.Insert
     (To_Unbounded_String ("Pi"), 3.141_59);
   Math_Constants2.Insert
     (To_Unbounded_String ("e"), 2.718);

   --$ begin cut
   -- xxx2 are objects with the exact same content
   pragma Assert (Student_Per_Day = Student_Per_Day2);
   pragma Assert (Received_Parcels = Received_Parcels2);
   pragma Assert (Math_Constants = Math_Constants2);

   -- After changing the content, equality does not hold
   Student_Per_Day.Append (10);
   Received_Parcels.Insert ("Chronopost 13214GUU-035");
   Math_Constants.Insert (To_Unbounded_String ("G"), 9.8);

   pragma Assert (Student_Per_Day /= Student_Per_Day2);
   pragma Assert (Received_Parcels /= Received_Parcels2);
   pragma Assert (Math_Constants /= Math_Constants2);
   --$ end cut

   -------------
   -- Sorting --
   -------------

   Integer_Vectors_By_Positive.Sort
     (Student_Per_Day, Student_Per_Day.First_Index,
      Student_Per_Day.Last_Index);

   for Student_Count of Student_Per_Day loop
      Put_Line (Integer'Image (Student_Count));
   end loop;

   ------------
   -- Search --
   ------------

   declare
      --$ begin cut
      C : constant Pkg_Vectors.Cursor :=
        Student_Per_Day.Find (10);
      C2 : constant Pkg_Sets.Cursor :=
        Received_Parcels.Find ("UPS ZZ-44-I12");
      C3 : constant Pkg_Maps.Cursor :=
        Math_Constants.Find
          (To_Unbounded_String
             ("Pi")); -- Finds by the key!
      --$ end cut
   begin
      pragma Assert (C /= Pkg_Vectors.No_Element);
      pragma Assert (C2 /= Pkg_Sets.No_Element);
      pragma Assert (C3 /= Pkg_Maps.No_Element);
   end;

end Main;
