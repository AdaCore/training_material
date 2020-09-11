--  This code is a subset of ADA.STRINGS.FIXED implementation in GNAT

with Ada.Strings.Maps; use Ada.Strings.Maps;

package body String_Fixed with SPARK_Mode is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Belongs
     (Element : Character;
      Set     : Maps.Character_Set;
      Test    : Membership) return Boolean;
   pragma Inline (Belongs);
   --  Determines if the given element is in (Test = Inside) or not in
   --  (Test = Outside) the given character set.

   -------------
   -- Belongs --
   -------------

   function Belongs
     (Element : Character;
      Set     : Maps.Character_Set;
      Test    : Membership) return Boolean
   is
   begin
      if Test = Inside then
         return Is_In (Element, Set);
      else
         return not Is_In (Element, Set);
      end if;
   end Belongs;

   ------------------------
   -- Search Subprograms --
   ------------------------

   -----------
   -- Index --
   -----------

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural
   is
   begin
      --  Forwards case

      if Going = Forward then
         for J in Source'Range loop
            if Belongs (Source (J), Set, Test) then
               return J;
            end if;
         end loop;

      --  Backwards case

      else
         for J in reverse Source'Range loop
            if Belongs (Source (J), Set, Test) then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match

      return 0;
   end Index;

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural
   is
   begin

      --  AI05-056 : if source is empty result is always 0.

      if Source'Length = 0 then
         return 0;

      elsif Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return
           Index (Source (From .. Source'Last), Set, Test, Forward);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return
           Index (Source (Source'First .. From), Set, Test, Backward);
      end if;
   end Index;

   ---------------------
   -- Index_Non_Blank --
   ---------------------

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         for J in Source'Range loop
            if Source (J) /= ' ' then
               return J;
            end if;
         end loop;

      else -- Going = Backward
         for J in reverse Source'Range loop
            if Source (J) /= ' ' then
               return J;
            end if;
         end loop;
      end if;

      --  Fall through if no match

      return 0;
   end Index_Non_Blank;

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural
   is
   begin
      if Going = Forward then
         if From < Source'First then
            raise Index_Error;
         end if;

         return
           Index_Non_Blank (Source (From .. Source'Last), Forward);

      else
         if From > Source'Last then
            raise Index_Error;
         end if;

         return
           Index_Non_Blank (Source (Source'First .. From), Backward);
      end if;
   end Index_Non_Blank;

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Natural;
      Right : Character) return String
   is
      Result : String (1 .. Left);

   begin
      for J in Result'Range loop
         Result (J) := Right;
      end loop;

      return Result;
   end "*";

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural) return String
   is
   begin
      if From > Through then
         declare
            subtype Result_Type is String (1 .. Source'Length);

         begin
            return Result_Type (Source);
         end;

      elsif From not in Source'Range
        or else Through > Source'Last
      then
         raise Index_Error;

      else
         declare
            Front  : constant Integer := From - Source'First;
            Result : String (1 .. Source'Length - (Through - From + 1));

         begin
            Result (1 .. Front) :=
              Source (Source'First .. From - 1);
            Result (Front + 1 .. Result'Last) :=
              Source (Through + 1 .. Source'Last);

            return Result;
         end;
      end if;
   end Delete;

   ----------
   -- Head --
   ----------

   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   is
      subtype Result_Type is String (1 .. Count);

   begin
      if Count < Source'Length then
         return
           Result_Type (Source (Source'First .. Source'First + Count - 1));

      else
         declare
            Result : Result_Type;

         begin
            Result (1 .. Source'Length) := Source;

            for J in Source'Length + 1 .. Count loop
               Result (J) := Pad;
            end loop;

            return Result;
         end;
      end if;
   end Head;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String) return String
   is
      Result : String (1 .. Source'Length + New_Item'Length);
      Front  : constant Integer := Before - Source'First;

   begin
      if Before not in Source'First .. Source'Last + 1 then
         raise Index_Error;
      end if;

      Result (1 .. Front) :=
        Source (Source'First .. Before - 1);
      Result (Front + 1 .. Front + New_Item'Length) :=
        New_Item;
      Result (Front + New_Item'Length + 1 .. Result'Last) :=
        Source (Before .. Source'Last);

      return Result;
   end Insert;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String) return String
   is
   begin
      if Position not in Source'First .. Source'Last + 1 then
         raise Index_Error;
      end if;

      declare
         Result_Length : constant Natural :=
           Integer'Max
             (Source'Length,
              Position - Source'First + New_Item'Length);

         Result : String (1 .. Result_Length);
         Front  : constant Integer := Position - Source'First;

      begin
         Result (1 .. Front) :=
           Source (Source'First .. Position - 1);
         Result (Front + 1 .. Front + New_Item'Length) :=
           New_Item;
         Result (Front + New_Item'Length + 1 .. Result'Length) :=
           Source (Position + New_Item'Length .. Source'Last);
         return Result;
      end;
   end Overwrite;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String
   is
      subtype Result_Type is String (1 .. Count);

   begin
      if Count < Source'Length then
         return Result_Type (Source (Source'Last - Count + 1 .. Source'Last));

      --  Pad on left

      else
         declare
            Result : Result_Type;

         begin
            for J in 1 .. Count - Source'Length loop
               Result (J) := Pad;
            end loop;

            Result (Count - Source'Length + 1 .. Count) := Source;
            return Result;
         end;
      end if;
   end Tail;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : String;
      Side   : Trim_End) return String
   is
   begin
      case Side is
         when Ada.Strings.Left =>
            declare
               Low : constant Natural := Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  return "";
               end if;

               declare
                  subtype Result_Type is String (1 .. Source'Last - Low + 1);
               begin
                  return Result_Type (Source (Low .. Source'Last));
               end;
            end;

         when Ada.Strings.Right =>
            declare
               High : constant Natural := Index_Non_Blank (Source, Backward);
            begin
               --  All blanks case

               if High = 0 then
                  return "";
               end if;

               declare
                  subtype Result_Type is String (1 .. High - Source'First + 1);
               begin
                  return Result_Type (Source (Source'First .. High));
               end;
            end;

         when Ada.Strings.Both =>
            declare
               Low : constant Natural := Index_Non_Blank (Source, Forward);
            begin
               --  All blanks case

               if Low = 0 then
                  return "";
               end if;

               declare
                  High : constant Natural :=
                    Index_Non_Blank (Source, Backward);
                  subtype Result_Type is String (1 .. High - Low + 1);
               begin
                  return Result_Type (Source (Low .. High));
               end;
            end;
      end case;
   end Trim;

   function Trim
     (Source : String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return String
   is
      High, Low : Integer;

   begin
      Low := Index (Source, Set => Left, Test  => Outside, Going => Forward);

      --  Case where source comprises only characters in Left

      if Low = 0 then
         return "";
      end if;

      High :=
        Index (Source, Set => Right, Test  => Outside, Going => Backward);

      --  Case where source comprises only characters in Right

      if High = 0 then
         return "";
      end if;

      declare
         Lgth : constant Integer := High - Low + 1;
         subtype Result_Type is String (1 .. Lgth);

      begin
         return Result_Type (Source (Low .. High));
      end;
   end Trim;

end String_Fixed;
