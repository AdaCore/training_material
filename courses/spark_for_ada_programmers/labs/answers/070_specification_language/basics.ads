pragma Unevaluated_Use_Of_Old (Allow);

package Basics is

   type Rec (Disc : Boolean := False) is record
      case Disc is
         when True =>
            A : Integer;
         when False =>
            B : Integer;
      end case;
   end record;

   type Index is range 1 .. 10;
   type Table is array (Index range <>) of Integer;

   procedure Swap (X, Y : in out Integer)
     with Post =>
       (declare
          X_Old : constant Integer := X'Old;
          Y_Old : constant Integer := Y'Old;
        begin
          X = Y_Old and then Y = X_Old);

   function Value_Rec (R : Rec) return Integer is
     (if R.Disc then R.A else R.B);

   procedure Bump_Rec (R : in out Rec)
   with
     Pre  => Value_Rec (R) < Integer'Last,
     Post =>
       (if R.Disc then
          R = (R'Old with delta A => Value_Rec (R)'Old + 1)
        else
          R = (R'Old with delta B => Value_Rec (R)'Old + 1));

   procedure Swap_Table (T : in out Table; I, J : Index)
   with
     Pre  => I in T'Range and then J in T'Range,
     Post => T (I) = T (J)'Old and then T (J) = T (I)'Old
       and then (for all K in T'Range =>
                   (if K not in I | J then T (K) = T'Old (K)));

   function Value_Rec_Is_One (R : Rec) return Boolean is
     (Value_Rec (R) = 1);

   procedure Init_Rec (R : out Rec)
     with Post => Value_Rec_Is_One (R);

   function Constant_Value
     (T : Table; Start, Stop : Index; Value : Integer)
      return Boolean
   is
     (for all J in Start .. Stop => T (J) = Value)
   with
     Pre => Start > Stop or else (Start in T'Range and then Stop in T'Range);

   procedure Init_Table (T : out Table)
   with
     Pre  => T'Length >= 2,
     Post => T (T'First) = 1 and then T (T'Last) = 2
       and then Constant_Value (T, Start => T'First + 1, Stop => T'Last - 1, Value => 0);

end Basics;
