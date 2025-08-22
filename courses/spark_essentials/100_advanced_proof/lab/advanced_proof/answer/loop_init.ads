pragma Unevaluated_Use_Of_Old (Allow);

with Loop_Types; use Loop_Types;
use Loop_Types.Vectors; use Loop_Types.Lists;

package Loop_Init is

   type Index is range 1 .. 10;
   type Table is array (Index range <>) of Integer;

   procedure Init_Table (T : out Table)
   with
     Relaxed_Initialization => T,
     Post => (for all J in T'Range => T(J) = 0);

   procedure Bump_Table (T : in out Table)
   with
     Pre  => (for all J in T'Range => T(J) < Integer'Last),
     Post => (for all J in T'Range => T(J) = T'Old(J) + 1);

   procedure Init_Vector (V : in out Vector)
   with
     Post => (for all J in V.First_Index .. V.Last_Index => V.Element (J) = 0);

   procedure Init_List (L : in out List)
   with
     Post => (for all E of L => E = 0);

end Loop_Init;
