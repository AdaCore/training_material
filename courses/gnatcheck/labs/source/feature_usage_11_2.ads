with Ada.Finalization;
package Feature_Usage_11_2 is

   type T_Abstract_Type is abstract tagged null record;
   type T_Controlled_Type is new Ada.Finalization.Controlled with null record;

   type T_Array_Type is array (1 .. 2) of Integer;
   Array_Object : T_Array_Type := (1 => 1, 2 => 2);

   procedure Complex_Inlined (Flag : in out Integer);
   pragma Inline (Complex_Inlined);

end Feature_Usage_11_2;
