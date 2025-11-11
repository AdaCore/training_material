with Ada.Text_IO;
with Interfaces;

package body Memory_Mgmt is
   use System.Storage_Elements;
   use type System.Address;

   --|objects_begin
   subtype Index_T is Storage_Count range 1 .. 1_000;
   Memory_Block : aliased array (Index_T)
      of Interfaces.Unsigned_8;
   Memory_Used  : array (Index_T) of Boolean :=
      (others => False);

   Current_Water_Mark : Storage_Count := 0;
   High_Water_Mark    : Storage_Count := 0;
   --|objects_end

   --|set_in_use_begin
   procedure Set_In_Use
     (Start : Index_T; Length : Storage_Count; Used : Boolean) is
   begin
      for I in 0 .. Length - 1 loop
         Memory_Used (Start + I) := Used;
      end loop;
      if Used then
         Current_Water_Mark := Current_Water_Mark + Length;
         High_Water_Mark :=
           Storage_Count'Max (High_Water_Mark, Current_Water_Mark);
      else
         Current_Water_Mark := Current_Water_Mark - Length;
      end if;
   end Set_In_Use;
   --|set_in_use_end

   --|find_free_block_begin
   function Find_Free_Block (Length : Storage_Count) return Index_T is
      Consecutive : Storage_Count := 0;
   begin
      for I in Memory_Used'Range loop
         if Memory_Used (I) then
            Consecutive := 0;
         else
            Consecutive := Consecutive + 1;
            if Consecutive >= Length then
               return I;
            end if;
         end if;
      end loop;
      raise Storage_Error;
   end Find_Free_Block;
   --|find_free_block_end

   --|external_apis_begin
   procedure Allocate
     (Pool                     : in out Storage_Pool_T;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
      Index : Storage_Count := Find_Free_Block (Size_In_Storage_Elements);
   begin
      Storage_Address := Memory_Block (Index)'Address;
      Set_In_Use (Index, Size_In_Storage_Elements, True);
   end Allocate;

   procedure Deallocate
     (Pool                     : in out Storage_Pool_T;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count) is
   begin
      for I in Memory_Block'Range loop
         if Memory_Block (I)'Address = Storage_Address then
            Set_In_Use (I, Size_In_Storage_Elements, False);
         end if;
      end loop;
   end Deallocate;
   --|external_apis_end

   --|diagnostics_begin
   function Storage_Size
     (Pool : Storage_Pool_T)
      return System.Storage_Elements.Storage_Count is
   begin
      return Current_Water_Mark;
   end Storage_Size;

   procedure Print_Info is
   begin
      Ada.Text_IO.Put_Line
        ("Current Water Mark: " &
         Storage_Count'Image (Current_Water_Mark));
      Ada.Text_IO.Put_Line
        ("High Water Mark: " &
         Storage_Count'Image (High_Water_Mark));
   end Print_Info;
   --|diagnostics_end

end Memory_Mgmt;
