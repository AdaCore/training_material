--|memory_tracking_begin
with Ada.Text_IO;
with Interfaces;

package body Memory_Mgmt is
   use System.Storage_Elements;
   use type System.Address;

   subtype Index_T is Storage_Count range 1 .. 20;
   Memory_Block : aliased array (Index_T) of Interfaces.Unsigned_8;
   Memory_Used  : array (Index_T) of Boolean := (others => False);

   function Storage_Size (Pool : Storage_Pool_T) return Storage_Count is
   begin
      return Storage_Count (Memory_Block'Size);
   end Storage_Size;

   function Storage_Used return Storage_Count is
      Retval : Storage_Count := 0;
   begin
      for Is_In_Use of Memory_Used loop
         if Is_In_Use then
            Retval := Retval + 1;
         end if;
      end loop;
      return Retval;
   end Storage_Used;

   procedure Set_In_Use (Start : Index_T; Length : Storage_Count) is
   begin
      Ada.Text_IO.Put_Line
        (Start'Image & Length'Image & Memory_Block'Length'Image);
      if Integer (Start) + Integer (Length) > Memory_Block'Length then
         raise Storage_Error;
      end if;
      for I in 0 .. Length - 1 loop
         Memory_Used (Start + I) := True;
      end loop;
   end Set_In_Use;

   function Find_Unused return Index_T is
   begin
      for Index in Memory_Used'Range loop
         if not Memory_Used (Index) then
            return Index;
         end if;
      end loop;
      raise Storage_Error;
   end Find_Unused;
--|memory_tracking_end

--|memory_apis_begin
   procedure Allocate
     (Pool                     : in out Storage_Pool_T;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
      Index : Storage_Count := Find_Unused;
   begin
      Storage_Address := Memory_Block (Index)'Address;
      Set_In_Use (Index, Size_In_Storage_Elements);
      if Storage_Used > Memory_Block'Last * 8 / 10 then
         Ada.Text_IO.Put_Line ("WARNING: approaching memory limit");
      end if;
   end Allocate;

   procedure Deallocate
     (Pool                     : in out Storage_Pool_T;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count) is
   begin
      null;  -- Memory-safe - no deallocation!
   end Deallocate;

   procedure Print_Info is
   begin
      Ada.Text_IO.Put_Line ("Memory in use: " & Storage_Used'Image);
   end Print_Info;
--|memory_apis_end

end Memory_Mgmt;
