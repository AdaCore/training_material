with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with System;

--|database_begin
with Memory_Mgmt;

package body Database is

   type Variant_T is (I_Nteger, F_Loat, C_Haracter);
   type Record_T (Kind : Variant_T) is record
      case Kind is
         when I_Nteger =>
            I_Value : Short_Short_Integer;

         when F_Loat =>
            F_Value : Float;

         when C_Haracter =>
            C_Value : String (1 .. 10);
      end case;
   end record
   with Pack;
   type Record_Access_T is access Record_T;
   for Record_Access_T'Storage_Pool use Memory_Mgmt.Storage_Pool;
--|database_end

   type Index_T is range 1 .. 1_000;
   Values : array (Index_T) of Record_Access_T;
   Next   : Index_T := Index_T'First;

   procedure Insert (Element : Record_T) is
   begin
      Values (Next) := new Record_T'(Element);
      Values (Next).all := Element;
      Next := Next + 1;
   end Insert;

   procedure Free is new
     Ada.Unchecked_Deallocation (Record_T, Record_Access_T);

   procedure Delete (Element : Record_T) is
   begin
      for Idx in Values'First .. Next - 1 loop
         if Element = Values (Idx).all then
            Free (Values (Idx));
            Values (Idx .. Values'Last - 1) := Values (Idx + 1 .. Values'Last);
            Values (Values'Last) := null;
            Next := Next - 1;
            exit;
         end if;
      end loop;
   end Delete;

   procedure Add (I_Value : Integer) is
   begin
      Insert ((I_Nteger, Short_Short_Integer (I_Value)));
   end Add;

   procedure Add (F_Value : Float) is
   begin
      Insert ((F_Loat, F_Value));
   end Add;

   procedure Add (C_Value : Character) is
   begin
      Insert ((C_Haracter, (others => C_Value)));
   end Add;

   procedure Remove (I_Value : Integer) is
   begin
      Delete ((I_Nteger, Short_Short_Integer (I_Value)));
   end Remove;

   procedure Remove (F_Value : Float) is
   begin
      Delete ((F_Loat, F_Value));
   end Remove;

   procedure Remove (C_Value : Character) is
   begin
      Delete ((C_Haracter, (others => C_Value)));
   end Remove;

   procedure Print is
   begin
      for One of Values (Values'First .. Next - 1) loop
         case One.Kind is
            when F_Loat =>
               Put_Line (One.F_Value'Image);

            when I_Nteger =>
               Put_Line (One.I_Value'Image);

            when C_Haracter =>
               Put_Line (One.C_Value);
         end case;
      end loop;
   end Print;

end Database;
