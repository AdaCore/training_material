--Flags_Spec

package body Flags is

--Flags_Body_1
   function Find (Map : Map_T;
                  Key : Key_T)
                  return Integer is
   begin
      for I in 1 .. Map.Length loop
         if Map.Values (I).Key = Key then
            return I;
         end if;
      end loop;
      return -1;
   end Find;

   procedure Add (Map         : in out Map_T;
                  Key         :        Key_T;
                  Description :        Colors.Color_Set_T;
                  Success     :    out Boolean) is
      Index : constant Integer := Find (Map, Key);
   begin
      Success := False;
      if Index not in Map.Values'Range then
         declare
            New_Item : constant Map_Component_T :=
              (Key         => Key,
               Description => Description);
         begin
            Map.Length              := Map.Length + 1;
            Map.Values (Map.Length) := New_Item;
            Success                 := True;
         end;
      end if;
   end Add;
   
   procedure Remove (Map     : in out Map_T;
                     Key     :        Key_T;
                     Success :    out Boolean) is
      Index : constant Integer := Find (Map, Key);
   begin
      Success := False;
      if Index in Map.Values'Range then
         Map.Values (Index .. Map.Length - 1) :=
           Map.Values (Index + 1 .. Map.Length);
         Success                              := True;
      end if;
   end Remove;
--Flags_Body_1
--Flags_Body_2
   procedure Modify (Map         : in out Map_T;
                     Key         :        Key_T;
                     Description :        Colors.Color_Set_T;
                     Success     :    out Boolean) is
      Index : constant Integer := Find (Map, Key);
   begin
      Success := False;
      if Index in Map.Values'Range then
         Map.Values (Index).Description := Description;
         Success                        := True;
      end if;
   end Modify;
   
   function Exists (Map : Map_T;
                    Key : Key_T)
                    return Boolean is
      (Find (Map, Key) in Map.Values'Range);
   
   function Get (Map : Map_T;
                 Key : Key_T)
                 return Map_Component_T is
      Index   : constant Integer := Find (Map, Key);
      Ret_Val : Map_Component_T;
   begin
      if Index in Map.Values'Range then
         Ret_Val := Map.Values (Index);
      end if;
      return Ret_Val;
   end Get;
   
   function Image (Item : Map_Component_T) return String is
     (Item.Key'Image & " => " & Colors.Image (Item.Description));
   
   function Image (Flag : Map_T) return String is
      Ret_Val : String (1 .. 1_000);
      Next    : Integer := Ret_Val'First;
   begin
      for I in 1 .. Flag.Length loop
         declare
            Item : constant Map_Component_T := Flag.Values (I);
            Str  : constant String          := Image (Item);
         begin
            Ret_Val (Next .. Next + Str'Length) := Image (Item) & ASCII.LF;
            Next                                := Next + Str'Length + 1;
         end;
      end loop;
      return Ret_Val (1 .. Next - 1);
   end Image;
--Flags_Body_2

end Flags;
