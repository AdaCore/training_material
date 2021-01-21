
package body Flags is

   procedure Add (Map         : in out Map_T;
                  Key         :        Key_T;
                  Description :        Colors.Color_Set_T;
                  Success     :    out Boolean) is
   begin
      Success := (for all Item of Map.Values
           (1 .. Map.Length) => Item.Key /= Key);
      if Success then
         declare
            New_Item : Map_Element_T :=
              (Key => Key, Description => Description);
         begin
            Map.Length              := Map.Length + 1;
            Map.Values (Map.Length) := New_Item;
         end;
      end if;
   end Add;
   procedure Remove (Map     : in out Map_T;
                     Key     :        Key_T;
                     Success :    out Boolean) is
   begin
      Success := False;
      for I in 1 .. Map.Length loop
         if Map.Values (I).Key = Key then
            Map.Values
              (I .. Map.Length - 1) := Map.Values
                (I + 1 .. Map.Length);
            Success := True;
            exit;
         end if;
      end loop;
   end Remove;
   procedure Modify (Map         : in out Map_T;
                     Key         :        Key_T;
                     Description :        Colors.Color_Set_T;
                     Success     :    out Boolean) is
   begin
      Success := False;
      for I in 1 .. Map.Length loop
         if Map.Values (I).Key = Key then
            Map.Values (I).Description := Description;
            Success                    := True;
            exit;
         end if;
      end loop;
   end Modify;
   function Exists (Map : Map_T; Key : Key_T) return Boolean is
      (for some Item of Map.Values (1 .. Map.Length) => Item.Key = Key);
   function Get (Map : Map_T; Key : Key_T) return Map_Element_T is
      Ret_Val : Map_Element_T;
   begin
      for I in 1 .. Map.Length loop
         if Map.Values (I).Key = Key then
            Ret_Val := Map.Values (I);
            exit;
         end if;
      end loop;
      return Ret_Val;
   end Get;
   function Image (Item : Map_Element_T) return String is
     (Key_T'Image (Item.Key) & " => " & Colors.Image (Item.Description));
   function Image (Flag : Map_T) return String is
      Ret_Val : String (1 .. 1_000);
      Next    : Integer := Ret_Val'First;
   begin
      for Item of Flag.Values (1 .. Flag.Length) loop
         declare
            Str : constant String := Image (Item);
         begin
            Ret_Val (Next .. Next + Str'Length) := Image (Item) & ASCII.LF;
            Next                          := Next + Str'Length + 1;
         end;
      end loop;
      return Ret_Val (1 .. Next - 1);
   end Image;

end Flags;
