package body Keys_Pkg is
--$ begin answer
   Global_In_Use : array (Character range 'a' .. 'z') of Boolean :=
     (others => False);

   pragma Warnings ( Off );

   function Next_Available return Character is
   begin
      for C in Global_In_Use'Range loop
         if not Global_In_Use (C) then
            return C;
         end if;
      end loop;
      -- we ran out of keys! exception if we get here
   end Next_Available;
   pragma Warnings ( On );
--$ end answer

   function In_Use return Natural is
--$ line question
     (0);
--$ begin answer
      Ret_Val : Natural := 0;
   begin
      for Flag of Global_In_Use loop
         Ret_Val := Ret_Val + (if Flag then 1 else 0);
      end loop;
      return Ret_Val;
   end In_Use;
--$ end answer

   function Generate return Key_T is
--$ line question
     ((others => <>));
--$ begin answer
   begin
      return X : Key_T;
   end Generate;
--$ end answer

   procedure Destroy (Key : Key_T) is
--$ line question
     null;
--$ begin answer
   begin
      Global_In_Use (Key.Value) := False;
   end Destroy;
--$ end answer

   function Image (Key : Key_T) return String is
--$ line question
     ("");
--$ begin answer
      ( "KEY: " & Key.Value );

   procedure Initialize (Key : in out Key_T) is
   begin
      Key.Value                 := Next_Available;
      Global_In_Use (Key.Value) := True;
   end Initialize;

   procedure Finalize (Key : in out Key_T) is
   begin
      Global_In_Use (Key.Value) := False;
   end Finalize;
--$ end answer

end Keys_Pkg;
