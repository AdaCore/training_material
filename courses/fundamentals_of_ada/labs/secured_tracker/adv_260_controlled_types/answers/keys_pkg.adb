package body Keys_Pkg is
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

   function In_Use return Natural is
      Ret_Val : Natural := 0;
   begin
      for Flag of Global_In_Use loop
         Ret_Val := Ret_Val + (if Flag then 1 else 0);
      end loop;
      return Ret_Val;
   end In_Use;

   function Generate return Key_T is
   begin
      return X : Key_T;
   end Generate;

   procedure Destroy (Key : Key_T) is
   begin
      Global_In_Use (Key.Value) := False;
   end Destroy;

   function Image (Key : Key_T) return String is
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

end Keys_Pkg;
