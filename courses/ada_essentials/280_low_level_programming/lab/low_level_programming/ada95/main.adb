with Ada.Text_IO; use Ada.Text_IO;
with Messages;

procedure Main is
   Message : Messages.Message_T;
   function Get_Line return String is
      S : String (1 .. 100);
      L : Integer;
   begin
      Get_Line (S, L);
      return S (1 .. L);
   end Get_Line;
   function Command return Messages.Command_T is
   begin
      loop
         Put ("Command (");
         for E in Messages.Command_T loop
            Put (Messages.Command_T'Image (E) & " ");
         end loop;
         Put ("): ");
         begin
            return Messages.Command_T'Value (Get_Line);
         exception
            when others =>
               Put_Line ("Illegal");
         end;
      end loop;
   end Command;
   function Value return Positive is
   begin
      loop
         Put ("Value: ");
         begin
            return Positive'Value (Get_Line);
         exception
            when others =>
               Put_Line ("Illegal");
         end;
      end loop;
   end Value;
   function Text return String is
   begin
      Put ("Text: ");
      return Get_Line;
   end Text;

   procedure Create is
      C : constant Messages.Command_T := Command;
      V : constant Positive := Value;
      T : constant String := Text;
   begin
      Message := Messages.Create (Command => C, Value => V, Text => T);
   end Create;
   procedure Read is
      Valid : Boolean;
   begin
      Messages.Read (Message, Valid);
      Ada.Text_IO.Put_Line ("Message valid: " & Boolean'Image (Valid));
   end Read;
begin
   loop
      Put ("Create Write Read Print: ");
      declare
         Command : constant String := Get_Line;
      begin
         exit when Command'Length = 0;
         case Command (Command'First) is
            when 'c' | 'C' =>
               Create;

            when 'w' | 'W' =>
               Messages.Write (Message);

            when 'r' | 'R' =>
               Read;

            when 'p' | 'P' =>
               Messages.Print (Message);

            when others =>
               null;
         end case;
      end;
   end loop;
end Main;
