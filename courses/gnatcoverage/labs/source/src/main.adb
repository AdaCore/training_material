with Ada.Text_IO; use Ada.Text_IO;
with Inventory;
with Loading_Dock;
with Point_Of_Sale;

procedure Main is

   function Get
     (Prompt : String)
      return String is
   begin
      Put (Prompt & "? ");
      return Get_Line;
   end Get;

   function Get
     (Prompt : String)
      return Positive is
      Retval : constant String := Get (Prompt);
   begin
      return Positive'value (Retval);
   end Get;

   type Command_Type is (L_Oad, P_Rint, S_Ell, R_Eturn, Q_Uit);
begin

   loop
      for C in Command_Type'range
      loop
         Put (C'image & " ");
      end loop;
      Put ("? ");
      begin
         case Get_Line (1) is
            when 'l' | 'L' =>
               Loading_Dock.Load_From_Manifest (Get ("Filename"));
            when 'p' | 'P' =>
               Point_Of_Sale.Catalog (True);
            when 's' | 'S' =>
               Point_Of_Sale.Sell_Item (Get ("Item"), Get ("Quantity to buy"));
            when 'r' | 'R' =>
               Point_Of_Sale.Return_Item
                 (Get ("Item"), Get ("Quantity to return"));
            when 'q' | 'Q' =>
               exit;
            when others =>
               Ada.Text_IO.Put_Line ("Illegal command");
         end case;
      exception
         when Constraint_Error =>
            Ada.Text_IO.Put_Line ("Illegal quantity");
      end;
   end loop;

end Main;
