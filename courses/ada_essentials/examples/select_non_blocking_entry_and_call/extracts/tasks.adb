select
   accept Receive_Message (V : String) do
      Put_Line ("T: Receive " & V);
   end Receive_Message;
else
   Put_Line ("T: Nothing received");
end select;
