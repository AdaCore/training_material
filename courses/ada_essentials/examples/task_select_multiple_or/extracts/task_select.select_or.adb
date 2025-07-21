loop
   select
      accept Receive_Message (V : String) do
         Put_Line ("Select_Loop_Task Receive: " & V);
      end Receive_Message;
   or
      accept Send_Message (V : String) do
         Put_Line ("Select_Loop_Task Send: " & V);
      end Send_Message;
   or when Termination_Flag =>
      accept Stop;
   or
      delay 0.5;
      Put_Line
        ("No more waiting at" & Day_Duration'Image (Seconds (Clock)));
      exit;
   end select;
end loop;
