with My_Button; use My_Button;

package body Solar_System.Button is

   task body Button_Monitor is
      Body_Data : Body_Type;
   begin
      loop
         My_Button.Button.Wait_Press;

         for PO_Body of Bodies loop
            Body_Data := PO_Body.Get_Data;
            Body_Data.Speed := -Body_Data.Speed;
            PO_Body.Set_Data (Body_Data);
         end loop;
      end loop;
   end Button_Monitor;

end Solar_System.Button;
