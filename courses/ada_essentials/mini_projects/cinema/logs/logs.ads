package Logs is
   
   type Style is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White, Dummy, None,
                  Black_On_Red, White_On_Red);
   
   procedure Buffer;
   procedure Unbuffer;
   procedure Put_Buffer;
   procedure Drop_Buffer;
   
   procedure Put (S : String);
   procedure Put_Line (S : String);
   procedure Put (Sty : Style; S : String);
   procedure Put_Line (Sty : Style; S : String);
   procedure New_Line;
   
end Logs;
