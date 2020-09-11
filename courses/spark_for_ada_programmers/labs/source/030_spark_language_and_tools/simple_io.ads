package Simple_IO
  with SPARK_Mode => On
is

   procedure Put_Line (S : in String)
     with Global => null;

   procedure Put_Line (S : in Integer)
     with Global => null;
   
end Simple_IO;
