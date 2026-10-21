package Advanced is

   procedure Visible1;
   procedure Visible2;
   procedure Visible3;

   protected Protected_Object is
      function Get return Integer;
      procedure Set (Value : Integer);
   private
      Global : Integer := 0;
   end Protected_Object;

end Advanced;
