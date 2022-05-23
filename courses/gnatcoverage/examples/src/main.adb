with ada.text_io; use ada.text_io;
with Examples;
procedure Main is
   type test_procedure_t is access procedure (A, B, C :     Integer;
                                            Z       : out Integer);
   procedure test ( name : string;
                    a, b, c : integer;
                    proc : test_procedure_t ) is
      Z : Integer;
   begin
      put_line ( Name & " => " & a'image & ", " & b'image & ", " & c'image );
      begin
         proc ( a, b, c, z );
         begin
            put_line ( "   result: " & z'image );
         exception
            when others =>
               put_line ( "   result invalid" );
         end;
      exception
         when others =>
            put_line ( "   call failed" );
      end;
   end test;


begin
   test ( "Test_Statement", 1, 2, integer'last, examples.Test_Statement'access );
   test ( "Test_Decision True", 0, 0, 0, examples.Test_Decision'access );
   test ( "Test_Decision False", 1, 1, integer'last, examples.Test_Decision'access );
   test ( "Test_Mcdc A > 0, B = 0", 1, 0, 0, examples.Test_Mcdc'access );
   test ( "Test_Mcdc A = 0, B > 0", 0, 1, 0, examples.Test_Mcdc'access );
   test ( "Test_Mcdc A > 0, B > 0", 1, 1, 0, examples.Test_Mcdc'access );

end Main;
