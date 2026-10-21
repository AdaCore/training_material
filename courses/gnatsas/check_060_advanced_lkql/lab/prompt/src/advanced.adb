package body Advanced is

   procedure Visible1 is
      procedure Nested1 is
      begin -- Nested1
         null;
         null;
         null;
         null;
         null;
         null;
         null;
         null;
         null;
         null;
         null;
      end Nested1;
      procedure Nested2 is
      begin -- Nested2
         null;
      end Nested2;
   begin -- Visible1
      null;
   end Visible1;

   procedure Visible2 is
      procedure Nested1 is
         procedure Nesteda is
         begin -- NestedA
            null;
         end Nesteda;
         procedure Nestedb is
         begin -- NestedB
            null;
            null;
            null;
            null;
            null;
            null;
            null;
         end Nestedb;
      begin -- Nested1
         null;
      end Nested1;
      procedure Nested2 is
         procedure Nesteda is
         begin -- NestedA
            null;
         end Nesteda;
         procedure Nestedb is
         begin -- NestedB
            null;
         end Nestedb;
      begin -- Nested2
         null;
      end Nested2;
   begin -- Visible2
      null;
   end Visible2;

   procedure Visible3 is
      procedure Nested1 is
         procedure Nesteda is
            procedure Nestedx is
            begin -- NestedX
               null;
            end Nestedx;
            procedure Nestedy is
            begin -- NestedY
               null;
            end Nestedy;
         begin -- NestedA
            null;
         end Nesteda;
         procedure Nestedb is
            procedure Nestedx is
            begin -- NestedX
               null;
               null;
               null;
               null;
               null;
               null;
               null;
            end Nestedx;
            procedure Nestedy is
            begin -- NestedY
               null;
            end Nestedy;
         begin -- NestedB
            null;
         end Nestedb;
      begin -- Nested1
         null;
      end Nested1;
      procedure Nested2 is
         procedure Nesteda is
            procedure Nestedx is
            begin -- NestedX
               null;
            end Nestedx;
            procedure Nestedy is
            begin -- NestedY
               null;
            end Nestedy;
         begin -- NestedA
            null;
         end Nesteda;
         procedure Nestedb is
            procedure Nestedx is
            begin -- NestedX
               null;
            end Nestedx;
            procedure Nestedy is
            begin -- NestedY
               null;
            end Nestedy;
         begin -- NestedB
            null;
         end Nestedb;
      begin -- Nested2
         null;
      end Nested2;
   begin -- Visible3
      null;
   end Visible3;

   protected body Protected_Object is
      function Get return Integer is
      begin
         return Global;
      end Get;

      procedure Set (Value : Integer) is
         procedure Nested1 is
         begin -- Nested1
            Global := Global + Value;
         end Nested1;
         procedure Nested2 is
         begin -- Nested2
            Global := Global + Value;
         end Nested2;
      begin
         Nested1;
         Nested2;
      end Set;
   end Protected_Object;

end Advanced;
