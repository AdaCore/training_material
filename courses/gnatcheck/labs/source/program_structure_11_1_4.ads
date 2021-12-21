package Program_Structure_11_1_4 is

   generic
      type T_Formal1_Type is private;
   package Level1 is
      procedure Proc (Param : in out T_Formal1_Type);

      generic
         type T_Formal2_Type is private;
      package Level2 is
         procedure Proc (Param : in out T_Formal2_Type);

         generic
            type T_Formal3_Type is private;
         package Level3 is
            procedure Proc (Param : in out T_Formal3_Type);

            generic
               type T_Formal4_Type is private;
            package Level4 is
               procedure Proc (Param : in out T_Formal4_Type);

               generic
                  type T_Formal5_Type is private;
               package Level5 is
                  procedure Proc (Param : in out T_Formal5_Type);
               end Level5;

            end Level4;

         end Level3;

      end Level2;

   end Level1;

end Program_Structure_11_1_4;
