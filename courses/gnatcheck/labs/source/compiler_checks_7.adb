with Ada.Containers;
package body Compiler_Checks_7 is

   Global1 : Integer := 1_234;

   task type T_Task_Type is
      entry Start;
   end T_Task_Type;

   procedure Proc (Param : in out Integer) is
      type A_Access_Type is access T_Task_Type;
      T : A_Access_Type;
   begin

      Param := Param + Global1;
      if Param = 111 then
         T := new T_Task_Type;
      end if;

   end;

   task body T_Task_Type is
   begin
      accept Start;
   end T_Task_Type;

end Compiler_Checks_7;
