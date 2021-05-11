
package body Rectangle is

   procedure Set_Lines
     (Object : in out Object_T;
      Lines  :        Lines_T) is
   begin
      Object.Lines := Lines;
   end Set_Lines;

   function Lines
     (Object : Object_T)
      return Lines_T is (Object.Lines);

end Rectangle;
