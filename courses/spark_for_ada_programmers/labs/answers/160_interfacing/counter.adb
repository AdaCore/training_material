with System.Storage_Elements;

package body Counter
  with SPARK_Mode
is

   procedure Bump_And_Monitor (Alarm : out Boolean)
   is
      Calc : Integer;
   begin
      Calc := Port;
      Alarm := Limit < Calc;
   end Bump_And_Monitor;

end Counter;
