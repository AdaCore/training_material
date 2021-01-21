with Constants;
with Ada.Text_IO; use Ada.Text_IO;
package body Datastore is

  subtype Valid_Range is
   Integer range Constants.Minimum_Value .. Constants.Maximum_Value;
  Attempt : Integer;
  Count   : Integer := Object'First;

begin

  loop
    Put ("Value: ");
    Attempt := Integer'Value (Ada.Text_IO.Get_Line);
    exit when Attempt not in Valid_Range;
    Object (Count) := Attempt;
    Count          := Count + 1;
  end loop;

  for I in Count .. Object'Last loop
    Object (I) := Constants.Invalid_Value;
  end loop;

end Datastore;
