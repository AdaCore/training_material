--::::::::::
--screen.adb
--::::::::::
with Text_IO;
package body Screen is

  -- simple ANSI terminal emulator
  -- Michael Feldman, The George Washington University
  -- July, 1995

  -- These procedures will work correctly only if the actual
  -- terminal is ANSI compatible. ANSI.SYS on a DOS machine
  -- will suffice.

  package Int_IO is new Text_IO.Integer_IO (Num => Integer);

  procedure Beep is
  begin
    Text_IO.Put (Item => ASCII.BEL);
  end Beep;

  procedure ClearScreen is
  begin
    Text_IO.Put (Item => ASCII.ESC);
    Text_IO.Put (Item => "[2J");
  end ClearScreen;

  procedure MoveCursor (To: in Position) is
  begin                                                
    Text_IO.New_Line;
    Text_IO.Put (Item => ASCII.ESC);
    Text_IO.Put ("[");
    Int_IO.Put (Item => To.Row, Width => 1);
    Text_IO.Put (Item => ';');
    Int_IO.Put (Item => To.Column, Width => 1);
    Text_IO.Put (Item => 'f');
  end MoveCursor;  

end Screen;
