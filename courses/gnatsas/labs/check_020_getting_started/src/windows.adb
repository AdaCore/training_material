--::::::::::
--windows.adb
--::::::::::
with Text_IO, Screen;
package body Windows is

  -- manager for simple, nonoverlapping screen windows
  -- Michael Feldman, The George Washington University
  -- July, 1995

  function Open (UpperLeft: Screen.Position;
                 Height   : Screen.Height;
                 Width    : Screen.Width) return Window is
    Result: Window;
  begin
    Result.Current:= UpperLeft;
    Result.First  := UpperLeft;
    Result.Last   := (Row    => UpperLeft.Row + Height - 1, 
                      Column => UpperLeft.Column + Width - 1);
    return Result; 
  end Open;

  procedure EraseToEndOfLine (W : in out Window) is
  begin
    Screen.MoveCursor (W.Current);
    for Count in W.Current.Column .. W.Last.Column loop
      Text_IO.Put (' ');
    end loop;
    Screen.MoveCursor (W.Current);
  end EraseToEndOfLine;

  procedure Put (W  : in out Window;
                 Ch : in CHARACTER) is
  begin

    -- If at end of current line, move to next line 
    if W.Current.Column > W.Last.Column then
      if W.Current.Row = W.Last.Row then
        W.Current.Row := W.First.Row;
      else
        W.Current.Row := W.Current.Row + 1;
      end IF;
      W.Current.Column := W.First.Column;
    end IF;

    -- If at First char, erase line
    if W.Current.Column = W.First.Column then
      EraseToEndOfLine (W);
    end IF;

    Screen.MoveCursor (To => W.Current);

     -- here is where we actually write the character!
     Text_IO.Put (Ch);
     W.Current.Column := W.Current.Column + 1;
 
  end Put;

  procedure Put (W : in out Window;
                 S : in String) is
  begin
    for Count in S'Range loop
      Put (W, S (Count));
    end loop;
  end Put;

  procedure New_Line (W : in out Window) is
  begin
    if W.Current.Column = 1 then
      EraseToEndOfLine (W);
    end IF;
    if W.Current.Row = W.Last.Row then
      W.Current.Row := W.First.Row;
    else
      W.Current.Row := W.Current.Row + 1;
    end IF;
    W.Current.Column := W.First.Column;
  end New_Line;

  procedure Title (W     : in out Window;
                   Name  : in String;
                   Under : in Character) is
  begin
    -- Put name on top line
    W.Current := W.First;
    Put (W, Name);
    New_Line (W);
    -- Underline name if desired, and reduce the writable area
    -- of the window by one line
    if Under = ' ' then   -- no underlining
      W.First.Row := W.First.Row + 1;      
    else                  -- go across the row, underlining
      for Count in W.First.Column..W.Last.Column loop 
        Put (W, Under);
      end loop;
      New_Line (W);
      W.First.Row := W.First.Row + 2; -- reduce writable area
    end IF;
  end Title;
 
  procedure Borders (W                    : in out Window;
                     Corner, Down, Across : in Character) is
  begin
    -- Put top line of border
    Screen.MoveCursor (W.First);
    Text_IO.Put (Corner);
    for Count in W.First.Column + 1 .. W.Last.Column - 1 loop
      Text_IO.Put (Across);
    end loop;
    Text_IO.Put (Corner);

    -- Put the two side lines
    for Count in W.First.Row + 1 .. W.Last.Row - 1 loop
      Screen.MoveCursor ((Row => Count, Column => W.First.Column));
      Text_IO.Put (Down);
      Screen.MoveCursor ((Row => Count, Column => W.Last.Column));
      Text_IO.Put (Down);
    end loop;

    -- Put the bottom line of the border
    Screen.MoveCursor ((Row => W.Last.Row, Column => W.First.Column));
    Text_IO.Put (corner);
    for Count in W.First.Column + 1 .. W.Last.Column - 1 loop
      Text_IO.Put (Across);
    end loop;
    Text_IO.Put (Corner);

    -- Make the Window smaller by one character on each side
    W.First  := (Row => W.First.Row  + 1, Column => W.First.Column  + 1);
    W.Last   := (Row => W.Last.Row - 1,   Column => W.Last.Column - 1);
    W.Current    := W.First;
  end Borders;

  procedure MoveCursor (W : in out Window;
                        P : in Screen.Position) is
    -- Relative to writable Window boundaries, of course
  begin
    W.Current.Row    := W.First.Row + P.Row;
    W.Current.Column := W.First.Column + P.Column;
  end MoveCursor;

begin

  Text_IO.New_Line;
  Screen.ClearScreen;
  Text_IO.New_Line;

end Windows;
