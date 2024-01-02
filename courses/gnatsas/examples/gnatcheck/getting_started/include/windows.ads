--::::::::::
--windows.ads
--::::::::::
with Screen;
package Windows is

  -- manager for simple, nonoverlapping screen windows
  -- Michael Feldman, The George Washington University
  -- July, 1995

  type Window is private;

  function Open (UpperLeft: Screen.Position;
                 Height   : Screen.Height;
                 Width    : Screen.Width) return Window;
  -- Pre:  W, Height, and Width are defined
  -- Post: returns a Window with the given upper-left corner,
  --   height, and width

  procedure Title (W     : in out Window;
                   Name  : in String;
                   Under : in Character);
  -- Pre:  W, Name, and Under are defined
  -- Post: Name is displayed at the top of the window W, underlined
  -- with the character Under. 

  procedure Borders (W                    : in out Window;
                     Corner, Down, Across : in Character);
  -- Pre:  All parameters are defined
  -- Post: Draw border around current writable area in window with 
  -- characters specified.  Call this BEFORE Title.  

  procedure MoveCursor (W : in out Window;
                        P : in Screen.Position);
  -- Pre:  W and P are defined, and P lies within the area of W
  -- Post: Cursor is moved to the specified position.
  --   Coordinates are relative to the
  --   upper left corner of W, which is (1, 1) 

  procedure Put (W  : in out Window;
                 Ch : in Character);
  -- Pre:  W and Ch are defined.
  -- Post: Ch is displayed in the window at 
  --   the next available position.
  --   If end of column, go to the next row.
  --   If end of window, go to the top of the window. 

  procedure Put (W : in out Window;
                 S : in String);
  -- Pre:  W and S are defined
  -- Post: S is displayed in the window, "line-wrapped" if necessary

  procedure New_Line (W : in out Window);
  -- Pre:  W is defined
  -- Post: Cursor moves to beginning of next line of W;
  --   line is not blanked until next character is written  

private
  type Window is record
    First  : Screen.Position; -- coordinates of upper left
    Last   : Screen.Position; -- coordinates of lower right
    Current: Screen.Position; -- current cursor position
  end record;

end Windows;
