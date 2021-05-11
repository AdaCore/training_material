
with Ada.Text_IO;
package body Line_Draw is

   type Matrix_T is array (Integer range <>, Integer range <>) of Character;

   function Rise
     (Line : Base_Types.Line_T)
      return Float is (Float (Line (2).Y_Coord - Line (1).Y_Coord));
   function Run
     (Line : Base_Types.Line_T)
      return Float is (Float (Line (2).X_Coord - Line (1).X_Coord));

   procedure Fill_Matrix_Vary_Y
     (Matrix : in out Matrix_T;
      Line   : in     Base_Types.Line_T;
      M      : in     Float;
      B      : in     Float) is
      X : Integer;
   begin
      for Y in Line (1).Y_Coord .. Line (2).Y_Coord
      loop
         X             := Integer ((Float (Y) - B) / M);
         Matrix (X, Y) := 'X';
      end loop;
   end Fill_Matrix_Vary_Y;

   procedure Fill_Matrix_Vary_X
     (Matrix : in out Matrix_T;
      Line   : in     Base_Types.Line_T;
      M      : in     Float;
      B      : in     Float) is
      Y : Integer;
   begin
      for X in Line (1).X_Coord .. Line (2).X_Coord
      loop
         Y             := Integer (M * Float (X) + B);
         Matrix (X, Y) := 'X';
      end loop;
   end Fill_Matrix_Vary_X;

   procedure Find_Slope_And_Intercept
     (Line     : in     Base_Types.Line_T;
      M        :    out Float;
      B        :    out Float;
      Vertical :    out Boolean) is
   begin
      if Run (Line) = 0.0
      then
         M        := 0.0;
         B        := 0.0;
         Vertical := True;
      else
         M        := Rise (Line) / Run (Line);
         B        := Float (Line (1).Y_Coord) - M * Float (Line (1).X_Coord);
         Vertical := False;
      end if;
   end Find_Slope_And_Intercept;

   procedure Fill_Matrix
     (Matrix : in out Matrix_T;
      Line   : in     Base_Types.Line_T) is
      M, B     : Float;
      Vertical : Boolean;
   begin
      Find_Slope_And_Intercept (Line, M, B, Vertical);
      if Vertical
      then
         for Y in
           Integer'Min (Line (1).Y_Coord, Line (2).Y_Coord) ..
             Integer'Max (Line (1).Y_Coord, Line (2).Y_Coord)
         loop
            Matrix (Line (1).X_Coord, Y) := 'X';
         end loop;
      elsif Rise (Line) > Run (Line)
      then
         Fill_Matrix_Vary_Y (Matrix, Line, M, B);
      else
         Fill_Matrix_Vary_X (Matrix, Line, M, B);
      end if;
   end Fill_Matrix;

   procedure Print (Object : Object_T'Class) is
      Lines        : Base_Types.Lines_T := Object.Convert;
      Max_X, Max_Y : Integer            := Integer'First;
      Min_X, Min_Y : Integer            := Integer'Last;
   begin
      for Line of Lines
      loop
         for Coord of Line
         loop
            Max_X := Integer'Max (Max_X, Coord.X_Coord);
            Min_X := Integer'Min (Min_X, Coord.X_Coord);
            Max_Y := Integer'Max (Max_Y, Coord.Y_Coord);
            Min_Y := Integer'Min (Min_Y, Coord.Y_Coord);
         end loop;
      end loop;
      declare
         Matrix : Matrix_T (Min_X .. Max_X, Min_Y .. Max_Y) :=
           (others => (others => ' '));
      begin
         for Line of Lines
         loop
            Fill_Matrix (Matrix, Base_Types.Ordered (Line));
         end loop;
         for Y in Matrix'Range (2)
         loop
            for X in Matrix'Range (1)
            loop
               Ada.Text_IO.Put (Matrix (X, Y));
            end loop;
            Ada.Text_IO.New_Line;
         end loop;
      end;
   end Print;

end Line_Draw;
