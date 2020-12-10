
package body Solar_System.Graphics is

   function Get_X(Drawable : Visible_Body_Decorator_T) return Float is
   begin
      return Drawable.Object_Ptr.Get_X;
   end Get_X;

   function Get_Y(Drawable : Visible_Body_Decorator_T) return Float is
   begin
      return Drawable.Object_Ptr.Get_Y;
   end Get_Y;


   procedure Move (B : in out Visible_Orbiting_Body_T) is
   begin
      if B.Object_Ptr.all in Movable_I'Class then
         Movable_I'Class(B.Object_Ptr.all).Move;
      end if;
   end Move;

   function Create_Visible(B : access Orbiting_Body_T; Radius : Float; Color : RGBA_T) return access Visible_Orbiting_Body_T is
   begin
      return new Visible_Orbiting_Body_T'(Graphic    => (Radius => Radius, Color => Color),
                                        Object_Ptr => B);
   end Create_Visible;

   function Create_Visible(B : access Still_Body_T; Radius : Float; Color : RGBA_T) return access Visible_Still_Body_T is
   begin
      return new Visible_Still_Body_T'(Graphic    => (Radius => Radius, Color => Color),
                                         Object_Ptr => B);
   end Create_Visible;

   procedure Draw(Drawable : Visible_Body_Decorator_T; Canvas : Canvas_ID) is
   begin
         Draw_Sphere(Canvas   => Canvas,
                     Position => (Drawable.Object_Ptr.X, Drawable.Object_Ptr.Y, 0.0),
                     Radius   => Drawable.Graphic.Radius,
                     Color    => Drawable.Graphic.Color);
   end Draw;

   function Create_Visible(S : access Solar_System_T) return access Visible_Solar_System_T is
   begin
      return new Visible_Solar_System_T'(Object_Ptr => S);
   end Create_Visible;

   procedure Draw(Drawable : Visible_Solar_System_T; Canvas : Canvas_ID) is
   begin
      for B of Drawable.Object_Ptr.Still_Objects loop
         if Still_Body_I'Class(B.all) in Drawable_I'Class then
            Drawable_I'Class(B.all).Draw(Canvas);
         end if;
      end loop;
      for B of Drawable.Object_Ptr.Moving_Objects loop
         if Movable_I'Class(B.all) in Drawable_I'Class then
            Drawable_I'Class(B.all).Draw(Canvas);
         end if;
      end loop;
   end Draw;

   procedure Move (B : in out Visible_Solar_System_T) is
   begin
      B.Object_Ptr.Move;
   end Move;

   procedure Add_Still_Body(S : in out Visible_Solar_System_T; B : access Still_Body_I'Class) is
   begin
      S.Object_Ptr.Add_Still_Body(B);
   end Add_Still_Body;

   procedure Add_Moving_Body(S : in out Visible_Solar_System_T; B : access Movable_I'Class) is
   begin
      S.Object_Ptr.Add_Moving_Body(B);
   end Add_Moving_Body;


end Solar_System.Graphics;
