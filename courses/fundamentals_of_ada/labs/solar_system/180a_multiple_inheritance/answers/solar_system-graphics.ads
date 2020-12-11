with Display.Basic; use Display.Basic;
with Display; use Display;

package Solar_System.Graphics is

   type Drawable_I is interface;
   procedure Draw(Drawable : Drawable_I; Canvas : Canvas_ID) is abstract;
 
   type Visible_Body_Decorator_T is abstract new Drawable_I and Still_Body_I with private;
   procedure Draw(Drawable : Visible_Body_Decorator_T; Canvas : Canvas_ID);
   function Get_X(Drawable : Visible_Body_Decorator_T) return Float;
   function Get_Y(Drawable : Visible_Body_Decorator_T) return Float;
   
   type Visible_Orbiting_Body_T is new Visible_Body_Decorator_T and Movable_I with private;
   procedure Move (B : in out Visible_Orbiting_Body_T);   
   function Create_Visible(B : access Orbiting_Body_T; Radius : Float; Color : RGBA_T) return access Visible_Orbiting_Body_T;
   
   type Visible_Still_Body_T is new Visible_Body_Decorator_T with private;
   function Create_Visible(B : access Still_Body_T; Radius : Float; Color : RGBA_T) return access Visible_Still_Body_T;
   
   type Visible_Solar_System_T is new Drawable_I and Solar_System_I with private;
   function Create_Visible(S : access Solar_System_T) return access Visible_Solar_System_T;
   procedure Add_Still_Body(S : in out Visible_Solar_System_T; B : access Still_Body_I'Class);
   procedure Add_Moving_Body(S : in out Visible_Solar_System_T; B : access Movable_I'Class);
   procedure Draw(Drawable : Visible_Solar_System_T; Canvas : Canvas_ID);
   procedure Move (B : in out Visible_Solar_System_T);   
   
private

   type Sphere_Type is record
      Radius      : Float;
      Color       : RGBA_T;
   end record;
   
   type Visible_Body_Decorator_T is abstract new Drawable_I and Still_Body_I  with record
      Graphic : Sphere_Type;
      Object_Ptr : access Body_Base_T'Class;
   end record;
   
   type Visible_Orbiting_Body_T is new Visible_Body_Decorator_T and Movable_I with null record;
      
   type Visible_Still_Body_T is new Visible_Body_Decorator_T with null record;
  
   type Visible_Solar_System_T is new Drawable_I and Solar_System_I with record
      Object_Ptr : access Solar_System_T;
   end record;

end Solar_System.Graphics;
