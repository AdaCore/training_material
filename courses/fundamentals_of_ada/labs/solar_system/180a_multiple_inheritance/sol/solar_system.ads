with Ada.Containers.Bounded_Vectors;
package Solar_System is

   type Orbit_Ref_I is interface;
   function Get_X(O : Orbit_Ref_I) return Float is abstract;
   function Get_Y(O : Orbit_Ref_I) return Float is abstract;
   
   type Movable_I is interface; 
   type Moving_Access_I is access all Movable_I;

   procedure Move (B : in out Movable_I) is abstract;   
   
   type Orbiting_Body_I is interface and Movable_I and Orbit_Ref_I;
   
   type Orbiting_Body_T is new Orbiting_Body_I with private;
   
   procedure Move (B : in out Orbiting_Body_T);   
   function Create_Orbiting(Distance : Float; Speed: Float; Angle: Float; Turns_Around : access Orbit_Ref_I'Class) return access Orbiting_Body_T;
   
   
   type Still_Body_I is interface and Orbit_Ref_I;
   
   type Still_Body_Access_I is access all Still_Body_I;
   
   type Still_Body_T is new Still_Body_I with private;
 
   function Create_Still(X : Float; Y: Float) return access Still_Body_T;
 
   
   type Solar_System_I is interface and Movable_I;
   procedure Add_Still_Body(S : in out Solar_System_I; B : access Still_Body_I'Class) is abstract;
   procedure Add_Moving_Body(S : in out Solar_System_I; B : access Movable_I'Class) is abstract;
   
   
   type Solar_System_T is new Solar_System_I with private;
   function Create_Solar_System return access Solar_System_T;
   procedure Add_Still_Body(S : in out Solar_System_T; B : access Still_Body_I'Class);
   procedure Add_Moving_Body(S : in out Solar_System_T; B : access Movable_I'Class);
   
   procedure Move (S : in out Solar_System_T);   
  
private
   type Body_Base_T is new Orbit_Ref_I with record 
      X            : Float;
      Y            : Float;
   end record;
   function Get_X(O : Body_Base_T) return Float;
   function Get_Y(O : Body_Base_T) return Float;
 
   type Orbiting_Body_T is new Body_Base_T and Orbiting_Body_I with record
      Distance     : Float;
      Speed        : Float;
      Angle        : Float;
      Turns_Around : access Orbit_Ref_I'Class;
   end record;
   
   type Still_Body_T is new Body_Base_T and Still_Body_I with null record;
   
   type Object_Range_T is range 1 .. 100;
   
--     package Still_Container is new Ada.Containers.Vectors(Index_Type   => Object_Range_T,
--                                                           Element_Type => Still_Body_Access_I);
--     package Orbiting_Container is new Ada.Containers.Vectors(Index_Type   => Object_Range_T,
--                                                              Element_Type => Moving_Access_I);
  
   package Still_Container is new Ada.Containers.Bounded_Vectors(Index_Type   => Object_Range_T,
                                                         Element_Type => Still_Body_Access_I);
   package Orbiting_Container is new Ada.Containers.Bounded_Vectors(Index_Type   => Object_Range_T,
                                                            Element_Type => Moving_Access_I);

   
   type Solar_System_T is new Solar_System_I with record
      Still_Objects : Still_Container.Vector(100);
      Moving_Objects : Orbiting_Container.Vector(100);
   end record;

end Solar_System;
