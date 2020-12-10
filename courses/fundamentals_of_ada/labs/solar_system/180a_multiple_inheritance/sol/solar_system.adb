with Libm_Single;                       use Libm_Single;

package body Solar_System is

   function Get_X(O : Body_Base_T) return Float is
   begin
      return O.X;
   end Get_X;

   function Get_Y(O : Body_Base_T) return Float is
   begin
      return O.Y;
   end Get_Y;

   function Create_Orbiting(Distance : Float; Speed: Float; Angle: Float; Turns_Around : access Orbit_Ref_I'Class) return access Orbiting_Body_T is
   begin
      return new Orbiting_Body_T'(X            => 0.0,
                                  Y            => 0.0,
                                  Distance     => Distance,
                                  Speed        => Speed,
                                  Angle        => Angle,
                                  Turns_Around => Turns_Around);
   end Create_Orbiting;

   procedure Move (B : in out Orbiting_Body_T) is
   begin

      B.X := B.Turns_Around.Get_X +
        B.Distance * Cos (B.Angle);

      B.Y := B.Turns_Around.Get_Y +
        B.Distance * Sin (B.Angle);

      B.Angle := B.Angle + B.Speed;

   end Move;

   function Create_Still(X : Float; Y: Float) return access Still_Body_T is
   begin
      return new Still_Body_T'(X => X,
                               Y => Y);
   end Create_Still;

   function Create_Solar_System return access Solar_System_T is
   begin
      return new Solar_System_T;
   end Create_Solar_System;

   procedure Add_Still_Body(S : in out Solar_System_T; B : access Still_Body_I'Class) is
   begin
      S.Still_Objects.Append(Still_Body_Access_I(B));
   end Add_Still_Body;

   procedure Add_Moving_Body(S : in out Solar_System_T; B : access Movable_I'Class) is
   begin
      S.Moving_Objects.Append(Moving_Access_I(B));
   end Add_Moving_Body;

   procedure Move (S : in out Solar_System_T) is
   begin
      for B of S.Moving_Objects loop
         Movable_I'Class(B.all).Move;
      end loop;
   end Move;

end Solar_System;
