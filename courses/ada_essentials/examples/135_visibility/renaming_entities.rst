with Math_Utilities.Trigonometry;
with Viewpoint_Types;
with Sensors;
with Base_Types;
use type Base_Types.Float_T;
procedure Main is

   Observation : Viewpoint_Types.Observation_Data_T;

   procedure Full is
   begin

      Observation.Sides (Viewpoint_Types.Observer_Point1) := Sensors.Read;
      Observation.Sides (Viewpoint_Types.Observer_Point2) := Sensors.Read;
      Observation.Vertices (Viewpoint_Types.Observer)     := Sensors.Read;

      -- use cosine rule to determine distance between two points, given angle
      -- and distances between observer and 2 points A**2 = B**2 + C**2 -
      -- 2*B*C*cos(A)
      Observation.Sides (Viewpoint_Types.Point1_Point2) :=
        Math_Utilities.Trigonometry.Square_Root
          (Observation.Sides (Viewpoint_Types.Observer_Point1)**2 +
           Observation.Sides (Viewpoint_Types.Observer_Point2)**2 +
           2.0 * Observation.Sides (Viewpoint_Types.Observer_Point1) *
             Observation.Sides (Viewpoint_Types.Observer_Point2) *
             Math_Utilities.Trigonometry.Cosine
               (Observation.Vertices (Viewpoint_Types.Observer)));
   end Full;

   procedure Uses is
      use Viewpoint_Types;
      use Math_Utilities.Trigonometry;
   begin
      Observation.Sides (Observer_Point1) := Sensors.Read;
      Observation.Sides (Observer_Point2) := Sensors.Read;
      Observation.Vertices (Observer)     := Sensors.Read;

      -- use cosine rule to determine distance between two points, given angle
      -- and distances between observer and 2 points A**2 = B**2 + C**2 -
      -- 2*B*C*cos(A)
      Observation.Sides (Point1_Point2) :=
        Square_Root
          (Observation.Sides (Observer_Point1)**2 +
           Observation.Sides (Observer_Point2)**2 +
           2.0 * Observation.Sides (Observer_Point1) *
             Observation.Sides (Observer_Point2) *
             Cosine (Observation.Vertices (Observer)));
   end Uses;

   procedure Renamed is
      Side1 : Base_Types.Float_T renames
        Observation.Sides (Viewpoint_Types.Observer_Point1);
      Side2 : Base_Types.Float_T renames
        Observation.Sides (Viewpoint_Types.Observer_Point2);
      Angles : Viewpoint_Types.Vertices_Array_T renames Observation.Vertices;
      Required_Angle : Viewpoint_Types.Vertices_T renames
        Viewpoint_Types.Observer;
      Desired_Side : Base_Types.Float_T renames
        Observation.Sides (Viewpoint_Types.Point1_Point2);

      package Math renames Math_Utilities.Trigonometry;

      function Sqrt
        (X : Base_Types.Float_T)
         return Base_Types.Float_T renames Math.Square_Root;
   begin

      Side1                   := Sensors.Read;
      Side2                   := Sensors.Read;
      Angles (Required_Angle) := Sensors.Read;

      -- use cosine rule to determine distance between two points, given angle
      -- and distances between observer and 2 points A**2 = B**2 + C**2 -
      -- 2*B*C*cos(A)
      Desired_Side :=
        Sqrt
          (Side1**2 + Side2**2 +
           2.0 * Side1 * Side2 * Math.Cosine (Angles (Required_Angle)));

   end Renamed;
begin
   null;

end Main;

with Base_Types;
package Viewpoint_Types is

   type Vertices_T is (Observer, Point1, Point2);
   type Sides_T is (Observer_Point1, Observer_Point2, Point1_Point2);
   type Vertices_Array_T is array (Vertices_T) of Base_Types.Float_T;
   type Sides_Array_T is array (Sides_T) of Base_Types.Float_T;

   type Observation_Data_T is record
      Vertices : Vertices_Array_T;
      Sides    : Sides_Array_T;
   end record;

end Viewpoint_Types;

with Base_Types;
package Math_Utilities.Trigonometry is

   function Sine
     (X : Base_Types.Float_T)
      return Base_Types.Float_T;
   function Cosine
     (X : Base_Types.Float_T)
      return Base_Types.Float_T;
   function Square_Root
     (X : Base_Types.Float_T)
      return Base_Types.Float_T;

end Math_Utilities.Trigonometry;

package Base_Types is

   type Float_T is digits 12;

end Base_Types;

package Math_Utilities is
end Math_Utilities;

with Base_Types;
package Sensors is

   function Read return Base_Types.Float_T;

end Sensors;

with Ada.Numerics.Generic_Elementary_Functions;
package body Math_Utilities.Trigonometry is

   package Math is new Ada.Numerics.Generic_Elementary_Functions
     (Base_Types.Float_T);

   function Sine
     (X : Base_Types.Float_T)
      return Base_Types.Float_T renames Math.Sin;
   function Cosine
     (X : Base_Types.Float_T)
      return Base_Types.Float_T renames Math.Cos;
   function Square_Root
     (X : Base_Types.Float_T)
      return Base_Types.Float_T renames Math.Sqrt;

end Math_Utilities.Trigonometry;

package body Sensors is
   function Read return Base_Types.Float_T is (0.0);
end Sensors;
