===================
Renaming Entities
===================

---------------------------------
Three Positives Make a Negative
---------------------------------

* Good Coding Practices ...

   - Descriptive names
   - Modularization
   - Subsystem hierarchies

* Can result in cumbersome references

   .. code:: Ada

      -- use cosine rule to determine distance between two points,
      -- given angle and distances between observer and 2 points
      -- A**2 = B**2 + C**2 - 2*B*C*cos(angle)
      Observation.Sides (Viewpoint_Types.Point1_Point2) :=
        Math_Utilities.Square_Root
          (Observation.Sides (Viewpoint_Types.Observer_Point1)**2 +
           Observation.Sides (Viewpoint_Types.Observer_Point2)**2 -
           2.0 * Observation.Sides (Viewpoint_Types.Observer_Point1) *
             Observation.Sides (Viewpoint_Types.Observer_Point2) *
             Math_Utilities.Trigonometry.Cosine
               (Observation.Vertices (Viewpoint_Types.Observer)));

--------------------------------
Writing Readable Code - Part 1
--------------------------------

* We could use :ada:`use` on package names to remove some dot-notation

   .. code:: Ada

      -- use cosine rule to determine distance between two points, given angle
      -- and distances between observer and 2 points A**2 = B**2 + C**2 -
      -- 2*B*C*cos(angle)
      Observation.Sides (Point1_Point2) :=
        Square_Root
          (Observation.Sides (Observer_Point1)**2 +
           Observation.Sides (Observer_Point2)**2 -
           2.0 * Observation.Sides (Observer_Point1) *
             Observation.Sides (Observer_Point2) *
             Cosine (Observation.Vertices (Observer)));

* But that only shortens the problem, not simplifies it

   - If there are multiple "use" clauses in scope:

      + Reviewer may have hard time finding the correct definition
      + Homographs may cause ambiguous reference errors

* We want the ability to refer to certain entities by another name (like an alias) with full read/write access (unlike temporary variables)

-----------------------
The "renames" Keyword
-----------------------

* :ada:`renames` declaration creates an alias to an entity

   - Packages

      .. code:: Ada

         package Trig renames Math.Trigonometry

   - Objects (or elements of objects)

      .. code:: Ada

         Angles : Viewpoint_Types.Vertices_Array_T
                  renames Observation.Vertices;
         Required_Angle : Viewpoint_Types.Vertices_T
                  renames Viewpoint_Types.Observer;

   - Subprograms

      .. code:: Ada

         function Sqrt (X : Base_Types.Float_T)
                        return Base_Types.Float_T
                        renames Math.Square_Root;

--------------------------------
Writing Readable Code - Part 2
--------------------------------

* With :ada:`renames` our complicated code example is easier to understand

   - Executable code is very close to the specification
   - Declarations as "glue" to the implementation details

   .. code:: Ada

      begin
         package Math renames Math_Utilities;
         package Trig renames Math.Trigonometry;

         function Sqrt (X : Base_Types.Float_T) return Base_Types.Float_T
           renames Math.Square_Root;
         function Cos ...

         B : Base_Types.Float_T
           renames Observation.Sides (Viewpoint_Types.Observer_Point1);
         -- Rename the others as Side2, Angles, Required_Angle, Desired_Side
      begin
         ...
         -- A**2 = B**2 + C**2 - 2*B*C*cos(angle)
         A := Sqrt (B**2 + C**2 - 2.0 * B * C * Cos (Angle));
      end;

