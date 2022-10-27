C : constant Pkg_Vectors.Cursor :=
  Student_Per_Day.Find (10);
C2 : constant Pkg_Sets.Cursor :=
  Received_Parcels.Find ("UPS ZZ-44-I12");
C3 : constant Pkg_Maps.Cursor :=
  Math_Constants.Find
    (To_Unbounded_String
       ("Pi")); -- Finds by the key!
