------------------------------------------
Multiple Inheritance Lab
------------------------------------------
   
* Requirements
   
   - Create a tagged type to define shapes
 
      - Possible components could include location of shape
 
   - Create an interface to draw lines
 
      - Possible accessor functions could include line color and width
 
   - Create a new type inheriting from both of the above for a "printable object"
 
      - Implement a way to print the object using `Ada.Text_IO`
      - Does not have to be fancy!
 
   - Create a "printable object" type to draw something (rectangle, triangle, etc)
 
* Hints
 
   - This example is taken from Barnes' *Programming in Ada 2012* Section 21.2
   
---------------------------------------
Inheritance Lab Solution - Data Types
---------------------------------------
.. code:: Ada
   
   package Base_Types is
   
      type Coordinate_T is record
         X_Coord : Integer;
         Y_Coord : Integer;
      end record;
   
      type Line_T is array (1 .. 2) of Coordinate_T;
      -- convert Line_T so lowest X value is first
      function Ordered
        (Line : Line_T)
         return Line_T;
      type Lines_T is array (Natural range <>) of Line_T;
   
      type Color_Range_T is mod 255;
      type Color_T is record
         Red   : Color_Range_T;
         Green : Color_Range_T;
         Blue  : Color_Range_T;
      end record;
   
   private
      function Ordered
        (Line : Line_T)
         return Line_T is
        (if Line (1).X_Coord > Line (2).X_Coord then (Line (2), Line (1))
         else Line);
   
   end Base_Types;
   
---------------------------------------
Inheritance Lab Solution - Shapes
---------------------------------------
.. code:: Ada
   
   with Base_Types;
   package Geometry is
   
      type Object_T is abstract tagged private;
      function Origin
        (Object : Object_T'Class)
         return Base_Types.Coordinate_T;
   
   private
      type Object_T is abstract tagged record
         Origin : Base_Types.Coordinate_T;
      end record;
   
      function Origin
        (Object : Object_T'Class)
         return Base_Types.Coordinate_T is (Object.Origin);
   
   end Geometry;
   
---------------------------------------
Inheritance Lab Solution - Shapes
---------------------------------------
.. code:: Ada
   
   with Base_Types;
   package Line_Draw is
   
      type Object_T is interface;
      procedure Set_Color
        (Object : in out Object_T;
         Color  :        Base_Types.Color_T) is abstract;
      function Color
        (Object : Object_T)
         return Base_Types.Color_T is abstract;
      procedure Set_Pen
        (Object : in out Object_T;
         Size   :        Positive) is abstract;
      function Pen
        (Object : Object_T)
         return Positive is abstract;
      function Convert
        (Object : Object_T)
         return Base_Types.Lines_T is abstract;
      procedure Print (Object : Object_T'Class);
   
   end Line_Draw;
   
---------------------------------------------
Inheritance Lab Solution - Printable Object
---------------------------------------------
.. code:: Ada
   
   with Geometry;
   with Line_Draw;
   with Base_Types;
   package Printable_Object is
   
      type Object_T is
        abstract new Geometry.Object_T and Line_Draw.Object_T with private;
      procedure Set_Color
        (Object : in out Object_T;
         Color  :        Base_Types.Color_T);
      function Color
        (Object : Object_T)
         return Base_Types.Color_T;
      procedure Set_Pen
        (Object : in out Object_T;
         Size   :        Positive);
      function Pen
        (Object : Object_T)
         return Positive;
   
   private
      type Object_T is abstract new Geometry.Object_T and Line_Draw.Object_T with
      record
         Color    : Base_Types.Color_T := (0, 0, 0);
         Pen_Size : Positive           := 1;
      end record;
   
   end Printable_Object;
   
---------------------------------------------
Inheritance Lab Solution - Rectangle
---------------------------------------------
.. code:: Ada
   
   with Base_Types;
   with Printable_Object;
   
   package Rectangle is
   
      subtype Lines_T is Base_Types.Lines_T (1 .. 4);
   
      type Object_T is new Printable_Object.Object_T with private;
      procedure Set_Lines
        (Object : in out Object_T;
         Lines  :        Lines_T);
      function Lines
        (Object : Object_T)
         return Lines_T;
   
   private
      type Object_T is new Printable_Object.Object_T with record
         Lines : Lines_T;
      end record;
      function Convert
        (Object : Object_T)
         return Base_Types.Lines_T is (Object.Lines);
   
   end Rectangle;
   
