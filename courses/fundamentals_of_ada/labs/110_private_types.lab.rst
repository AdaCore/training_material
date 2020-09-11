-------------------
Private Types Lab
-------------------

* Requirements

   - Implement a program to create a map such that

      + Map key is a description of a flag
      + Map element content is the set of colors in the flag

   - Operations on the map should include: Add, Remove, Modify, Get, Exists, Image
   - Main program should print out the entire map before exiting

* Hints

   - Should implement a **map** ADT (to keep track of the flags)

      + This **map** will contain all the flags and their color descriptions

   - Should implement a **set** ADT (to keep track of the colors)

      + This **set** will be the description of the map element

   - Each ADT should be its own package
   - At a minimum, the **map** and **set** type should be `private`

---------------------------------------------
Private Types Lab Solution (Color Set Spec)
---------------------------------------------

.. code:: Ada
    
   package Colors is

     type Color_T is (Red, Yellow, Green, Blue, Black);
     type Color_Set_T is private;

     Empty_Set : constant Color_Set_T;

     procedure Add (Set   : in out Color_Set_T;
                    Color :        Color_T);
     procedure Remove (Set   : in out Color_Set_T;
                       Color :        Color_T);
     function Image (Set : Color_Set_T) return String;

   private
     type Color_Set_Array_T is array (Color_T) of Boolean;
     type Color_Set_T is record
       Values : Color_Set_Array_T := (others => False);
     end record;

     Empty_Set : constant Color_Set_T := (Values => (others => False));

   end Colors;

---------------------------------------------
Private Types Lab Solution (Flag Map Spec)
---------------------------------------------

.. code:: Ada

   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   with Colors;
   package Flags is

     type Map_Element_T is private;
     type Map_T is private;

     procedure Add (Map         : in out Map_T;
                    Key         :        String;
                    Description :        Colors.Color_Set_T;
                    Success     :    out Boolean);
     procedure Remove (Map     : in out Map_T;
                       Key     :        String;
                       Success :    out Boolean);
     procedure Modify (Map         : in out Map_T;
                       Key         :        String;
                       Description :        Colors.Color_Set_T;
                       Success     :    out Boolean);
     function Exists (Map : Map_T;
                      Key : String)
                      return Boolean;
     function Get (Map : Map_T;
                   Key : String)
                   return Map_Element_T;
     function Image (Item : Map_Element_T) return String;
     function Image (Flag : Map_T) return String;
   private
     type Map_Element_T is record
       Key         : Unbounded_String   := Null_Unbounded_String;
       Description : Colors.Color_Set_T := Colors.Empty_Set;
     end record;
     type Map_Array_T is array (1 .. 100) of Map_Element_T;
     type Map_T is record
       Values : Map_Array_T;
       Length : Natural := 0;
     end record;

   end Flags;

---------------------------------------------
Private Types Lab Solution (Color Set Body)
---------------------------------------------
.. code:: Ada

   package body Colors is
     procedure Add (Set   : in out Color_Set_T;
                    Color :        Color_T) is
     begin
       Set.Values (Color) := True;
     end Add;
     procedure Remove (Set   : in out Color_Set_T;
                       Color :        Color_T) is
     begin
       Set.Values (Color) := False;
     end Remove;
     function Image (Set   : Color_Set_T;
                     First : Color_T;
                     Last  : Color_T)
                     return String is
       Str : constant String := (if Set.Values (First) then Color_T'Image (First) else "");
     begin
       if First = Last then
         return Str;
       else
         return Str & " " & Image (Set, Color_T'Succ (First), Last);
       end if;
     end Image;
     function Image (Set : Color_Set_T) return String is
     begin
       return Image (Set, Color_T'First, Color_T'Last);
     end Image;
   end Colors;

---------------------------------------------------
Private Types Lab Solution (Flag Map Body 1 of 2)
---------------------------------------------------
   .. code:: Ada

      package body Flags is
        procedure Add (Map         : in out Map_T;
                       Key         :        String;
                       Description :        Colors.Color_Set_T;
                       Success     :    out Boolean) is
        begin
          Success := (for all Item of Map.Values
             (1 .. Map.Length) => Item.Key /= Key);
          if Success then
            declare
              New_Item : Map_Element_T :=
               (Key => To_Unbounded_String (Key), Description => Description);
            begin
              Map.Length              := Map.Length + 1;
              Map.Values (Map.Length) := New_Item;
            end;
          end if;
        end Add;
        procedure Remove (Map     : in out Map_T;
                          Key     :        String;
                          Success :    out Boolean) is
        begin
          Success := False;
          for I in 1 .. Map.Length loop
            if Map.Values (I).Key = Key then
              Map.Values
               (I .. Map.Length - 1) := Map.Values
                (I + 1 .. Map.Length);
              Success := True;
              exit;
            end if;
          end loop;
        end Remove;

---------------------------------------------------
Private Types Lab Solution (Flag Map Body 2 of 2)
---------------------------------------------------
   .. code:: Ada

        procedure Modify (Map         : in out Map_T;
                          Key         :        String;
                          Description :        Colors.Color_Set_T;
                          Success     :    out Boolean) is
        begin
          Success := False;
          for I in 1 .. Map.Length loop
            if Map.Values (I).Key = Key then
              Map.Values (I).Description := Description;
              Success                    := True;
              exit;
            end if;
          end loop;
        end Modify;
        function Exists (Map : Map_T; Key : String) return Boolean is
          (for some Item of Map.Values (1 .. Map.Length) => Item.Key = Key);
        function Get (Map : Map_T; Key : String) return Map_Element_T is
          Ret_Val : Map_Element_T;
        begin
          for I in 1 .. Map.Length loop
            if Map.Values (I).Key = Key then
              Ret_Val := Map.Values (I);
              exit;
            end if;
          end loop;
          return Ret_Val;
        end Get;
        function Image (Item : Map_Element_T) return String is
         (To_String (Item.Key) & " => " & Colors.Image (Item.Description));
        function Image (Flag : Map_T) return String is
          Ret_Val : Unbounded_String := Null_Unbounded_String;
        begin
          for Item of Flag.Values
           (1 .. Flag.Length) loop
            Ret_Val := Ret_Val & Image (Item) & ASCII.LF;
          end loop;
          return To_String (Ret_Val);
        end Image;
      end Flags;
