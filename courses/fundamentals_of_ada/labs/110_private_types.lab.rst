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

   with Colors;
   package Flags is

      type Key_T is (Boston, London, Paris, Rome);
      type Map_Element_T is private;
      type Map_T is private;

      procedure Add (Map         : in out Map_T;
                     Key         :        Key_T;
                     Description :        Colors.Color_Set_T;
                     Success     :    out Boolean);
      procedure Remove (Map     : in out Map_T;
                        Key     :        Key_T;
                        Success :    out Boolean);
      procedure Modify (Map         : in out Map_T;
                        Key         :        Key_T;
                        Description :        Colors.Color_Set_T;
                        Success     :    out Boolean);
      function Exists (Map : Map_T; Key : Key_T) return Boolean;
      function Get (Map : Map_T; Key : Key_T) return Map_Element_T;
      function Image (Item : Map_Element_T) return String;
      function Image (Flag : Map_T) return String;
   private
      type Map_Element_T is record
         Key         : Key_T;
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

      procedure Add (Map         : in out Map_T;
                     Key         :        Key_T;
                     Description :        Colors.Color_Set_T;
                     Success     :    out Boolean) is
      begin
         Success := (for all Item of Map.Values
              (1 .. Map.Length) => Item.Key /= Key);
         if Success then
            declare
               New_Item : Map_Element_T :=
                 (Key => Key, Description => Description);
            begin
               Map.Length              := Map.Length + 1;
               Map.Values (Map.Length) := New_Item;
            end;
         end if;
      end Add;
      procedure Remove (Map     : in out Map_T;
                        Key     :        Key_T;
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
                        Key         :        Key_T;
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
      function Exists (Map : Map_T; Key : Key_T) return Boolean is
         (for some Item of Map.Values (1 .. Map.Length) => Item.Key = Key);
      function Get (Map : Map_T; Key : Key_T) return Map_Element_T is
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
        (Key_T'Image (Item.Key) & " => " & Colors.Image (Item.Description));
      function Image (Flag : Map_T) return String is
         Ret_Val : String (1 .. 1_000);
         Next    : Integer := Ret_Val'First;
      begin
         for Item of Flag.Values (1 .. Flag.Length) loop
            declare
               Str : constant String := Image (Item);
            begin
               Ret_Val (Next .. Next + Str'Length) := Image (Item) & ASCII.LF;
               Next                          := Next + Str'Length + 1;
            end;
         end loop;
         return Ret_Val (1 .. Next - 1);
      end Image;

-----------------------------------
Private Types Lab Solution (Main)
-----------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Colors;
   with Flags;
   with Input;
   procedure Main is
      Map : Flags.Map_T;
   begin
      loop
         Put ("Enter country name (");
         for Key in Flags.Key_T loop
            Put (Flags.Key_T'Image (Key) & " ");
         end loop;
         Put ("): ");
         declare
            Str         : constant String := Get_Line;
            Key         : Flags.Key_T;
            Description : Colors.Color_Set_T;
            Success     : Boolean;
         begin
            exit when Str'Length = 0;
            Key         := Flags.Key_T'Value (Str);
            Description := Input.Get;
            if Flags.Exists (Map, Key) then
               Flags.Modify (Map, Key, Description, Success);
            else
               Flags.Add (Map, Key, Description, Success);
            end if;
         end;
      end loop;
      Put_Line (Flags.Image (Map));
   end Main;
