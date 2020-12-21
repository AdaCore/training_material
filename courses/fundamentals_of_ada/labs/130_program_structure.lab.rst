-----------------------
Program Structure Lab
-----------------------

* Requirements

   - Create a package to add text objects to a list

      + Create primitive operations to manipulate the list
      + Do not allow the same text object to appear more than once

   - Create a child package to print the list
   - Create a main program to enter unique data into the list

      + Use `Ada.Text_IO` to enter data

* Hints

   - Need equality operator(s) to compare data entry to list elements
   - Try writing code using "distinguished receiver" notation

      + i.e. `My_List.Add ( "foo" )` rather than `Add ( My_List, "foo" )`

----------------------------------------------
Program Structure Lab Solution - List (Spec)
----------------------------------------------

.. code:: Ada

   package List is

     type Element_T is private;
     type List_T is tagged private;
     type Index_T is range 0 .. 1_000;

     procedure Add (This : in out List_T;
                    Text : in     String);

     function Length (This : List_T) return Index_T;

     function At_Index (This  : List_T;
                        Index : Index_T)
                        return Element_T;

     function "=" (L : Element_T; R : String) return Boolean;
     function "=" (L : String; R : Element_T) return Boolean;

   private
     type Element_T is record
       Text   : String (1 .. 20) := (others => ' ');
       Length : Natural          := 0;
     end record;

     type List_Array_T is array (1 .. Index_T'Last) of Element_T;

     type List_T is tagged record
       Values : List_Array_T;
       Length : Index_T := 0;
     end record;

   end List;

----------------------------------------------
Program Structure Lab Solution - List (Body)
----------------------------------------------

.. code:: Ada

   package body List is

     procedure Add (This : in out List_T;
                    Text : in     String) is
     begin
       This.Length := This.Length + 1;
       This.Values (This.Length).Text
        (1 .. Text'Length)              := Text;
       This.Values (This.Length).Length := Text'Length;
      end Add;
   
     function Length (This : List_T) return Index_T is (This.Length);

     function At_Index (This  : List_T;
                        Index : Index_T)
                        return Element_T is
       (This.Values (Index));

     function "=" (L : Element_T; R : String) return Boolean is
       (L.Text(1 .. L.Length) = R);
     function "=" (L : String; R : Element_T) return Boolean is
       (R.Text(1 .. R.Length) = L);

   end List;

----------------------------------------------
Program Structure Lab Solution - Print List
----------------------------------------------

.. code:: Ada

   package List.Print is

     procedure Put (This : List_T);

   end List.Print;

   with Ada.Text_IO; use Ada.Text_IO;
   package body List.Print is

     procedure Put (This : List_T) is
     begin
       Put_Line ("---");
       if This.Length = 0 then
         Put_Line ("<empty>");
       else
         for Element of This.Values loop
           if Element.Length > 0 then
             Put_Line (Element.Text
                (1 .. Element.Length));
           end if;
         end loop;
       end if;
       Put_Line ("---");
     end Put;

   end List.Print;

---------------------------------------
Program Structure Lab Solution - Main
---------------------------------------

.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   with List;
   with List.Print;
   use type List.Element_T;
   use all type List.List_T;
   procedure Main is

     My_List : List.List_T;
   
     function Find (Str : String) return Boolean is
     begin
       for I in 1 .. My_List.Length loop
         if Str = My_List.At_Index (I) then
           return True;
         end if;
       end loop;
       return False;
     end Find;

   begin

     loop
       Put ("Enter string: ");
       declare
         Str : constant String := Get_Line;
       begin
         exit when Str'Length = 0;
         if Find (Str) then
           Put_Line ("Already added");
         else
           List.Add (My_List, Str);
         end if;
       end;
     end loop;

     List.Print.Put (My_List);

   end Main;
