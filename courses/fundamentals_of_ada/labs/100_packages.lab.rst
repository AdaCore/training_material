--------------
Packages Lab
--------------

* Requirements

   - Create a program to add and remove integer values from a list

   - Program should allow user to do the following as many times as desired

      - Add an integer to the list
      - Remove all occurrences of an integer from the list
      - Print the values in the list

* Hints

   - Create (at least) three packages

      1. minimum/maximum integer values and maximum number of items in list
      2. User input (ensure value is in range)
      3. List ADT

---------------------------------
Creating Packages in GNATstudio
---------------------------------

* Right-click on :filename:`src` node

* :menu:`New` :math:`\rightarrow` :menu:`Ada Package`

   - Fill in name of Ada package
   - Check the box if you want to create the package body in addition to the package spec

-----------------------------------
Packages Lab Solution (Constants)
-----------------------------------

.. code:: Ada
    
   package Constants is

      Lowest_Value  : constant := 100;
      Highest_Value : constant := 999;
      Maximum_Count : constant := 10;

   end Constants;

.. container:: speakernote

   Could use functions where the value is stored in the body - less recompilation if the value changes (but then they cannot be universal integers)
     
------------------------------
Packages Lab Solution (Input)
------------------------------
    
.. code:: Ada
    
   package Input is
      function Get_Value (Prompt : String) return Integer;
   end Input;

   with Ada.Text_IO; use Ada.Text_IO;
   with Constants;
   package body Input is

      function Get_Value (Prompt : String) return Integer is
         Ret_Val : Integer;
      begin
         Put (Prompt & "> ");
         loop
            Ret_Val := Integer'Value (Get_Line);
            exit when Ret_Val >= Constants.Lowest_Value
              and then Ret_Val <= Constants.Highest_Value;
            Put ("Invalid. Try Again >");
         end loop;
         return Ret_Val;

      end Get_Value;

   end Input;

-----------------------------------
Packages Lab Solution (List Spec)
-----------------------------------
.. code:: Ada

   package List is
     procedure Add (Value : Integer);
     procedure Remove (Value : Integer);
     function Length return Natural;
     procedure Print;
   end List;

-----------------------------------
Packages Lab Solution (List Body)
-----------------------------------
.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Constants;
   package body List is
       Content : array (1 .. Constants.Maximum_Count) of Integer;
       Last    : Natural := 0;

       procedure Add (Value : Integer) is
       begin
         if Last < Content'Last then
           Last           := Last + 1;
           Content (Last) := Value;
         else
           Put_Line ("Full");
         end if;
       end Add;

       procedure Remove (Value : Integer) is
       begin
         for I in 1 .. Last loop
           if Content (I) = Value then
             Content(I .. Last - 1) := Content(I + 1 .. Last);
             Last := Last - 1;
           end if;
         end loop;
       end Remove;

       procedure Print is
       begin
         for I in 1 .. Last loop
           Put_Line (Integer'Image(Content (I)));
         end loop;
       end Print;

       function Length return Natural is ( Last );

     end List;

------------------------------
Packages Lab Solution (Main)
------------------------------
    
.. code:: Ada

   with Ada.Text_IO; use Ada.Text_IO;
   with Input;
   with List;
   procedure Main is
   begin
      loop
         Put ("(A)dd | (R)emove | (P)rint | Q(uit) : ");
         declare
            Str : constant String := Get_Line;
         begin
            exit when Str'Length = 0;
            case Str (Str'First) is
               when 'A' =>
                  List.Add (Input.Get_Value ("Value to add"));
               when 'R' =>
                  List.Remove (Input.Get_Value ("Value to remove"));
               when 'P' =>
                  List.Print;
               when 'Q' =>
                  exit;
               when others =>
                  Put_Line ("Illegal entry");
            end case;
         end;
      end loop;
   end Main;
