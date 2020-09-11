-----------------
Subprograms Lab
-----------------

* Requirements

   - Allow the user to fill a list with values and then check to see if a value is in the list
   - Create at least two subprograms:

      + Sort a list of items
      + Search a list of items and return TRUE if found
      + You can create additional subprograms if desired
   
* Hints

   - Subprograms can be nested inside other subprograms

      + Like inside `main`

   - Try a binary search algorithm if you want to use recursion

      - Unconstrained arrays may be needed

.. container:: speakernote

   Nothing wrong with using brute force (e.g. for searching)

-----------------------------------
Subprograms Lab Solution - Search
-----------------------------------

.. code:: Ada

   function Is_Found (List : List_T;
                     Item : Integer)
                     return Boolean is
   begin
      if List'Length = 0 then
         return False;
      elsif List'Length = 1 then
         return List (List'First) = Item;
      else
         declare
            Midpoint : constant Integer := (List'First + List'Last) / 2;
         begin
            if List (Midpoint) = Item then
               return True;
            elsif List (Midpoint) > Item then
               return Is_Found (List(List'First .. Midpoint - 1),
                                Item);
            else -- List(Midpoint) < item
               return Is_Found (List(Midpoint + 1 .. List'Last),
                                Item);
            end if;
         end;
      end if;
   end Is_Found;

-----------------------------------
Subprograms Lab Solution - Sort
-----------------------------------

.. code:: Ada

   procedure Swap (I, J : in out Integer) is
      Temp : Integer := I;
   begin
      I := J;
      J := Temp;
   end Swap;

   procedure Sort (List : in out List_T) is
      Swapped : Boolean;
   begin
      for I in List'First .. List'Last loop
         Swapped := False;
         for J in 1 .. List'Last - I loop
            if List (J) > List (J + 1) then
               Swap (List (J), List (J + 1));
               Swapped := True;
            end if;
         end loop;
         if not Swapped then
            return;
         end if;
      end loop;
   end Sort;

-----------------------------------
Subprograms Lab Solution - Main
-----------------------------------

.. code:: Ada

      procedure Fill (List : out List_T) is
      begin
         Put_Line ("Enter values for list: ");
         for I in List'First .. List'Last loop
            List (I) := Integer'Value (Get_Line);
         end loop;
      end Fill;

      Number : Integer;

   begin

      Put ("Enter number of elements in list: ");
      Number := Integer'Value (Get_Line);

      declare
         List : List_T (1 .. Number);
      begin
         Fill (List);

         Sort (List);

         loop
            Put ("Enter number to look for: ");
            Number := Integer'Value (Get_Line);
            exit when Number < 0;
            Put_Line (Boolean'Image (Is_Found (List, Number)));
         end loop;
      end;
