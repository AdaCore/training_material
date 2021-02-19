-----------------
Overloading Lab
-----------------

* Requirements

   - Create multiple functions named "Convert" to convert between digits and text representation

      + One routine should take a digit and return the text version (e.g. **3** would return **three**)

      + One routine should take text and return the digit (e.g. **two** would return **2**)

   - Query the user to enter text or a digit and print it's equivalent
   - If the user enters consecutive entries that are equivalent, print a message

      + e.g. **4** followed by **four** should get the message

* Hints

   - You can use enumerals for the text representation

      + Then use *'image* / *'value* where needed

   - Use an equivalence function two compare different types

-------------------------------------------------
Overloading Lab Solution - Conversion Functions
-------------------------------------------------

.. container:: source_include labs/answers/090_overloading.txt :start-after:--Conversion_Functions :end-before:--Conversion_Functions :code:Ada

-------------------------------------------------
Overloading Lab Solution - Main
-------------------------------------------------

.. container:: source_include labs/answers/090_overloading.txt :start-after:--Main :end-before:--Main :code:Ada

   begin
      loop
         Put ("Input: ");
         declare
            Str : constant String := Get_Line;
         begin
            exit when Str'Length = 0;
            if Str (Str'First) in '0' .. '9' then
               declare
                  Converted : constant Digit_Name_T := Convert (Str (Str'First));
               begin
                  Put (Digit_Name_T'Image (Converted));
                  if Converted = Last_Entry then
                     Put_Line (" - same as previous");
                  else
                     Last_Entry := Convert (Converted);
                     New_Line;
                  end if;
               end;
            else
               declare
                  Converted : constant Digit_T := Convert (Str);
               begin
                  Put (Digit_T'Image (Converted));
                  if Converted = Last_Entry then
                     Put_Line (" - same as previous");
                  else
                     Last_Entry := Converted;
                     New_Line;
                  end if;
               end;
            end if;
         end;
      end loop;
