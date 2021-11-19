with Ada.Strings.Unbounded;

package Data_Files is
   function Read (File_Name : String) return Ada.Strings.Unbounded.Unbounded_String;
end Data_Files;
