package Datastore is
   pragma Elaborate_Body;

   -- Create an array of strings that can be of any length
   -- (i.e. array of string pointers)

   -- Create a record that contains this array
   type Element_T is null record;

   -- Create two access types to the element
   --    An immutable access type
   --    An access type that allows us to modify the pointed-to object

   subtype Index_T is Integer range 1 .. 100;

   -- Create functions that return the different access types
   -- based on the index into the array

end Datastore;
