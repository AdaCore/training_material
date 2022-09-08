with Ada.Containers.Ordered_Sets;
use type Ada.Containers.Count_Type;

package Drawable_Chars is
   
   type Drawable_Char_T (<>) is private;
   -- Element that can be rendered in a console
   type Drawable_Char_Access_T is access all Drawable_Char_T;
   function Image (C : Drawable_Char_T) return String;
   -- Element's representation, this is used to render the
   -- element in e.g. a console.
   
   type Drawable_Charset_T
     is array (Positive range <>) of Drawable_Char_Access_T;
   -- Set of unique Drawable_Char_T that form together
   -- a coherent set of primitive drawing elements.
   
   type Drawable_Char_Characteristic_T is new Integer;
   -- Characteristic attached to every elements of a Drawable_Charset_T
   -- e.g. how much of that drawing primitive is black
   -- This is then used to map those elements to pixels based on
   -- e.g. luminosity information

   type Sorted_Charset_T is private;
   -- Set of sorted drawable elements, this is a palette,
   -- from which images can be rendered to console.

   function Empty (SC : Sorted_Charset_T) return Boolean;
   -- True iif the set contains no element
   function Debug_Image (C : Sorted_Charset_T) return String;
   -- Displays the full set of characters, in order
   
   function Closest (Metric : Sorted_Charset_T;
                     Value : Drawable_Char_Characteristic_T)
                     return Drawable_Char_T
   with Pre => not Empty (Metric);
   -- Finds in the sorted set the closest element to match a given value
   -- e.g. when matching a pixel luminosity = 200 to a palette (' ' => 0, '#' => 250),
   -- it will return '#'

private

   type Drawable_Char_T is new String;
   
   function "+" (S : Drawable_Char_T)
     return Drawable_Char_Access_T is
      (new Drawable_Char_T'(S));
   -- Convert the given char to an access for storing in a charset
   
   type Char_With_Characteristic_T is record
      Char : Drawable_Char_Access_T;
      Value : Drawable_Char_Characteristic_T;
   end record;

   function "<" (A, B : Char_With_Characteristic_T) return Boolean is
     (A.Value <= B.Value);

   package Chars_Sorted_By_Characteristic_Pkg is
     new Ada.Containers.Ordered_Sets (Element_Type => Char_With_Characteristic_T);

   type Drawable_Char_Caracteristics_List_T
     is array (Positive range <>) of Drawable_Char_Characteristic_T;

   function Sort_By (Charset : Drawable_Charset_T;
                     Characteristic : Drawable_Char_Caracteristics_List_T)
                     return Sorted_Charset_T
     with Pre => Charset'Length = Characteristic'Length;
   -- Sort the given charset by a list of characteristics, mapping each characteristic in order
   -- e.g. for sorting by height (with 0 lower than 1), it could map ("_", "-", "*") to (1, 2, 3).

   function Reversed (SC : Sorted_Charset_T) return Sorted_Charset_T;
   -- Reversed representation of a sorted set
   
   
   type Sorted_Charset_T
      is new Chars_Sorted_By_Characteristic_Pkg.Set
      with null record;

   subtype Sorted_Charset_Cursor_T is Chars_Sorted_By_Characteristic_Pkg.Cursor;
   
   function Empty (SC : Sorted_Charset_T) return Boolean
      is (SC.Length = 0);

end Drawable_Chars;
