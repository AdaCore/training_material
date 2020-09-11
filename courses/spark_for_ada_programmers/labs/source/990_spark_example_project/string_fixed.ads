------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . S T R I N G S . F I X E D                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Maps;
with Ada.Strings; use Ada.Strings;

package String_Fixed with SPARK_Mode is
   pragma Preelaborate;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : String;
      Set     : Maps.Character_Set;
      From    : Positive;
      Test    : Membership := Inside;
      Going   : Direction := Forward) return Natural;
   --  Index searches for the first or last occurrence of any of a set of
   --  characters (when Test=Inside), or any of the complement of a set of
   --  characters (when Test=Outside). If Source is the null string, Index
   --  returns 0; otherwise, if From is not in Source'Range, then Index_Error
   --  is propagated. Otherwise, it returns the smallest index I >= From (if
   --  Going=Forward) or the largest index I <= From (if Going=Backward) such
   --  that Source(I) satisfies the Test condition with respect to Set; it
   --  returns 0 if there is no such Character in Source.

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward) return Natural;
   --  If Going = Forward,
   --  returns Index (Source, Set, Source'First, Test, Forward);
   --  otherwise, returns
   --  Index (Source, Set, Source'Last, Test, Backward);

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward) return Natural;
   --  Returns Index (Source, Maps.To_Set(Space), From, Outside, Going);

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward) return Natural;
   --  Returns Index(Source, Maps.To_Set(Space), Outside, Going)

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String) return String;
   --  Propagates Index_Error if Before is not in Source'First .. Source'Last+1;
   --  otherwise, returns Source(Source'First..Before-1) & New_Item &
   --  Source(Before..Source'Last), but with lower bound 1.
   --  Beware of the overflow of the string length !

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String) return String;
   --  Propagates Index_Error if Position is not in Source'First ..
   --  Source'Last+1; otherwise, returns the string obtained from Source by
   --  consecutively replacing characters starting at Position with
   --  corresponding characters from New_Item with lower bound 1. If the end of
   --  Source is reached before the characters in New_Item are exhausted, the
   --  remaining characters from New_Item are appended to the string.
   --  Beware of the overflow of the string length !

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural) return String;
   --  If From > Through, the returned string is Source with lower bound 1.
   --  If From not in Source'Range, or Through > Source'Last, then Index_Error
   --  is propagated. Otherwise, the returned string comprises
   --  Source(Source'First..From - 1) & Source(Through+1..Source'Last), but
   --  with lower bound 1.

   ---------------------------------
   -- String Selector Subprograms --
   ---------------------------------

   function Trim
     (Source : String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set) return String;
   --  Returns the string obtained by removing from Source all leading
   --  characters in Left and all trailing characters in Right.

   function Trim
     (Source : String;
      Side   : Trim_End) return String;
   --  Returns the string obtained by removing from Source all leading Space
   --  characters (if Side = Left), all trailing Space characters
   --  (if Side = Right), or all leading and trailing Space characters
   --  (if Side = Both).

   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String;
   --  Returns a string of length Count. If Count <= Source'Length, the string
   --  comprises the first Count characters of Source. Otherwise, its contents
   --  are Source concatenated with Count-Source'Length Pad characters.

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space) return String;
   --  Returns a string of length Count. If Count <= Source'Length, the string
   --  comprises the last Count characters of Source. Otherwise, its contents
   --  are Count-Source'Length Pad characters concatenated with Source.

   ----------------------------------
   -- String Constructor Functions --
   ----------------------------------

   function "*"
     (Left  : Natural;
      Right : Character) return String;
   --  This function replicates a character a specified number of times. It
   --  returns a string whose length is Left and each of whose elements is
   --  Right.

end String_Fixed;
