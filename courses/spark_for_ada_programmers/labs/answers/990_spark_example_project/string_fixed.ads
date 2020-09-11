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

with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings;      use Ada.Strings;

package String_Fixed with
   SPARK_Mode
is
   pragma Preelaborate;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      From   : Positive;
      Test   : Membership := Inside;
      Going  : Direction  := Forward)
      return Natural with
      Pre            => Source'Length = 0 or else From in Source'Range,
      Post           => Index'Result in 0 | Source'Range,
      Contract_Cases =>
      ((for all I in Source'Range =>
          (if I = From or else (I > From) = (Going = Forward) then
             (Test = Inside) /= Is_In (Source (I), Set))) =>
         Index'Result = 0,
       others =>
         Index'Result in Source'Range
         and then
         (Index'Result = From
          or else (Index'Result > From) = (Going = Forward))
         and then (Test = Inside) = Is_In (Source (Index'Result), Set)
         and then
         (for all I in Source'Range =>
            (if
               I /= Index'Result
               and then (I < Index'Result) = (Going = Forward)
               and then (I = From or else (I > From) = (Going = Forward))
             then (Test = Inside) /= Is_In (Source (I), Set))));
      --  Index searches for the first or last occurrence of any of a set of
      --  characters (when Test=Inside), or any of the complement of a set
      --  of characters (when Test=Outside). If Source is the null string,
      --  Index returns 0; otherwise, if From is not in Source'Range, then
      --  Index_Error is propagated. Otherwise, it returns the smallest index
      --  I >= From (if Going=Forward) or the largest index I <= From (if
      --  Going=Backward) such that Source(I) satisfies the Test condition with
      --  respect to Set; it returns 0 if there is no such Character in Source.

   function Any_In_Set
     (Source : String;
      Set    : Maps.Character_Set)
      return Boolean is (for some C of Source => Is_In (C, Set)) with
      Ghost;

   function Is_Valid_Inside_Forward
     (Source : String;
      Set    : Maps.Character_Set;
      Result : Natural)
      return Boolean is
     (if Result = 0 then not Any_In_Set (Source, Set)
      elsif Result not in Source'Range then Result = 0
      elsif Result = Source'First then Is_In (Source (Result), Set)
      else Is_In (Source (Result), Set) and then not Any_In_Set (Source
             (Source'First .. Result - 1), Set)) with
      Ghost;

   function Is_Valid_Inside_Backward
     (Source : String;
      Set    : Maps.Character_Set;
      Result : Natural)
      return Boolean is
     (if Result = 0 then not Any_In_Set (Source, Set)
      elsif Result not in Source'Range then Result = 0
      elsif Result = Source'Last then Is_In (Source (Result), Set)
      else Is_In (Source (Result), Set) and then not Any_In_Set (Source
             (Result + 1 .. Source'Last), Set)) with
      Ghost;

   function All_In_Set
     (Source : String;
      Set    : Maps.Character_Set)
      return Boolean is (for all C of Source => Is_In (C, Set)) with
      Ghost;

   function Is_Valid_Outside_Forward
     (Source : String;
      Set    : Maps.Character_Set;
      Result : Natural)
      return Boolean is
     (if Result = 0 then All_In_Set (Source, Set)
      elsif Result not in Source'Range then Result = 0
      elsif Result = Source'First then not Is_In (Source (Result), Set)
      else not Is_In (Source (Result), Set) and then All_In_Set (Source
             (Source'First .. Result - 1), Set)) with
      Ghost;

   function Is_Valid_Outside_Backward
     (Source : String;
      Set    : Maps.Character_Set;
      Result : Natural)
      return Boolean is
     (if Result = 0 then All_In_Set (Source, Set)
      elsif Result not in Source'Range then Result = 0
      elsif Result = Source'Last then not Is_In (Source (Result), Set)
      else not Is_In (Source (Result), Set) and then All_In_Set (Source
             (Result + 1 .. Source'Last), Set)) with
      Ghost;

   function Index
     (Source : String;
      Set    : Maps.Character_Set;
      Test   : Membership := Inside;
      Going  : Direction  := Forward)
      return Natural with
      Post =>
      (if Source'Length = 0 then Index'Result = 0
       elsif Test = Inside and Going = Forward then
         Is_Valid_Inside_Forward (Source, Set, Index'Result)
       elsif Test = Inside and Going = Backward then
         Is_Valid_Inside_Backward (Source, Set, Index'Result)
       elsif Test = Outside and Going = Forward then
         Is_Valid_Outside_Forward (Source, Set, Index'Result)
       elsif Test = Outside and Going = Backward then
         Is_Valid_Outside_Backward (Source, Set, Index'Result)
       else False);
      --  If Going = Forward,
      --  returns Index (Source, Set, Source'First, Test, Forward); otherwise,
      --  returns Index (Source, Set, Source'Last, Test, Backward);

   function Index_Non_Blank
     (Source : String;
      From   : Positive;
      Going  : Direction := Forward)
      return Natural with
      Pre            => Source'Length = 0 or else From in Source'Range,
      Post           => Index_Non_Blank'Result in 0 | Source'Range,
      Contract_Cases =>
      ((for all I in Source'Range =>
          (if I = From or else (I > From) = (Going = Forward) then
             Source (I) = ' ')) =>
         Index_Non_Blank'Result = 0,
       others =>
         Index_Non_Blank'Result in Source'Range
         and then
         (Index_Non_Blank'Result = From
          or else (Index_Non_Blank'Result > From) = (Going = Forward))
         and then Source (Index_Non_Blank'Result) /= ' '
         and then
         (for all I in Source'Range =>
            (if
               I /= Index_Non_Blank'Result
               and then (I < Index_Non_Blank'Result) = (Going = Forward)
               and then (I = From or else (I > From) = (Going = Forward))
             then Source (I) = ' ')));
      --  Returns Index (Source, Maps.To_Set(Space), From, Outside, Going);

   function Index_Non_Blank
     (Source : String;
      Going  : Direction := Forward)
      return Natural with
      Post           => Index_Non_Blank'Result in 0 | Source'Range,
      Contract_Cases =>
      ((for all C of Source => C = ' ') => Index_Non_Blank'Result = 0,
       others =>
         Index_Non_Blank'Result in Source'Range
         and then Source (Index_Non_Blank'Result) /= ' '
         and then
         (for all I in Source'Range =>
            (if
               I /= Index_Non_Blank'Result
               and then (I < Index_Non_Blank'Result) = (Going = Forward)
             then Source (I) = ' ')));
      --  Returns Index(Source, Maps.To_Set(Space), Outside, Going)

      ---------------------------------------
      -- String Transformation Subprograms --
      ---------------------------------------

   function Insert
     (Source   : String;
      Before   : Positive;
      New_Item : String)
      return String with
      Pre => Before >= Source'First
      and then (Source'Last = Integer'Last or else Before <= Source'Last + 1)
      and then New_Item'Length <= Integer'Last - Source'Length
      and then Source'First = 1 and then New_Item'First = 1,
      Post => Insert'Result'First = 1 and then Insert'Result = Source
            (Source'First .. Before - 1) & New_Item & Source
            (Before .. Source'Last);
      --  Propagates Index_Error if Before is not in Source'First ..
      --  Source'Last+1; otherwise, returns Source(Source'First..Before-1)
      --  & New_Item & Source(Before..Source'Last), but with lower bound 1.
      --  Beware of the overflow of the string length !

   function Overwrite
     (Source   : String;
      Position : Positive;
      New_Item : String)
      return String with
      Pre => Position >= Source'First
      and then (Source'Last = Integer'Last or else Position <= Source'Last + 1)
      and then New_Item'Length <= Integer'Last - (Position - Source'First + 1)
      and then Source'First = 1 and then New_Item'First = 1,
      Post => Overwrite'Result'First = 1 and then Overwrite'Result = Source
            (1 .. Position - 1) & New_Item &
          (if New_Item'Length > Source'Last - Position then "" else Source
               (Position + New_Item'Length .. Source'Last));
      --  Propagates Index_Error if Position is not in Source'First ..
      --  Source'Last+1; otherwise, returns the string obtained from Source
      --  by consecutively replacing characters starting at Position with
      --  corresponding characters from New_Item with lower bound 1. If the end
      --  of Source is reached before the characters in New_Item are exhausted,
      --  the remaining characters from New_Item are appended to the string.
      --  Beware of the overflow of the string length !

   function Delete
     (Source  : String;
      From    : Positive;
      Through : Natural)
      return String with
      Pre => From > Through
      or else (From in Source'Range and then Through <= Source'Last),
      Post           => Delete'Result'First = 1,
      Contract_Cases => (From > Through => Delete'Result = Source,
       others => Delete'Result = Source
             (Source'First .. From - 1) & (if Through < Source'Last then Source
                (Through + 1 .. Source'Last) else ""));
      --  If From > Through, the returned string is Source with lower bound
      --  1. If From not in Source'Range, or Through > Source'Last, then
      --  Index_Error is propagated. Otherwise, the returned string comprises
      --  Source(Source'First..From - 1) & Source(Through+1..Source'Last), but
      --  with lower bound 1.

      ---------------------------------
      -- String Selector Subprograms --
      ---------------------------------

   function Trim
     (Source : String;
      Left   : Maps.Character_Set;
      Right  : Maps.Character_Set)
      return String with
      Post =>
      (if
         (for all K in Source'Range => Is_In (Source (K), Left)) or
         (for all K in Source'Range => Is_In (Source (K), Right))
       then Trim'Result = ""
       else
         (for some Low in Source'Range =>
            (for some High in Source'Range =>
               Trim'Result = Source (Low .. High)
               and then not Is_In (Source (Low), Left)
               and then not Is_In (Source (High), Right)
               and then
               (for all K in Source'Range =>
                  (if K < Low then Is_In (Source (K), Left))
                  and then (if K > High then Is_In (Source (K), Right))))));
      --  Returns the string obtained by removing from Source all leading
      --  characters in Left and all trailing characters in Right.

   function Trim
     (Source : String;
      Side   : Trim_End)
      return String with
      Post =>
      (if (for all K in Source'Range => Source (K) = ' ') then Trim'Result = ""
       else
         (for some Low in Source'Range =>
            (for some High in Source'Range =>
               Trim'Result = Source (Low .. High)
               and then
               (if Side = Left then High = Source'Last
                else Source (High) /= ' ')
               and then
               (if Side = Right then Low = Source'First
                else Source (Low) /= ' ')
               and then
               (for all K in Source'Range =>
                  (if K < Low then Source (K) = ' ')
                  and then (if K > High then Source (K) = ' ')))));
      --  Returns the string obtained by removing from Source all leading Space
      --  characters (if Side = Left), all trailing Space characters (if Side
      --  = Right), or all leading and trailing Space characters (if Side =
      --  Both).

   function Head
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space)
      return String with
      Pre            => Source'First = 1,
      Post           => Head'Result'Length = Count,
      Contract_Cases => (Count <= Source'Length => Head'Result = Source
           (Source'First .. Count),
       others => Head'Result = Source & (1 .. Count - Source'Length => Pad));
      --  Returns a string of length Count. If Count <= Source'Length, the
      --  string comprises the first Count characters of Source. Otherwise,
      --  its contents are Source concatenated with Count-Source'Length Pad
      --  characters.

   function Tail
     (Source : String;
      Count  : Natural;
      Pad    : Character := Space)
      return String with
      Pre            => Source'First = 1,
      Post           => Tail'Result'Length = Count,
      Contract_Cases => (Count = 0 => Tail'Result = "",
       (Count in 1 .. Source'Length) => Tail'Result = Source
           (Source'Last - Count + 1 .. Source'Last),
       others => Tail'Result = (1 .. Count - Source'Length => Pad) & Source);
      --  Returns a string of length Count. If Count <= Source'Length, the
      --  string comprises the last Count characters of Source. Otherwise, its
      --  contents are Count-Source'Length Pad characters concatenated with
      --  Source.

      ----------------------------------
      -- String Constructor Functions --
      ----------------------------------

   function "*"
     (Left  : Natural;
      Right : Character)
      return String with
      Post => "*"'Result'Length = Left
      and then (for all E of "*"'Result => E = Right);
      --  This function replicates a character a specified number of times. It
      --  returns a string whose length is Left and each of whose elements is
      --  Right.

end String_Fixed;
