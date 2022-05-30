with Score; use Score;

package Conductor is

   type Positions is array (Positive range <>) of Position;
   type Durations is array (Positive range <>) of Score.Duration;

   type Part (Max_Players : Positive) is record
      Inter       : Interpreter;  --  Interpreter for this part
      Instr       : Instrument;   --  Instrument for this part
      Num_Players : Positive;     --  Number of players in 1 .. Max_Players
      Pos         : Positions (1 .. Max_Players);
      --  Current position of each player in the score
      Dur : Durations (1 .. Max_Players);
      --  Remaining duration of the current note for each player
   end record;

   type Parts is array (Positive range <>) of Part (5);

   task type Concert (Num_Parts : Positive) is
      --  Initialize the parts to be played
      entry Initialize (Initial_Parts : Parts);
      --  Signal that a player starts a note
      entry Start_Of_Note
        (Index : Positive;  --  Index of the Part being played in the array
         Num   : Positive;  --  Number of the player in the Part
         Pit   : Pitch);    --  Pitch of the note
      --  Advance the position of all players by a Step, which is expected to
      --  be much smaller than the interval between two consecutive notes
      entry Advance (Step : Score.Duration);
      --  Start of the concert
      entry Play;
   end Concert;

end Conductor;
