package body Conductor is

   task body Concert is
      Orchestra : Parts (1 .. Num_Parts);
   begin

      accept Initialize (Initial_Parts : Parts) do
         declare
            Part : Integer := 0;
         begin
            while Part in Orchestra'range
            loop
               Orchestra (Part) := Initial_Parts (Part);
               Part             := Part + 1;
            end loop;
         end;
      end Initialize;

      accept Play;  --  Start the concert

      loop
         select
            accept Advance (Step : Score.Duration) do
               declare
                  Part : Integer := 0;
               begin
                  while Part < Num_Parts
                  loop
                     declare
                        Pos : Positions renames Orchestra (Part).Pos;
                        Dur : Durations renames Orchestra (Part).Dur;
                     begin
                        for Index in Pos'range
                        loop
                           declare
                              --  Do not allow to advance beyond a new note as
                              --  it is the role of Start_Of_Note to do so
                              Min : constant Score.Duration :=
                                Score.Duration'min (Step, Dur (Index) - 1);
                           begin
                              Pos (Index) :=
                                Pos (Index) +
                                Position (Min / Score.Duration'last);
                           end;
                        end loop;
                     end;
                     Part := Part + 1;
                  end loop;
               end;
            end Advance;
         or
            accept Start_Of_Note
              (Index : Positive;
               Num   : Positive;
               Pit   : Pitch) do
               declare
                  Pos        : Position renames Orchestra (Index).Pos (Num);
                  Inter      : Interpreter    := Orchestra (Index).Inter;
                  Instr      : Instrument     := Orchestra (Index).Instr;
                  Dur        : Score.Duration := Orchestra (Index).Dur (Num);
                  Expect_Pit : Pitch;
                  Expect_Dur : Score.Duration;
                  Cumul_Dur  : Long_Duration  := Dur;
               begin
                  --  Look for the next note to be played
                  Get_Note (Inter, Instr, Pos, Dur, Expect_Pit, Expect_Dur);

                  --  If this note's pitch does not match, suppose that either
                  --  the player or the sensor missed a note and continue

                  while Pit /= Expect_Pit and then
                    --  Look for a note at the appropriate pitch
                  Dur <= Score.Duration'last
                    --  If it cannot be found one measure in advance, give up
                  loop
                     Cumul_Dur := Cumul_Dur + Expect_Dur;
                     Get_Note
                       (Inter, Instr, Pos, Cumul_Dur, Expect_Pit, Expect_Dur);
                  end loop;

                  if Pit = Expect_Pit
                  then
                     --  A matching note was found, so update the state
                     Pos := Pos + Position (Cumul_Dur / Score.Duration'last);
                     Dur := Expect_Dur;
                  end if;
               end;
            end Start_Of_Note;
         end select;
      end loop;

   end Concert;

end Conductor;
