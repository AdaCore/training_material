package Score is

   type Pitch is new Natural range 0 .. 127;
   --  MIDI note in the MIDI Tuning Standard, only representing semitones here

   type Long_Duration is new Natural;
   subtype Duration is Long_Duration range 0 .. 96;
   --  Fraction of a measure, 96 representing the complete measure, which
   --  allows representing from thirty-second note to whole note for measures
   --  with one to four beats

   type Position is delta 10.0**(-2) range 0.0 .. 10_000.0;
   --  Distance from the start of the score in number of measures

   type Instrument is
     (Flute, Recorder, Violin, Trumpet, Clarinet, Oboe, Cornet, Flugelhorn,
      Piccolo_Trumpet, Piccolo, Alto_Saxophone, Alto_Flute, Viola, Horn,
      Alto_Horn, Trombone, Tenor_Saxophone, Bass_Trumpet, Bassoon,
      English_Horn, Baritone_Saxophone, Baritone_Horn, Bass_Clarinet, Cello,
      Euphonium, Bass_Trombone, Contrabassoon, Bass_Saxophone, Double_Bass,
      Tuba, Contrabass_Saxophone, Contrabass_Bugle);

   type Interpreter is (First, Second, Third, Fourth, Fifth);

   procedure Get_Note
     (Inter :     Interpreter;
      Instr :     Instrument;
      Pos   :     Position;
      Fwd   :     Long_Duration;
      Pit   : out Pitch;
      Dur   : out Duration);
   --  Return the Pitch and Duration of the note starting forward by Fwd from
   --  position Pos for the Interpreter of the Instrument

end Score;
