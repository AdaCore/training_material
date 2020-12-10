pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;
with Interfaces.C.Strings;

package SDL_SDL_cdrom_h is


   SDL_MAX_TRACKS : constant := 99;  --  ../include/SDL/SDL_cdrom.h:48

   SDL_AUDIO_TRACK : constant := 16#00#;  --  ../include/SDL/SDL_cdrom.h:54
   SDL_DATA_TRACK : constant := 16#04#;  --  ../include/SDL/SDL_cdrom.h:55
   --  arg-macro: function CD_INDRIVE (status)
   --    return (int)(status) > 0;

   CD_FPS : constant := 75;  --  ../include/SDL/SDL_cdrom.h:96
   --  arg-macro: procedure FRAMES_TO_MSF (f, M, S, F)
   --    { int value := f; *(F) := valuemodCD_FPS; value /= CD_FPS; *(S) := valuemod60; value /= 60; *(M) := value; }
   --  arg-macro: function MSF_TO_FRAMES (M, S, F)
   --    return (M)*60*CD_FPS+(S)*CD_FPS+(F);

   subtype CDstatus is unsigned;
   CD_TRAYEMPTY : constant CDstatus := 0;
   CD_STOPPED : constant CDstatus := 1;
   CD_PLAYING : constant CDstatus := 2;
   CD_PAUSED : constant CDstatus := 3;
   CD_ERROR : constant CDstatus := -1;  -- ../include/SDL/SDL_cdrom.h:65

   type SDL_CDtrack is record
      id : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_cdrom.h:71
      c_type : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_cdrom.h:72
      unused : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_cdrom.h:73
      length : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_cdrom.h:74
      offset : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_cdrom.h:75
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_CDtrack);  -- ../include/SDL/SDL_cdrom.h:70

   type SDL_CD_track_array is array (0 .. 99) of aliased SDL_CDtrack;
   type SDL_CD is record
      id : aliased int;  -- ../include/SDL/SDL_cdrom.h:80
      status : aliased CDstatus;  -- ../include/SDL/SDL_cdrom.h:81
      numtracks : aliased int;  -- ../include/SDL/SDL_cdrom.h:85
      cur_track : aliased int;  -- ../include/SDL/SDL_cdrom.h:86
      cur_frame : aliased int;  -- ../include/SDL/SDL_cdrom.h:87
      track : aliased SDL_CD_track_array;  -- ../include/SDL/SDL_cdrom.h:88
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_CD);  -- ../include/SDL/SDL_cdrom.h:79

   function SDL_CDNumDrives return int;  -- ../include/SDL/SDL_cdrom.h:114
   pragma Import (C, SDL_CDNumDrives, "SDL_CDNumDrives");

   function SDL_CDName (drive : int) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_cdrom.h:123
   pragma Import (C, SDL_CDName, "SDL_CDName");

   function SDL_CDOpen (drive : int) return access SDL_CD;  -- ../include/SDL/SDL_cdrom.h:132
   pragma Import (C, SDL_CDOpen, "SDL_CDOpen");

   function SDL_CDStatus (cdrom : access SDL_CD) return CDstatus;  -- ../include/SDL/SDL_cdrom.h:139
   pragma Import (C, SDL_CDStatus, "SDL_CDStatus");

   function SDL_CDPlayTracks
     (cdrom : access SDL_CD;
      start_track : int;
      start_frame : int;
      ntracks : int;
      nframes : int) return int;  -- ../include/SDL/SDL_cdrom.h:163
   pragma Import (C, SDL_CDPlayTracks, "SDL_CDPlayTracks");

   function SDL_CDPlay
     (cdrom : access SDL_CD;
      start : int;
      length : int) return int;  -- ../include/SDL/SDL_cdrom.h:170
   pragma Import (C, SDL_CDPlay, "SDL_CDPlay");

   function SDL_CDPause (cdrom : access SDL_CD) return int;  -- ../include/SDL/SDL_cdrom.h:175
   pragma Import (C, SDL_CDPause, "SDL_CDPause");

   function SDL_CDResume (cdrom : access SDL_CD) return int;  -- ../include/SDL/SDL_cdrom.h:180
   pragma Import (C, SDL_CDResume, "SDL_CDResume");

   function SDL_CDStop (cdrom : access SDL_CD) return int;  -- ../include/SDL/SDL_cdrom.h:185
   pragma Import (C, SDL_CDStop, "SDL_CDStop");

   function SDL_CDEject (cdrom : access SDL_CD) return int;  -- ../include/SDL/SDL_cdrom.h:190
   pragma Import (C, SDL_CDEject, "SDL_CDEject");

   procedure SDL_CDClose (cdrom : access SDL_CD);  -- ../include/SDL/SDL_cdrom.h:193
   pragma Import (C, SDL_CDClose, "SDL_CDClose");

end SDL_SDL_cdrom_h;
