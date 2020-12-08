pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;
with System;
with Interfaces.C.Strings;
limited with SDL_SDL_rwops_h;

package SDL_SDL_audio_h is


   AUDIO_U8 : constant := 16#0008#;  --  ../include/SDL/SDL_audio.h:100
   AUDIO_S8 : constant := 16#8008#;  --  ../include/SDL/SDL_audio.h:101
   AUDIO_U16LSB : constant := 16#0010#;  --  ../include/SDL/SDL_audio.h:102
   AUDIO_S16LSB : constant := 16#8010#;  --  ../include/SDL/SDL_audio.h:103
   AUDIO_U16MSB : constant := 16#1010#;  --  ../include/SDL/SDL_audio.h:104
   AUDIO_S16MSB : constant := 16#9010#;  --  ../include/SDL/SDL_audio.h:105
   --  unsupported macro: AUDIO_U16 AUDIO_U16LSB
   --  unsupported macro: AUDIO_S16 AUDIO_S16LSB
   --  unsupported macro: AUDIO_U16SYS AUDIO_U16LSB
   --  unsupported macro: AUDIO_S16SYS AUDIO_S16LSB
   --  arg-macro: procedure SDL_LoadWAV (file, spec, audSDL_LoadWAV_RW(SDL_RWFromFile(file, "rb"),1, spec,audio_buf,audio_len)
   --    SDL_LoadWAV_RW(SDL_RWFromFile(file, "rb"),1, spec,audio_buf,audio_len)

   SDL_MIX_MAXVOLUME : constant := 128;  --  ../include/SDL/SDL_audio.h:250

   type SDL_AudioSpec is record
      freq : aliased int;  -- ../include/SDL/SDL_audio.h:75
      format : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_audio.h:76
      channels : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_audio.h:77
      silence : aliased SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_audio.h:78
      samples : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_audio.h:79
      padding : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_audio.h:80
      size : aliased SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_audio.h:81
      callback : access procedure
           (arg1 : System.Address;
            arg2 : access SDL_SDL_stdinc_h.Uint8;
            arg3 : int);  -- ../include/SDL/SDL_audio.h:91
      userdata : System.Address;  -- ../include/SDL/SDL_audio.h:92
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_AudioSpec);  -- ../include/SDL/SDL_audio.h:74

   type SDL_AudioCVT_filters_array is array (0 .. 9) of access procedure (arg1 : System.Address; arg2 : SDL_SDL_stdinc_h.Uint16);
   type SDL_AudioCVT is record
      needed : aliased int;  -- ../include/SDL/SDL_audio.h:127
      src_format : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_audio.h:128
      dst_format : aliased SDL_SDL_stdinc_h.Uint16;  -- ../include/SDL/SDL_audio.h:129
      rate_incr : aliased double;  -- ../include/SDL/SDL_audio.h:130
      buf : access SDL_SDL_stdinc_h.Uint8;  -- ../include/SDL/SDL_audio.h:131
      len : aliased int;  -- ../include/SDL/SDL_audio.h:132
      len_cvt : aliased int;  -- ../include/SDL/SDL_audio.h:133
      len_mult : aliased int;  -- ../include/SDL/SDL_audio.h:134
      len_ratio : aliased double;  -- ../include/SDL/SDL_audio.h:135
      filters : aliased SDL_AudioCVT_filters_array;  -- ../include/SDL/SDL_audio.h:136
      filter_index : aliased int;  -- ../include/SDL/SDL_audio.h:137
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_AudioCVT);  -- ../include/SDL/SDL_audio.h:126

   function SDL_AudioInit (driver_name : Interfaces.C.Strings.chars_ptr) return int;  -- ../include/SDL/SDL_audio.h:150
   pragma Import (C, SDL_AudioInit, "SDL_AudioInit");

   procedure SDL_AudioQuit;  -- ../include/SDL/SDL_audio.h:151
   pragma Import (C, SDL_AudioQuit, "SDL_AudioQuit");

   function SDL_AudioDriverName (namebuf : Interfaces.C.Strings.chars_ptr; maxlen : int) return Interfaces.C.Strings.chars_ptr;  -- ../include/SDL/SDL_audio.h:159
   pragma Import (C, SDL_AudioDriverName, "SDL_AudioDriverName");

   function SDL_OpenAudio (desired : access SDL_AudioSpec; obtained : access SDL_AudioSpec) return int;  -- ../include/SDL/SDL_audio.h:178
   pragma Import (C, SDL_OpenAudio, "SDL_OpenAudio");

   type SDL_audiostatus is 
     (SDL_AUDIO_STOPPED,
      SDL_AUDIO_PLAYING,
      SDL_AUDIO_PAUSED);
   pragma Convention (C, SDL_audiostatus);  -- ../include/SDL/SDL_audio.h:184

   function SDL_GetAudioStatus return SDL_audiostatus;  -- ../include/SDL/SDL_audio.h:187
   pragma Import (C, SDL_GetAudioStatus, "SDL_GetAudioStatus");

   procedure SDL_PauseAudio (pause_on : int);  -- ../include/SDL/SDL_audio.h:196
   pragma Import (C, SDL_PauseAudio, "SDL_PauseAudio");

   function SDL_LoadWAV_RW
     (src : access SDL_SDL_rwops_h.SDL_RWops;
      freesrc : int;
      spec : access SDL_AudioSpec;
      audio_buf : System.Address;
      audio_len : access SDL_SDL_stdinc_h.Uint32) return access SDL_AudioSpec;  -- ../include/SDL/SDL_audio.h:215
   pragma Import (C, SDL_LoadWAV_RW, "SDL_LoadWAV_RW");

   procedure SDL_FreeWAV (audio_buf : access SDL_SDL_stdinc_h.Uint8);  -- ../include/SDL/SDL_audio.h:224
   pragma Import (C, SDL_FreeWAV, "SDL_FreeWAV");

   function SDL_BuildAudioCVT
     (cvt : access SDL_AudioCVT;
      src_format : SDL_SDL_stdinc_h.Uint16;
      src_channels : SDL_SDL_stdinc_h.Uint8;
      src_rate : int;
      dst_format : SDL_SDL_stdinc_h.Uint16;
      dst_channels : SDL_SDL_stdinc_h.Uint8;
      dst_rate : int) return int;  -- ../include/SDL/SDL_audio.h:234
   pragma Import (C, SDL_BuildAudioCVT, "SDL_BuildAudioCVT");

   function SDL_ConvertAudio (cvt : access SDL_AudioCVT) return int;  -- ../include/SDL/SDL_audio.h:247
   pragma Import (C, SDL_ConvertAudio, "SDL_ConvertAudio");

   procedure SDL_MixAudio
     (dst : access SDL_SDL_stdinc_h.Uint8;
      src : access SDL_SDL_stdinc_h.Uint8;
      len : SDL_SDL_stdinc_h.Uint32;
      volume : int);  -- ../include/SDL/SDL_audio.h:258
   pragma Import (C, SDL_MixAudio, "SDL_MixAudio");

   procedure SDL_LockAudio;  -- ../include/SDL/SDL_audio.h:268
   pragma Import (C, SDL_LockAudio, "SDL_LockAudio");

   procedure SDL_UnlockAudio;  -- ../include/SDL/SDL_audio.h:269
   pragma Import (C, SDL_UnlockAudio, "SDL_UnlockAudio");

   procedure SDL_CloseAudio;  -- ../include/SDL/SDL_audio.h:275
   pragma Import (C, SDL_CloseAudio, "SDL_CloseAudio");

end SDL_SDL_audio_h;
