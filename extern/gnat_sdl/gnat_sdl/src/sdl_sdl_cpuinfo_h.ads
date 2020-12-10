pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_SDL_stdinc_h;

package SDL_SDL_cpuinfo_h is

   function SDL_HasRDTSC return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_cpuinfo.h:40
   pragma Import (C, SDL_HasRDTSC, "SDL_HasRDTSC");

   function SDL_HasMMX return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_cpuinfo.h:43
   pragma Import (C, SDL_HasMMX, "SDL_HasMMX");

   function SDL_HasMMXExt return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_cpuinfo.h:46
   pragma Import (C, SDL_HasMMXExt, "SDL_HasMMXExt");

   function SDL_Has3DNow return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_cpuinfo.h:49
   pragma Import (C, SDL_Has3DNow, "SDL_Has3DNow");

   function SDL_Has3DNowExt return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_cpuinfo.h:52
   pragma Import (C, SDL_Has3DNowExt, "SDL_Has3DNowExt");

   function SDL_HasSSE return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_cpuinfo.h:55
   pragma Import (C, SDL_HasSSE, "SDL_HasSSE");

   function SDL_HasSSE2 return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_cpuinfo.h:58
   pragma Import (C, SDL_HasSSE2, "SDL_HasSSE2");

   function SDL_HasAltiVec return SDL_SDL_stdinc_h.SDL_bool;  -- ../include/SDL/SDL_cpuinfo.h:61
   pragma Import (C, SDL_HasAltiVec, "SDL_HasAltiVec");

end SDL_SDL_cpuinfo_h;
