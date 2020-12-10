pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with SDL_SDL_stdinc_h;

package SDL_SDL_mutex_h is


   SDL_MUTEX_TIMEDOUT : constant := 1;  --  ../include/SDL/SDL_mutex.h:44
   --  unsupported macro: SDL_MUTEX_MAXWAIT (~(Uint32)0)
   --  arg-macro: procedure SDL_LockMutex (m)
   --    SDL_mutexP(m)
   --  arg-macro: procedure SDL_UnlockMutex (m)
   --    SDL_mutexV(m)

   --  skipped empty struct SDL_mutex

   function SDL_CreateMutex return System.Address;  -- ../include/SDL/SDL_mutex.h:59
   pragma Import (C, SDL_CreateMutex, "SDL_CreateMutex");

   function SDL_mutexP (mutex : System.Address) return int;  -- ../include/SDL/SDL_mutex.h:65
   pragma Import (C, SDL_mutexP, "SDL_mutexP");

   function SDL_mutexV (mutex : System.Address) return int;  -- ../include/SDL/SDL_mutex.h:74
   pragma Import (C, SDL_mutexV, "SDL_mutexV");

   procedure SDL_DestroyMutex (mutex : System.Address);  -- ../include/SDL/SDL_mutex.h:77
   pragma Import (C, SDL_DestroyMutex, "SDL_DestroyMutex");

   --  skipped empty struct SDL_semaphore

   --  skipped empty struct SDL_sem

   function SDL_CreateSemaphore (initial_value : SDL_SDL_stdinc_h.Uint32) return System.Address;  -- ../include/SDL/SDL_mutex.h:90
   pragma Import (C, SDL_CreateSemaphore, "SDL_CreateSemaphore");

   procedure SDL_DestroySemaphore (sem : System.Address);  -- ../include/SDL/SDL_mutex.h:93
   pragma Import (C, SDL_DestroySemaphore, "SDL_DestroySemaphore");

   function SDL_SemWait (sem : System.Address) return int;  -- ../include/SDL/SDL_mutex.h:100
   pragma Import (C, SDL_SemWait, "SDL_SemWait");

   function SDL_SemTryWait (sem : System.Address) return int;  -- ../include/SDL/SDL_mutex.h:106
   pragma Import (C, SDL_SemTryWait, "SDL_SemTryWait");

   function SDL_SemWaitTimeout (sem : System.Address; ms : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL_mutex.h:115
   pragma Import (C, SDL_SemWaitTimeout, "SDL_SemWaitTimeout");

   function SDL_SemPost (sem : System.Address) return int;  -- ../include/SDL/SDL_mutex.h:120
   pragma Import (C, SDL_SemPost, "SDL_SemPost");

   function SDL_SemValue (sem : System.Address) return SDL_SDL_stdinc_h.Uint32;  -- ../include/SDL/SDL_mutex.h:123
   pragma Import (C, SDL_SemValue, "SDL_SemValue");

   --  skipped empty struct SDL_cond

   function SDL_CreateCond return System.Address;  -- ../include/SDL/SDL_mutex.h:138
   pragma Import (C, SDL_CreateCond, "SDL_CreateCond");

   procedure SDL_DestroyCond (cond : System.Address);  -- ../include/SDL/SDL_mutex.h:141
   pragma Import (C, SDL_DestroyCond, "SDL_DestroyCond");

   function SDL_CondSignal (cond : System.Address) return int;  -- ../include/SDL/SDL_mutex.h:146
   pragma Import (C, SDL_CondSignal, "SDL_CondSignal");

   function SDL_CondBroadcast (cond : System.Address) return int;  -- ../include/SDL/SDL_mutex.h:151
   pragma Import (C, SDL_CondBroadcast, "SDL_CondBroadcast");

   function SDL_CondWait (cond : System.Address; mut : System.Address) return int;  -- ../include/SDL/SDL_mutex.h:158
   pragma Import (C, SDL_CondWait, "SDL_CondWait");

   function SDL_CondWaitTimeout
     (cond : System.Address;
      mutex : System.Address;
      ms : SDL_SDL_stdinc_h.Uint32) return int;  -- ../include/SDL/SDL_mutex.h:166
   pragma Import (C, SDL_CondWaitTimeout, "SDL_CondWaitTimeout");

end SDL_SDL_mutex_h;
