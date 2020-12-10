pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with umingw_h;
with System;

package process_h is

   --  unsupported macro: P_WAIT _P_WAIT
   --  unsupported macro: P_NOWAIT _P_NOWAIT
   --  unsupported macro: P_OVERLAY _P_OVERLAY
   --  unsupported macro: OLD_P_OVERLAY _OLD_P_OVERLAY
   --  unsupported macro: P_NOWAITO _P_NOWAITO
   --  unsupported macro: P_DETACH _P_DETACH
   --  unsupported macro: WAIT_CHILD _WAIT_CHILD
   --  unsupported macro: WAIT_GRANDCHILD _WAIT_GRANDCHILD
  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

  -- Includes a definition of _pid_t and pid_t  
   --  skipped func _beginthread

   --  skipped func _endthread

   --  skipped func _beginthreadex

   --  skipped func _endthreadex

   procedure c_exit (u_Code : int);  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:36
   pragma Import (C, c_exit, "exit");

   --  skipped func _exit

  -- C99 function name  
   --  skipped func _Exit

   procedure c_abort;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:50
   pragma Import (C, c_abort, "abort");

   --  skipped func _cexit

   --  skipped func _c_exit

   --  skipped func _getpid

   --  skipped func _cwait

   --  skipped func _execl

   --  skipped func _execle

   --  skipped func _execlp

   --  skipped func _execlpe

   --  skipped func _execv

   --  skipped func _execve

   --  skipped func _execvp

   --  skipped func _execvpe

   --  skipped func _spawnl

   --  skipped func _spawnle

   --  skipped func _spawnlp

   --  skipped func _spawnlpe

   --  skipped func _spawnv

   --  skipped func _spawnve

   --  skipped func _spawnvp

   --  skipped func _spawnvpe

   function c_system (u_Command : Interfaces.C.Strings.chars_ptr) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:78
   pragma Import (C, c_system, "system");

   --  skipped func _wexecl

   --  skipped func _wexecle

   --  skipped func _wexeclp

   --  skipped func _wexeclpe

   --  skipped func _wexecv

   --  skipped func _wexecve

   --  skipped func _wexecvp

   --  skipped func _wexecvpe

   --  skipped func _wspawnl

   --  skipped func _wspawnle

   --  skipped func _wspawnlp

   --  skipped func _wspawnlpe

   --  skipped func _wspawnv

   --  skipped func _wspawnve

   --  skipped func _wspawnvp

   --  skipped func _wspawnvpe

   --  skipped func _wsystem

   --  skipped func __security_init_cookie

   --  skipped func __security_check_cookie

   --  skipped func __report_gsfailure

   --  skipped func _loaddll

   --  skipped func _unloaddll

   --  skipped func _getdllprocaddr

   function cwait
     (u_TermStat : access int;
      u_ProcHandle : umingw_h.intptr_t;
      u_Action : int) return umingw_h.intptr_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:143
   pragma Import (C, cwait, "cwait");

   function execl (u_Filename : Interfaces.C.Strings.chars_ptr; u_ArgList : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:145
   pragma Import (C, execl, "execl");

   function execle (u_Filename : Interfaces.C.Strings.chars_ptr; u_ArgList : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:146
   pragma Import (C, execle, "execle");

   function execlp (u_Filename : Interfaces.C.Strings.chars_ptr; u_ArgList : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:147
   pragma Import (C, execlp, "execlp");

   function execlpe (u_Filename : Interfaces.C.Strings.chars_ptr; u_ArgList : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:148
   pragma Import (C, execlpe, "execlpe");

   function spawnl
     (arg1 : int;
      u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return umingw_h.intptr_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:155
   pragma Import (C, spawnl, "spawnl");

   function spawnle
     (arg1 : int;
      u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return umingw_h.intptr_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:156
   pragma Import (C, spawnle, "spawnle");

   function spawnlp
     (arg1 : int;
      u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return umingw_h.intptr_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:157
   pragma Import (C, spawnlp, "spawnlp");

   function spawnlpe
     (arg1 : int;
      u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return umingw_h.intptr_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:158
   pragma Import (C, spawnlpe, "spawnlpe");

   function getpid return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:159
   pragma Import (C, getpid, "getpid");

  -- Those methods are predefined by gcc builtins to return int. So to prevent
  --     stupid warnings, define them in POSIX way.  This is save, because those
  --     methods do not return in success case, so that the return value is not
  --     really dependent to its scalar width.   

   function execv (u_Filename : Interfaces.C.Strings.chars_ptr; u_ArgList : System.Address) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:165
   pragma Import (C, execv, "execv");

   function execve
     (u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : System.Address;
      u_Env : System.Address) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:166
   pragma Import (C, execve, "execve");

   function execvp (u_Filename : Interfaces.C.Strings.chars_ptr; u_ArgList : System.Address) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:167
   pragma Import (C, execvp, "execvp");

   function execvpe
     (u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : System.Address;
      u_Env : System.Address) return int;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:168
   pragma Import (C, execvpe, "execvpe");

   function spawnv
     (arg1 : int;
      u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : System.Address) return umingw_h.intptr_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:175
   pragma Import (C, spawnv, "spawnv");

   function spawnve
     (arg1 : int;
      u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : System.Address;
      u_Env : System.Address) return umingw_h.intptr_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:176
   pragma Import (C, spawnve, "spawnve");

   function spawnvp
     (arg1 : int;
      u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : System.Address) return umingw_h.intptr_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:177
   pragma Import (C, spawnvp, "spawnvp");

   function spawnvpe
     (arg1 : int;
      u_Filename : Interfaces.C.Strings.chars_ptr;
      u_ArgList : System.Address;
      u_Env : System.Address) return umingw_h.intptr_t;  -- d:\install\gpl2018\x86_64-pc-mingw32\include\process.h:178
   pragma Import (C, spawnvpe, "spawnvpe");

end process_h;
