pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with System;

package SDL_atomic_h is

   --  arg-macro: procedure SDL_CompilerBarrier ()
   --    __asm__ __volatile__ ("" : : : & "memory")
   --  arg-macro: procedure SDL_MemoryBarrierRelease ()
   --    SDL_CompilerBarrier()
   --  arg-macro: procedure SDL_MemoryBarrierAcquire ()
   --    SDL_CompilerBarrier()
   --  arg-macro: procedure SDL_AtomicIncRef (a)
   --    SDL_AtomicAdd(a, 1)
   --  arg-macro: function SDL_AtomicDecRef (a)
   --    return SDL_AtomicAdd(a, -1) = 1;
  --  Simple DirectMedia Layer
  --  Copyright (C) 1997-2018 Sam Lantinga <slouken@libsdl.org>
  --  This software is provided 'as-is', without any express or implied
  --  warranty.  In no event will the authors be held liable for any damages
  --  arising from the use of this software.
  --  Permission is granted to anyone to use this software for any purpose,
  --  including commercial applications, and to alter it and redistribute it
  --  freely, subject to the following restrictions:
  --  1. The origin of this software must not be misrepresented; you must not
  --     claim that you wrote the original software. If you use this software
  --     in a product, an acknowledgment in the product documentation would be
  --     appreciated but is not required.
  --  2. Altered source versions must be plainly marked as such, and must not be
  --     misrepresented as being the original software.
  --  3. This notice may not be removed or altered from any source distribution.
  -- 

  --*
  -- * \file SDL_atomic.h
  -- *
  -- * Atomic operations.
  -- *
  -- * IMPORTANT:
  -- * If you are not an expert in concurrent lockless programming, you should
  -- * only be using the atomic lock and reference counting functions in this
  -- * file.  In all other cases you should be protecting your data structures
  -- * with full mutexes.
  -- *
  -- * The list of "safe" functions to use are:
  -- *  SDL_AtomicLock()
  -- *  SDL_AtomicUnlock()
  -- *  SDL_AtomicIncRef()
  -- *  SDL_AtomicDecRef()
  -- *
  -- * Seriously, here be dragons!
  -- * ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- *
  -- * You can find out a little more about lockless programming and the
  -- * subtle issues that can arise here:
  -- * http://msdn.microsoft.com/en-us/library/ee418650%28v=vs.85%29.aspx
  -- *
  -- * There's also lots of good information here:
  -- * http://www.1024cores.net/home/lock-free-algorithms
  -- * http://preshing.com/
  -- *
  -- * These operations may or may not actually be implemented using
  -- * processor specific atomic operations. When possible they are
  -- * implemented as true processor specific atomic operations. When that
  -- * is not possible the are implemented using locks that *do* use the
  -- * available atomic operations.
  -- *
  -- * All of the atomic operations that modify memory are full memory barriers.
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- * \name SDL AtomicLock
  -- *
  -- * The atomic locks are efficient spinlocks using CPU instructions,
  -- * but are vulnerable to starvation and can spin forever if a thread
  -- * holding a lock has been terminated.  For this reason you should
  -- * minimize the code executed inside an atomic lock and never do
  -- * expensive things like API or system calls while holding them.
  -- *
  -- * The atomic locks are not safe to lock recursively.
  -- *
  -- * Porting Note:
  -- * The spin lock functions and type are required and can not be
  -- * emulated because they are used in the atomic emulation code.
  --  

  -- @{  
   subtype SDL_SpinLock is int;  -- ..\SDL2_tmp\SDL_atomic.h:89

  --*
  -- * \brief Try to lock a spin lock by setting it to a non-zero value.
  -- *
  -- * \param lock Points to the lock.
  -- *
  -- * \return SDL_TRUE if the lock succeeded, SDL_FALSE if the lock is already held.
  --  

   function SDL_AtomicTryLock (lock : access SDL_SpinLock) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_atomic.h:98
   pragma Import (C, SDL_AtomicTryLock, "SDL_AtomicTryLock");

  --*
  -- * \brief Lock a spin lock by setting it to a non-zero value.
  -- *
  -- * \param lock Points to the lock.
  --  

   procedure SDL_AtomicLock (lock : access SDL_SpinLock);  -- ..\SDL2_tmp\SDL_atomic.h:105
   pragma Import (C, SDL_AtomicLock, "SDL_AtomicLock");

  --*
  -- * \brief Unlock a spin lock by setting it to 0. Always returns immediately
  -- *
  -- * \param lock Points to the lock.
  --  

   procedure SDL_AtomicUnlock (lock : access SDL_SpinLock);  -- ..\SDL2_tmp\SDL_atomic.h:112
   pragma Import (C, SDL_AtomicUnlock, "SDL_AtomicUnlock");

  -- @}  
  -- SDL AtomicLock  
  --*
  -- * The compiler barrier prevents the compiler from reordering
  -- * reads and writes to globally visible variables across the call.
  --  

  -- This is correct for all CPUs when using GCC or Solaris Studio 12.1+.  
  --*
  -- * Memory barriers are designed to prevent reads and writes from being
  -- * reordered by the compiler and being seen out of order on multi-core CPUs.
  -- *
  -- * A typical pattern would be for thread A to write some data and a flag,
  -- * and for thread B to read the flag and get the data. In this case you
  -- * would insert a release barrier between writing the data and the flag,
  -- * guaranteeing that the data write completes no later than the flag is
  -- * written, and you would insert an acquire barrier between reading the
  -- * flag and reading the data, to ensure that all the reads associated
  -- * with the flag have completed.
  -- *
  -- * In this pattern you should always see a release barrier paired with
  -- * an acquire barrier and you should gate the data reads/writes with a
  -- * single flag variable.
  -- *
  -- * For more information on these semantics, take a look at the blog post:
  -- * http://preshing.com/20120913/acquire-and-release-semantics
  --  

   procedure SDL_MemoryBarrierReleaseFunction;  -- ..\SDL2_tmp\SDL_atomic.h:155
   pragma Import (C, SDL_MemoryBarrierReleaseFunction, "SDL_MemoryBarrierReleaseFunction");

   procedure SDL_MemoryBarrierAcquireFunction;  -- ..\SDL2_tmp\SDL_atomic.h:156
   pragma Import (C, SDL_MemoryBarrierAcquireFunction, "SDL_MemoryBarrierAcquireFunction");

  -- The mcr instruction isn't available in thumb mode, use real functions  
  -- This is correct for all CPUs on Solaris when using Solaris Studio 12.1+.  
  -- This is correct for the x86 and x64 CPUs, and we'll expand this over time.  
  --*
  -- * \brief A type representing an atomic integer value.  It is a struct
  -- *        so people don't accidentally use numeric operations on it.
  --  

   --  skipped anonymous struct anon_5

   type SDL_atomic_t is record
      value : aliased int;  -- ..\SDL2_tmp\SDL_atomic.h:198
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_atomic_t);  -- ..\SDL2_tmp\SDL_atomic.h:198

  --*
  -- * \brief Set an atomic variable to a new value if it is currently an old value.
  -- *
  -- * \return SDL_TRUE if the atomic variable was set, SDL_FALSE otherwise.
  -- *
  -- * \note If you don't know what this function is for, you shouldn't use it!
  -- 

   function SDL_AtomicCAS
     (a : access SDL_atomic_t;
      oldval : int;
      newval : int) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_atomic.h:207
   pragma Import (C, SDL_AtomicCAS, "SDL_AtomicCAS");

  --*
  -- * \brief Set an atomic variable to a value.
  -- *
  -- * \return The previous value of the atomic variable.
  --  

   function SDL_AtomicSet (a : access SDL_atomic_t; v : int) return int;  -- ..\SDL2_tmp\SDL_atomic.h:214
   pragma Import (C, SDL_AtomicSet, "SDL_AtomicSet");

  --*
  -- * \brief Get the value of an atomic variable
  --  

   function SDL_AtomicGet (a : access SDL_atomic_t) return int;  -- ..\SDL2_tmp\SDL_atomic.h:219
   pragma Import (C, SDL_AtomicGet, "SDL_AtomicGet");

  --*
  -- * \brief Add to an atomic variable.
  -- *
  -- * \return The previous value of the atomic variable.
  -- *
  -- * \note This same style can be used for any number operation
  --  

   function SDL_AtomicAdd (a : access SDL_atomic_t; v : int) return int;  -- ..\SDL2_tmp\SDL_atomic.h:228
   pragma Import (C, SDL_AtomicAdd, "SDL_AtomicAdd");

  --*
  -- * \brief Increment an atomic variable used as a reference count.
  --  

  --*
  -- * \brief Decrement an atomic variable used as a reference count.
  -- *
  -- * \return SDL_TRUE if the variable reached zero after decrementing,
  -- *         SDL_FALSE otherwise
  --  

  --*
  -- * \brief Set a pointer to a new value if it is currently an old value.
  -- *
  -- * \return SDL_TRUE if the pointer was set, SDL_FALSE otherwise.
  -- *
  -- * \note If you don't know what this function is for, you shouldn't use it!
  -- 

   function SDL_AtomicCASPtr
     (a : System.Address;
      oldval : System.Address;
      newval : System.Address) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_atomic.h:254
   pragma Import (C, SDL_AtomicCASPtr, "SDL_AtomicCASPtr");

  --*
  -- * \brief Set a pointer to a value atomically.
  -- *
  -- * \return The previous value of the pointer.
  --  

   function SDL_AtomicSetPtr (a : System.Address; v : System.Address) return System.Address;  -- ..\SDL2_tmp\SDL_atomic.h:261
   pragma Import (C, SDL_AtomicSetPtr, "SDL_AtomicSetPtr");

  --*
  -- * \brief Get the value of a pointer atomically.
  --  

   function SDL_AtomicGetPtr (a : System.Address) return System.Address;  -- ..\SDL2_tmp\SDL_atomic.h:266
   pragma Import (C, SDL_AtomicGetPtr, "SDL_AtomicGetPtr");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_atomic_h;
