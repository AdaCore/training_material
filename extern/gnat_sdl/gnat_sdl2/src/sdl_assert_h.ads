pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package SDL_assert_h is

   SDL_ASSERT_LEVEL : constant := 2;  --  ..\SDL2_tmp\SDL_assert.h:38
   --  arg-macro: procedure SDL_TriggerBreakpoint ()
   --    __asm__ __volatile__ ( "int $3" & ASCII.LF & "" & ASCII.HT & "" )
   --  unsupported macro: SDL_FUNCTION __FUNCTION__
   --  unsupported macro: SDL_FILE __FILE__
   --  unsupported macro: SDL_LINE __LINE__

   SDL_NULL_WHILE_LOOP_CONDITION : constant := (0);  --  ..\SDL2_tmp\SDL_assert.h:96
   --  arg-macro: procedure SDL_disabled_assert (condition)
   --    do { (void) sizeof ((condition)); } while (SDL_NULL_WHILE_LOOP_CONDITION)
   --  unsupported macro: SDL_enabled_assert(condition) do { while ( !(condition) ) { static struct SDL_AssertData sdl_assert_data = { 0, 0, #condition, 0, 0, 0, 0 }; const SDL_AssertState sdl_assert_state = SDL_ReportAssertion(&sdl_assert_data, SDL_FUNCTION, SDL_FILE, SDL_LINE); if (sdl_assert_state == SDL_ASSERTION_RETRY) { continue; } else if (sdl_assert_state == SDL_ASSERTION_BREAK) { SDL_TriggerBreakpoint(); } break; } } while (SDL_NULL_WHILE_LOOP_CONDITION)
   --  arg-macro: procedure SDL_assert (condition)
   --    SDL_enabled_assert(condition)
   --  arg-macro: procedure SDL_assert_release (condition)
   --    SDL_enabled_assert(condition)
   --  arg-macro: procedure SDL_assert_paranoid (condition)
   --    SDL_disabled_assert(condition)
   --  arg-macro: procedure SDL_assert_always (condition)
   --    SDL_enabled_assert(condition)
   --  unsupported macro: SDL_assert_state SDL_AssertState
   --  unsupported macro: SDL_assert_data SDL_AssertData

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

  -- Set up for C function definitions, even when using C++  
  --These are macros and not first class functions so that the debugger breaks
  --on the assertion line and not in some random guts of SDL, and so each
  --assert can have unique static variables associated with it.
  -- 

  -- Don't include intrin.h here because it contains C++ code  
  -- How do we trigger breakpoints on this platform?  
  --sizeof (x) makes the compiler still parse the expression even without
  --assertions enabled, so the code is always checked at compile time, but
  --doesn't actually generate code for it, so there are no side effects or
  --expensive checks at run time, just the constant size of what x WOULD be,
  --which presumably gets optimized out as unused.
  --This also solves the problem of...
  --    int somevalue = blah();
  --    SDL_assert(somevalue == 1);
  --...which would cause compiles to complain that somevalue is unused if we
  --disable assertions.
  -- 

  -- "while (0,0)" fools Microsoft's compiler's /W4 warning level into thinking
  --    this condition isn't constant. And looks like an owl's face!  

  --*< Retry the assert immediately.  
  --*< Make the debugger trigger a breakpoint.  
  --*< Terminate the program.  
  --*< Ignore the assert.  
  --*< Ignore the assert from now on.  
   type SDL_AssertState is 
     (SDL_ASSERTION_RETRY,
      SDL_ASSERTION_BREAK,
      SDL_ASSERTION_ABORT,
      SDL_ASSERTION_IGNORE,
      SDL_ASSERTION_ALWAYS_IGNORE);
   pragma Convention (C, SDL_AssertState);  -- ..\SDL2_tmp\SDL_assert.h:109

   type SDL_AssertData;
   type SDL_AssertData is record
      always_ignore : aliased int;  -- ..\SDL2_tmp\SDL_assert.h:113
      trigger_count : aliased unsigned;  -- ..\SDL2_tmp\SDL_assert.h:114
      condition : Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_assert.h:115
      filename : Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_assert.h:116
      linenum : aliased int;  -- ..\SDL2_tmp\SDL_assert.h:117
      c_function : Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_assert.h:118
      next : access constant SDL_AssertData;  -- ..\SDL2_tmp\SDL_assert.h:119
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_AssertData);  -- ..\SDL2_tmp\SDL_assert.h:111

  -- Never call this directly. Use the SDL_assert* macros.  
   function SDL_ReportAssertion
     (arg1 : access SDL_AssertData;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : int) return SDL_AssertState;  -- ..\SDL2_tmp\SDL_assert.h:125
   pragma Import (C, SDL_ReportAssertion, "SDL_ReportAssertion");

  -- this tells Clang's static analysis that we're a custom assert function,
  --   and that the analyzer should assume the condition was always true past this
  --   SDL_assert test.  

  -- the do {} while(0) avoids dangling else problems:
  --    if (x) SDL_assert(y); else blah();
  --       ... without the do/while, the "else" could attach to this macro's "if".
  --   We try to handle just the minimum we need here in a macro...the loop,
  --   the static vars, and break points. The heavy lifting is handled in
  --   SDL_ReportAssertion(), in SDL_assert.c.
  -- 

  -- Enable various levels of assertions.  
  -- this assertion is never disabled at any level.  
   type SDL_AssertionHandler is access function (arg1 : access constant SDL_AssertData; arg2 : System.Address) return SDL_AssertState;
   pragma Convention (C, SDL_AssertionHandler);  -- ..\SDL2_tmp\SDL_assert.h:188

  --*
  -- *  \brief Set an application-defined assertion handler.
  -- *
  -- *  This allows an app to show its own assertion UI and/or force the
  -- *  response to an assertion failure. If the app doesn't provide this, SDL
  -- *  will try to do the right thing, popping up a system-specific GUI dialog,
  -- *  and probably minimizing any fullscreen windows.
  -- *
  -- *  This callback may fire from any thread, but it runs wrapped in a mutex, so
  -- *  it will only fire from one thread at a time.
  -- *
  -- *  Setting the callback to NULL restores SDL's original internal handler.
  -- *
  -- *  This callback is NOT reset to SDL's internal handler upon SDL_Quit()!
  -- *
  -- *  Return SDL_AssertState value of how to handle the assertion failure.
  -- *
  -- *  \param handler Callback function, called when an assertion fails.
  -- *  \param userdata A pointer passed to the callback as-is.
  --  

   procedure SDL_SetAssertionHandler (handler : SDL_AssertionHandler; userdata : System.Address);  -- ..\SDL2_tmp\SDL_assert.h:211
   pragma Import (C, SDL_SetAssertionHandler, "SDL_SetAssertionHandler");

  --*
  -- *  \brief Get the default assertion handler.
  -- *
  -- *  This returns the function pointer that is called by default when an
  -- *   assertion is triggered. This is an internal function provided by SDL,
  -- *   that is used for assertions when SDL_SetAssertionHandler() hasn't been
  -- *   used to provide a different function.
  -- *
  -- *  \return The default SDL_AssertionHandler that is called when an assert triggers.
  --  

   function SDL_GetDefaultAssertionHandler return SDL_AssertionHandler;  -- ..\SDL2_tmp\SDL_assert.h:225
   pragma Import (C, SDL_GetDefaultAssertionHandler, "SDL_GetDefaultAssertionHandler");

  --*
  -- *  \brief Get the current assertion handler.
  -- *
  -- *  This returns the function pointer that is called when an assertion is
  -- *   triggered. This is either the value last passed to
  -- *   SDL_SetAssertionHandler(), or if no application-specified function is
  -- *   set, is equivalent to calling SDL_GetDefaultAssertionHandler().
  -- *
  -- *   \param puserdata Pointer to a void*, which will store the "userdata"
  -- *                    pointer that was passed to SDL_SetAssertionHandler().
  -- *                    This value will always be NULL for the default handler.
  -- *                    If you don't care about this data, it is safe to pass
  -- *                    a NULL pointer to this function to ignore it.
  -- *  \return The SDL_AssertionHandler that is called when an assert triggers.
  --  

   function SDL_GetAssertionHandler (puserdata : System.Address) return SDL_AssertionHandler;  -- ..\SDL2_tmp\SDL_assert.h:242
   pragma Import (C, SDL_GetAssertionHandler, "SDL_GetAssertionHandler");

  --*
  -- *  \brief Get a list of all assertion failures.
  -- *
  -- *  Get all assertions triggered since last call to SDL_ResetAssertionReport(),
  -- *  or the start of the program.
  -- *
  -- *  The proper way to examine this data looks something like this:
  -- *
  -- *  <code>
  -- *  const SDL_AssertData *item = SDL_GetAssertionReport();
  -- *  while (item) {
  -- *      printf("'%s', %s (%s:%d), triggered %u times, always ignore: %s.\\n",
  -- *             item->condition, item->function, item->filename,
  -- *             item->linenum, item->trigger_count,
  -- *             item->always_ignore ? "yes" : "no");
  -- *      item = item->next;
  -- *  }
  -- *  </code>
  -- *
  -- *  \return List of all assertions.
  -- *  \sa SDL_ResetAssertionReport
  --  

   function SDL_GetAssertionReport return access constant SDL_AssertData;  -- ..\SDL2_tmp\SDL_assert.h:266
   pragma Import (C, SDL_GetAssertionReport, "SDL_GetAssertionReport");

  --*
  -- *  \brief Reset the list of all assertion failures.
  -- *
  -- *  Reset list of all assertions triggered.
  -- *
  -- *  \sa SDL_GetAssertionReport
  --  

   procedure SDL_ResetAssertionReport;  -- ..\SDL2_tmp\SDL_assert.h:275
   pragma Import (C, SDL_ResetAssertionReport, "SDL_ResetAssertionReport");

  -- these had wrong naming conventions until 2.0.4. Please update your app!  
  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_assert_h;
